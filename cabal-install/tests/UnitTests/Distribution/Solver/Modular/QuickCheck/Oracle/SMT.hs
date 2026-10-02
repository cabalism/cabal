-- | A second reference resolver for the solver QuickCheck tests, which hands
-- the search to an SMT solver.
--
-- The definition of a resolution and every rule about packages are those of
-- "UnitTests.Distribution.Solver.Modular.QuickCheck.Oracle". Only the search
-- differs: where that module searches depth first until its fuel runs out,
-- this one writes the definition out as a formula and asks Z3 for a model.
--
-- The formula is over the qualified names that some resolution could contain,
-- found by following the dependencies of every choice from the targets. Each
-- qualified name @q@ has
--
--   * an integer @i@ for its instance: 0 when @q@ is not in the resolution,
--     otherwise the position of the instance among those of its package name.
--     There is one such variable per qualified name, which is version
--     uniqueness per scope;
--   * a boolean @f@ for each flag and @s@ for each stanza of its package; and
--   * an integer rank @r@.
--
-- Following the calculus's Features extension, the choices at a qualified name
-- are enumerated: one per instance and assignment of its flags and stanzas,
-- each with a boolean @g@ defined to hold when it is the choice made. The
-- assertions are then
--
--   * root inclusion: the instance of every target is not 0;
--   * an instance that may not be chosen at @q@ is not its instance, and a
--     flag or stanza that a constraint fixes has that value;
--   * dependency closure: a choice implies, for each of its dependencies, that
--     the qualified name depended on has one of the instances that satisfy it,
--     and a choice that needs something the environment lacks is ruled out;
--   * acyclicity: a choice implies that everything it depends on has a lower
--     rank; and
--   * the single instance restriction: every instance of a package that occurs
--     in more than one scope has link variables @lf@, @ls@ and @ld@ for its
--     flags, its stanzas and what each of its dependencies resolves to, and
--     choosing the instance at any qualified name implies agreeing with them.
--
-- Stanzas that no constraint requires are left for Z3 to choose, where the
-- depth-first search enables only those that a constraint requires in some
-- scope.
--
-- Every assertion is named and comes with a description of the rule it states
-- for the packages it is about. When there is no resolution, Z3 is asked for
-- a minimal set of assertions that cannot hold together, and their
-- descriptions say why.
module UnitTests.Distribution.Solver.Modular.QuickCheck.Oracle.SMT
  ( findZ3
  , resolve
  , showConflict

    -- * Encoding
  , Encoding (..)
  , encode

    -- * Tests
  , tests
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Directory (findExecutable)
import System.Process (readProcessWithExitCode)

import Distribution.Simple.Utils (ordNub)
import Distribution.Solver.Types.OptionalStanza (OptionalStanza (..))
import Distribution.Version (isAnyVersion)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase, (@?=))

import UnitTests.Distribution.Solver.Modular.DSL
import UnitTests.Distribution.Solver.Modular.QuickCheck.Oracle hiding (resolve, tests)

{-------------------------------------------------------------------------------
  Terms
-------------------------------------------------------------------------------}

-- | An SMT-LIB term.
data Term = Atom String | App String [Term]
  deriving (Eq)

render :: Term -> ShowS
render (Atom a) = showString a
render (App f xs) =
  showChar '(' . showString f . foldr (\x k -> showChar ' ' . render x . k) id xs . showChar ')'

true, false :: Term
true = Atom "true"
false = Atom "false"

int :: Int -> Term
int = Atom . show

conj :: [Term] -> Term
conj xs
  | false `elem` xs = false
  | otherwise = case filter (/= true) xs of
      [] -> true
      [x] -> x
      xs' -> App "and" xs'

disj :: [Term] -> Term
disj xs
  | true `elem` xs = true
  | otherwise = case filter (/= false) xs of
      [] -> false
      [x] -> x
      xs' -> App "or" xs'

neg :: Term -> Term
neg x = App "not" [x]

implies :: Term -> Term -> Term
implies a b
  | b == true = true
  | b == false = neg a
  | otherwise = App "=>" [a, b]

equals :: Term -> Term -> Term
equals a b = App "=" [a, b]

-- | A boolean variable having the given value.
is :: Term -> Bool -> Term
is v b = if b then v else neg v

{-------------------------------------------------------------------------------
  Descriptions

  Qualified names, flags and stanzas are written the way the solver writes
  them in its log.
-------------------------------------------------------------------------------}

showScope :: Scope -> String
showScope (ns, q) = namespace ++ qualifier
  where
    namespace = case ns of
      DefaultNamespace -> ""
      Independent t -> t ++ "."
    qualifier = case q of
      Toplevel -> ""
      Setup p -> p ++ ":setup."
      Exe p e -> p ++ ":" ++ e ++ ":exe."

showQName :: QName -> String
showQName (s, n) = showScope s ++ n

showInstance :: Instance -> String
showInstance (Source a) = exAvName a ++ "-" ++ show (exAvVersion a)
showInstance (Installed i) =
  exInstName i ++ "-" ++ show (exInstVersion i) ++ "/installed-" ++ exInstHash i

-- | An instance at a qualified name.
showAt :: QName -> Instance -> String
showAt (s, _) inst = showScope s ++ showInstance inst

showFlag :: ExampleFlagName -> Bool -> String
showFlag f b = (if b then '+' else '-') : f

showStanza :: OptionalStanza -> String
showStanza TestStanzas = "*test"
showStanza BenchStanzas = "*bench"

-- | A choice at a qualified name.
showChoice :: QName -> Choice -> String
showChoice q ch =
  unwords $
    showAt q (chInstance ch)
      : [showFlag f b | (f, b) <- Map.toList (chFlags ch)]
      ++ map showStanza (chStanzas ch)

showGoal :: Goal -> String
showGoal g = case g of
  Target t -> showQName t
  LibDep t lib vr -> showQName t ++ maybe "" (':' :) lib ++ range vr
  ExeDep t exe vr -> "the executable " ++ exe ++ " of " ++ showQName t ++ range vr
  UnitDep _ h -> "the installed unit " ++ h
  where
    range vr = if isAnyVersion vr then "" else " " ++ prettyShow vr

-- | Why an instance may not be chosen.
showProblem :: Problem -> String
showProblem p = case p of
  ConstraintViolated _ c -> "it violates " ++ c
  NonReinstallableSource _ -> "it is non-reinstallable and not installed"
  CannotReinstall _ -> "that version is installed and reinstalls are avoided"
  Shadowed _ -> "another installed unit of that version shadows it"
  _ -> show p

-- | Say why there is no resolution, given the descriptions of assertions that
-- cannot hold together.
showConflict :: [String] -> String
showConflict [] = ""
showConflict conflict = unlines ("there is no resolution because, together:" : map ("  " ++) conflict)

{-------------------------------------------------------------------------------
  Encoding
-------------------------------------------------------------------------------}

-- | Which of a choice's dependencies a requirement comes from. Linked copies
-- of an instance have the same dependencies, and this names the dependency
-- that corresponds between them: on a package in the copy's own scope, in
-- its setup scope, or in its build-tool scope for that package.
data Slot
  = SlotRegular ExamplePkgName
  | SlotSetup ExamplePkgName
  | SlotExe ExamplePkgName
  deriving (Eq, Ord, Show)

-- | A dependency of a choice, qualified: the goal and the qualified name that
-- has to satisfy it.
data Requirement = Requirement
  { reqSlot :: Slot
  , reqTarget :: QName
  , reqGoal :: Goal
  }

-- | A choice at a qualified name: the number of its instance, and its
-- requirements, or the reason why the choice can never be made.
type Candidate = (Int, Choice, Either String [Requirement])

data Encoding = Encoding
  { encScript :: String
  -- ^ An SMT-LIB script that checks satisfiability and asks for the values
  -- of the variables that describe the resolution and for the names of the
  -- assertions in an unsatisfiable core. Only one of the two is available.
  , encDecode :: Map String String -> Resolution
  -- ^ The resolution that those values describe, without the qualified names
  -- that no target reaches.
  , encExplain :: [String] -> [String]
  -- ^ The descriptions of the assertions with those names, in the order they
  -- were asserted.
  }

-- | Write the question of whether a resolution exists as an SMT-LIB script.
-- The arguments are those of 'Oracle.resolve' without the fuel.
encode :: Env -> Bool -> [ExConstraint] -> ExampleDb -> [ExamplePkgName] -> Encoding
encode env indep cs db targets =
  Encoding
    { encScript =
        unlines $
          [ "(set-option :produce-unsat-cores true)"
          , "(set-option :smt.core.minimize true)"
          , "(set-logic QF_LIA)"
          ]
            ++ ["; " ++ render (instVar q) "" ++ ": " ++ showQName q | (q, _) <- universe]
            ++ ["(declare-const " ++ render v (" " ++ ty ++ ")") | (v, ty) <- declarations]
            ++ ["(define-fun " ++ render v (" () Bool " ++ render t ")") | (v, t) <- definitions]
            ++ concat
              [ ["; " ++ description, "(assert (! " ++ render t (" :named " ++ name ++ "))")]
              | (name, (description, t)) <- assertions
              ]
            ++ ["(check-sat)"]
            ++ ["(get-value (" ++ unwords [render v "" | v <- modelVars] ++ "))" | not (null modelVars)]
            ++ ["(get-unsat-core)"]
    , encDecode = decode
    , encExplain = \names ->
        [description | (name, (description, _)) <- assertions, name `elem` names]
    }
  where
    instances :: Map ExamplePkgName [Instance]
    instances =
      Map.fromListWith
        (flip (++))
        [(instName i, [i]) | i <- map (either Installed Source) db]

    -- The instances of a package name, numbered from 1 in database order.
    -- The number stands for the instance wherever instances are compared, so
    -- no two entries of the database may have the same 'instKey'.
    instancesOf :: ExamplePkgName -> [(Int, Instance)]
    instancesOf n = zip [1 ..] (Map.findWithDefault [] n instances)

    byHash :: Map ExamplePkgHash ExamplePkgName
    byHash = Map.fromList [(exInstHash i, exInstName i) | Left i <- db]

    roots :: [QName]
    roots = ordNub [(targetScope indep t, t) | t <- targets]

    -- Why an instance may not be chosen at a qualified name, whatever its
    -- flags and stanzas.
    unselectable :: QName -> Instance -> [String]
    unselectable qn@(_, n) inst =
      ["it is neither a target nor version-constrained" | not (explicit env cs targets n)]
        ++ map showProblem (instanceProblems env db cs qn inst)

    -- The choices that the rules about a single choice allow.
    choices :: QName -> [(Int, Choice)]
    choices qn@(_, n) =
      [ (i, Choice inst flags stanzas)
      | (i, inst) <- instancesOf n
      , null (unselectable qn inst)
      , flags <- flagAssignments inst
      , stanzas <- stanzaAssignments inst
      ]
      where
        flagAssignments (Installed _) = [Map.empty]
        flagAssignments (Source a) =
          map Map.fromList (traverse (\f -> [(f, b) | b <- allowedFlagValues cs qn a f]) (usedFlags a))
        stanzaAssignments (Installed _) = [[]]
        stanzaAssignments (Source a) =
          [ ss
          | ss <- L.subsequences (availableStanzas a)
          , all (`elem` ss) (requiredStanzas cs qn a)
          ]

    -- What a choice requires of other qualified names, unless it needs
    -- something the environment lacks or an installed unit that is not in
    -- the database.
    requirements :: QName -> Choice -> Either String [Requirement]
    requirements qn ch
      | null lacking =
          (++) <$> traverse (requirement False) regularGoals <*> traverse (requirement True) setupGoals
      | otherwise = Left ("the environment lacks " ++ intercalate ", " (map show lacking))
      where
        (regular, setup) = instanceDeps env (chStanzas ch) (chFlags ch) (chInstance ch)
        (regularGoals, setupGoals) = qualifiedGoals env qn ch
        lacking = filter (not . envSatisfied env) (regular ++ setup)

        requirement isSetup g = case g of
          LibDep t@(_, n) _ _ -> Right (Requirement (if isSetup then SlotSetup n else SlotRegular n) t g)
          ExeDep t@(_, e) _ _ -> Right (Requirement (SlotExe e) t g)
          UnitDep s h -> case Map.lookup h byHash of
            Just n -> Right (Requirement (SlotRegular n) (s, n) g)
            Nothing -> Left ("the installed unit " ++ h ++ " is not in the database")
          Target t -> error ("SMT.encode: a choice requires the target " ++ show t)

    -- The qualified names that a resolution could contain, each with its
    -- choices, in the order they are reached from the targets.
    universe :: [(QName, [Candidate])]
    universe = go Set.empty roots
      where
        go _ [] = []
        go seen (q : qs)
          | q `Set.member` seen = go seen qs
          | otherwise =
              let candidates = [(i, ch, requirements q ch) | (i, ch) <- choices q]
                  reached = [reqTarget r | (_, _, Right rs) <- candidates, r <- rs]
               in (q, candidates) : go (Set.insert q seen) (qs ++ reached)

    -- Package names that occur in more than one scope. Only their instances
    -- can be subject to the single instance restriction.
    shared :: ExamplePkgName -> Bool
    shared n = length [() | ((_, n'), _) <- universe, n' == n] > 1

    {- Variables -}

    index :: Ord a => [a] -> a -> String
    index xs = \x -> maybe (error "SMT.encode: unknown name") show (Map.lookup x m)
      where
        m = Map.fromList (zip xs [0 :: Int ..])

    qIx :: QName -> String
    qIx = index (map fst universe)

    nIx :: ExamplePkgName -> String
    nIx = index (ordNub (map (snd . fst) universe))

    fIx :: ExampleFlagName -> String
    fIx = index (ordNub [f | Right a <- db, f <- usedFlags a])

    sIx :: OptionalStanza -> String
    sIx TestStanzas = "t"
    sIx BenchStanzas = "b"

    instVar, rankVar :: QName -> Term
    instVar q = Atom ("i" ++ qIx q)
    rankVar q = Atom ("r" ++ qIx q)

    flagVar :: QName -> ExampleFlagName -> Term
    flagVar q f = Atom ("f" ++ qIx q ++ "_" ++ fIx f)

    stanzaVar :: QName -> OptionalStanza -> Term
    stanzaVar q s = Atom ("s" ++ qIx q ++ "_" ++ sIx s)

    -- Whether the choice made at a qualified name is its candidate of the
    -- given position.
    choiceVar :: QName -> Int -> Term
    choiceVar q k = Atom ("g" ++ qIx q ++ "_" ++ show k)

    -- What every copy of an instance of a shared package has to agree with.
    linkPrefix :: String -> ExamplePkgName -> Int -> String
    linkPrefix v n i = v ++ nIx n ++ "_" ++ show i ++ "_"

    linkFlag :: ExamplePkgName -> Int -> ExampleFlagName -> Term
    linkFlag n i f = Atom (linkPrefix "lf" n i ++ fIx f)

    linkStanza :: ExamplePkgName -> Int -> OptionalStanza -> Term
    linkStanza n i s = Atom (linkPrefix "ls" n i ++ sIx s)

    linkDep :: ExamplePkgName -> Int -> Slot -> Term
    linkDep n i slot = Atom (linkPrefix "ld" n i ++ tag)
      where
        tag = case slot of
          SlotRegular p -> "r" ++ nIx p
          SlotSetup p -> "s" ++ nIx p
          SlotExe p -> "e" ++ nIx p

    -- The flags and stanzas a qualified name has variables for.
    flagsOf :: ExamplePkgName -> [ExampleFlagName]
    flagsOf n = ordNub [f | (_, Source a) <- instancesOf n, f <- usedFlags a]

    stanzasOf :: ExamplePkgName -> [OptionalStanza]
    stanzasOf n = ordNub [s | (_, Source a) <- instancesOf n, s <- availableStanzas a]

    -- The variables whose values describe the resolution.
    modelVars :: [Term]
    modelVars =
      concat
        [ instVar q : map (flagVar q) (flagsOf n) ++ map (stanzaVar q) (stanzasOf n)
        | (q@(_, n), _) <- universe
        ]

    declarations :: [(Term, String)]
    declarations =
      concat
        [ (instVar q, "Int")
          : (rankVar q, "Int")
          : [(flagVar q f, "Bool") | f <- flagsOf n]
          ++ [(stanzaVar q s, "Bool") | s <- stanzasOf n]
        | (q@(_, n), _) <- universe
        ]
        ++ concat
          [ [(linkFlag n i f, "Bool") | f <- usedFlags a]
            ++ [(linkStanza n i s, "Bool") | s <- availableStanzas a]
          | n <- sharedNames
          , (i, Source a) <- instancesOf n
          ]
        ++ [ (linkDep n i slot, "Int")
           | (n, i, slot) <-
              ordNub
                [ (n, i, reqSlot r)
                | ((_, n), candidates) <- universe
                , shared n
                , (i, _, Right rs) <- candidates
                , r <- rs
                ]
           ]
      where
        sharedNames = filter shared (ordNub (map (snd . fst) universe))

    hasInstance :: QName -> Int -> Term
    hasInstance q i = equals (instVar q) (int i)

    definitions :: [(Term, Term)]
    definitions =
      [ (choiceVar q k, conj (hasInstance q i : flagValues ++ stanzaValues))
      | (q, candidates) <- universe
      , (k, (i, ch, _)) <- zip [0 ..] candidates
      , let flagValues = [flagVar q f `is` b | (f, b) <- Map.toList (chFlags ch)]
            stanzaValues = case chInstance ch of
              Installed _ -> []
              Source a -> [stanzaVar q s `is` (s `elem` chStanzas ch) | s <- availableStanzas a]
      ]

    {- Assertions -}

    -- The assertions, each with its name and a description.
    assertions :: [(String, (String, Term))]
    assertions =
      zip
        ["a" ++ show k | k <- [0 :: Int ..]]
        ( filter
            ((/= true) . snd)
            ( [(showQName q ++ " is a target", App ">=" [instVar q, int 1]) | q <- roots]
                ++ concatMap (uncurry nameAssertions) universe
            )
        )

    nameAssertions :: QName -> [Candidate] -> [(String, Term)]
    nameAssertions q@(_, n) candidates =
      ( showQName q ++ case instancesOf n of
          [] -> " is not in the database"
          insts -> " can only be " ++ intercalate " or " (map (showAt q . snd) insts)
      , App "<=" [int 0, instVar q, int (length (instancesOf n))]
      )
        : concatMap (uncurry instanceAssertions) (instancesOf n)
        ++ concatMap candidateAssertions (zip [0 ..] candidates)
      where
        instanceAssertions i inst = case (unselectable q inst, inst) of
          (reasons@(_ : _), _) ->
            [(showAt q inst ++ " may not be chosen: " ++ intercalate ", " reasons, neg (hasInstance q i))]
          ([], Installed _) -> []
          ([], Source a) ->
            [ case allowedFlagValues cs q a f of
              [] ->
                ( showAt q inst ++ " may not be chosen: constraints leave no value for the flag " ++ f
                , neg (hasInstance q i)
                )
              [b] ->
                ( showAt q inst ++ " must have " ++ showFlag f b
                , hasInstance q i `implies` (flagVar q f `is` b)
                )
              _ -> ("", true)
            | f <- usedFlags a
            ]
              ++ [ ( showAt q inst ++ " must have " ++ showStanza s
                   , hasInstance q i `implies` stanzaVar q s
                   )
                 | s <- requiredStanzas cs q a
                 ]
              ++ [ ( showAt q inst ++ " has the flags and stanzas of its copies in other scopes"
                   , hasInstance q i
                      `implies` conj
                        ( [flagVar q f `equals` linkFlag n i f | f <- usedFlags a]
                            ++ [stanzaVar q s `equals` linkStanza n i s | s <- availableStanzas a]
                        )
                   )
                 | shared n
                 ]

        candidateAssertions (k, (i, ch, requires)) = case requires of
          Left reason -> [(this ++ " may not be chosen: " ++ reason, neg chosen)]
          Right rs -> concatMap requirementAssertions rs
          where
            this = showChoice q ch
            chosen = choiceVar q k

            requirementAssertions r =
              [
                ( this ++ " depends on " ++ showGoal (reqGoal r) ++ case satisfying of
                    [] -> ", which nothing satisfies"
                    _ -> ", which " ++ intercalate " or " (map (showAt t . snd) satisfying) ++ " satisfies"
                , chosen `implies` disj [hasInstance t j | (j, _) <- satisfying]
                )
              , if t == q
                  then (this ++ " depends on itself", neg chosen)
                  else
                    ( this ++ " comes after " ++ showQName t ++ " in dependency order"
                    , chosen `implies` App "<" [rankVar t, rankVar q]
                    )
              ]
                ++ [ ( this ++ " resolves " ++ showQName t ++ " as its copies in other scopes do"
                     , chosen `implies` (instVar t `equals` linkDep n i (reqSlot r))
                     )
                   | shared n
                   ]
              where
                t = reqTarget r
                satisfying =
                  [ (j, inst)
                  | (j, inst) <- instancesOf (snd t)
                  , satisfies cs (reqGoal r) (Choice inst Map.empty [])
                  ]

    {- Models -}

    decode :: Map String String -> Resolution
    decode model = reach Map.empty roots
      where
        chosen :: Map QName Choice
        chosen = Map.fromList [(q, ch) | (q, _) <- universe, Just ch <- [choice q]]

        choice q@(_, n) = do
          i <- readMaybe =<< value (instVar q)
          inst <- lookup i (instancesOf n)
          pure $ case inst of
            Installed _ -> Choice inst Map.empty []
            Source a ->
              Choice
                inst
                (Map.fromList [(f, holds (flagVar q f)) | f <- usedFlags a])
                [s | s <- availableStanzas a, holds (stanzaVar q s)]

        value v = Map.lookup (render v "") model
        holds v = value v == Just "true"

        reach res [] = res
        reach res (q : qs) = case Map.lookup q chosen of
          Just ch
            | q `Map.notMember` res ->
                reach (Map.insert q ch res) (either (const []) (map reqTarget) (requirements q ch) ++ qs)
          _ -> reach res qs

{-------------------------------------------------------------------------------
  Running Z3
-------------------------------------------------------------------------------}

findZ3 :: IO (Maybe FilePath)
findZ3 = findExecutable "z3"

-- | Search for a resolution by asking Z3, given the path to its executable.
-- The other arguments are those of 'Oracle.resolve' without the fuel. The
-- result is 'OutOfFuel' when Z3 gives no answer within the time limit.
--
-- When there is no resolution, the second component describes a minimal set
-- of rules that cannot hold together; see 'showConflict'. Otherwise it is
-- empty.
resolve :: FilePath -> Env -> Bool -> [ExConstraint] -> ExampleDb -> [ExamplePkgName] -> IO (OracleResult, [String])
resolve z3 env indep cs db targets = do
  (_, out, err) <- readProcessWithExitCode z3 ["-in", "-smt2", "-T:" ++ show timeLimit] (encScript encoding)
  case lines out of
    "sat" : answers -> pure (Solvable (encDecode encoding (Map.fromList (pairs (atoms answers)))), [])
    "unsat" : answers -> pure (Unsolvable, encExplain encoding (atoms answers))
    "unknown" : _ -> pure (OutOfFuel, [])
    "timeout" : _ -> pure (OutOfFuel, [])
    _ -> fail ("SMT.resolve: unexpected answer from z3:\n" ++ out ++ err)
  where
    encoding = encode env indep cs db targets

    -- In seconds.
    timeLimit :: Int
    timeLimit = 30

    -- The script asks for a model and for an unsatisfiable core. Z3 answers
    -- the one that is available, with a list of pairs of a variable and a
    -- value or with a list of names, and reports an error for the other.
    atoms =
      words
        . map (\c -> if c `elem` "()" then ' ' else c)
        . unlines
        . filter (not . ("(error" `isPrefixOf`))

    pairs (k : v : rest) = (k, v) : pairs rest
    pairs _ = []

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: FilePath -> [TestTree]
tests z3 =
  [ -- The hand-written cases of the reference oracle: Z3 must give the same
    -- verdict, a resolution it finds must pass the validity check, and when
    -- there is none it must say why.
    testGroup
      "resolve"
      [ testCase (scName c) $ do
        (result, conflict) <- run c
        assertEqual (showConflict conflict) (scVerdict c) (verdict result)
        case result of
          Solvable res ->
            checkResolution (scEnv c) (scConstraints c) (scIndependent c) (scDb c) (scTargets c) (toResolved (scEnv c) res) @?= []
          Unsolvable -> assertBool "no reason given" (not (null conflict))
          OutOfFuel -> assertFailure "no answer from z3"
      | c <- solverCases
      ]
  , -- Cases whose minimal conflict is unique.
    testGroup
      "conflict"
      [ conflictCase
          "fixed version that does not exist"
          [ "A is a target"
          , "A can only be A-1"
          , "A-1 depends on B ==2, which nothing satisfies"
          ]
      , conflictCase
          "simple cycle"
          [ "A is a target"
          , "A can only be A-1"
          , "A-1 depends on B, which B-1 satisfies"
          , "A-1 comes after B in dependency order"
          , "B-1 comes after A in dependency order"
          ]
      , conflictCase
          "linked setup dependency cannot have a conflicting flag"
          [ "A is a target"
          , "A can only be A-1"
          , "A-1 depends on B ==1, which B-1 satisfies"
          , "A-1 depends on A:setup.B ==1, which A:setup.B-1 satisfies"
          , "B-1 must have +flag"
          , "B-1 has the flags and stanzas of its copies in other scopes"
          , "A:setup.B-1 must have -flag"
          , "A:setup.B-1 has the flags and stanzas of its copies in other scopes"
          ]
      , conflictCase
          "source base is not selectable"
          [ "base is a target"
          , "base can only be base-1"
          , "base-1 may not be chosen: it is non-reinstallable and not installed"
          ]
      ]
  ]
  where
    run c = resolve z3 (scEnv c) (scIndependent c) (scConstraints c) (scDb c) (scTargets c)

    conflictCase name expected = testCase name $
      case filter ((== name) . scName) solverCases of
        [c] -> do
          (_, conflict) <- run c
          conflict @?= expected
        _ -> assertFailure "no such case"
