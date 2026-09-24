-- | A reference resolver for the solver QuickCheck tests.
--
-- This module is written from the definition of a /resolution/ in the Package
-- Calculus ("Package Managers à la Carte", Gibb et al., ICFP 2026): a set of
-- packages that satisfies
--
--   * root inclusion: every target is in the set,
--   * dependency closure: every dependency of every member is satisfied by a
--     member, and
--   * version uniqueness: at most one instance per package name.
--
-- Following the calculus's Features extension, a flag assignment is folded into
-- the notion of an instance: each (version, flag assignment) pair is a distinct
-- core version whose dependencies are the flag-evaluated dependencies.
--
-- The resolver here is a plain depth-first search with no heuristics, so it is
-- easy to trust and can be compared against the modular solver in both
-- directions: if the solver finds a plan, the oracle must find one too (and the
-- solver's plan must pass 'checkResolution'), and if the solver reports that
-- there is no plan, the oracle must agree.
--
-- Phase 1 scope: only the top-level qualifier is modelled. Test databases must
-- not contain setup dependencies (see 'withoutSetupDeps') and the solver must
-- run without independent goals, so that version uniqueness is global.
module UnitTests.Distribution.Solver.Modular.QuickCheck.Oracle
  ( -- * Reference resolver
    Instance (..)
  , Choice (..)
  , Resolution
  , Goal (..)
  , OracleResult (..)
  , Verdict (..)
  , verdict
  , resolve

    -- * Validity check
  , ResolvedPackage (..)
  , Problem (..)
  , checkResolution
  , fromSolverPlan
  , toResolved

    -- * Helpers
  , withoutSetupDeps
  , tests
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import qualified Data.List as L
import qualified Data.Map as Map

import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.Package (PackageIdentifier (..), mkPackageName, packageId, unPackageName)
import Distribution.Simple.Utils (ordNub)
import Distribution.Types.Flag (unFlagAssignment, unFlagName)
import Distribution.Types.UnitId (unUnitId)
import Distribution.Version
  ( VersionRange
  , anyVersion
  , notThisVersion
  , thisVersion
  , versionNumbers
  , withinRange
  )

import Distribution.Client.SolverInstallPlan (ResolverPackage (..), SolverInstallPlan)
import qualified Distribution.Client.SolverInstallPlan as SolverInstallPlan
import Distribution.Solver.Types.ComponentDeps (Component (..))
import qualified Distribution.Solver.Types.ComponentDeps as CD
import Distribution.Solver.Types.InstSolverPackage (InstSolverPackage (..))
import Distribution.Solver.Types.OptionalStanza (OptionalStanza (..), optStanzaSetToList)
import Distribution.Solver.Types.PackageConstraint (ConstraintScope (..), scopeToPackageName)
import Distribution.Solver.Types.SolverPackage (SolverPackage (..))

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import UnitTests.Distribution.Solver.Modular.DSL

{-------------------------------------------------------------------------------
  Instances, choices and goals
-------------------------------------------------------------------------------}

-- | One selectable instance of a package name: an installed unit or a source
-- version. The solver picks at most one instance per name.
data Instance
  = Installed ExampleInstalled
  | Source ExampleAvailable
  deriving (Show)

instName :: Instance -> ExamplePkgName
instName (Installed i) = exInstName i
instName (Source a) = exAvName a

instVersion :: Instance -> ExamplePkgVersion
instVersion (Installed i) = exInstVersion i
instVersion (Source a) = exAvVersion a

instHash :: Instance -> Maybe ExamplePkgHash
instHash (Installed i) = Just (exInstHash i)
instHash (Source _) = Nothing

type Flags = Map ExampleFlagName Bool

-- | An instance together with the flag and stanza assignment it was chosen
-- with. This is a "core version" in the sense of the calculus.
data Choice = Choice
  { chInstance :: Instance
  , chFlags :: Flags
  , chStanzas :: [OptionalStanza]
  }
  deriving (Show)

-- | Version uniqueness holds by construction: the map is keyed by name.
type Resolution = Map ExamplePkgName Choice

-- | Something that must be satisfied by the resolution.
data Goal
  = -- | A build target. Any instance of the name will do.
    Target ExamplePkgName
  | -- | A @build-depends@ on the main library of a package.
    LibDep ExamplePkgName VersionRange
  | -- | An installed package's dependency on an exact installed unit.
    UnitDep ExamplePkgHash
  deriving (Eq, Show)

data OracleResult
  = Solvable Resolution
  | Unsolvable
  | -- | The search ran out of fuel before finishing.
    OutOfFuel
  deriving (Show)

data Verdict = IsSolvable | IsUnsolvable | IsUnknown
  deriving (Eq, Show)

verdict :: OracleResult -> Verdict
verdict (Solvable _) = IsSolvable
verdict Unsolvable = IsUnsolvable
verdict OutOfFuel = IsUnknown

{-------------------------------------------------------------------------------
  Package semantics

  Each function below mirrors one rule of the modular solver. The rule is
  named in the comment so that a failing property points at the rule in
  question.
-------------------------------------------------------------------------------}

-- | Flags that are used but not declared are automatic with default True
-- (DSL 'mkDefaultFlag'). The solver may pick either value; the default only
-- matters as a fallback when a plan does not mention the flag.
lookupFlag :: Flags -> ExampleFlagName -> Bool
lookupFlag flags f = Map.findWithDefault True f flags

-- | The flags mentioned anywhere in a package's dependencies. These are
-- exactly the flags the DSL declares for the package.
usedFlags :: ExampleAvailable -> [ExampleFlagName]
usedFlags a = ordNub (concatMap (goDeps . snd) (CD.toList (exAvDeps a)))
  where
    goDeps = concatMap go . depsExampleDependencies
    go (ExFlagged f t e) = f : goDeps t ++ goDeps e
    go _ = []

-- | The dependencies of a component under a flag assignment, or 'Nothing' when
-- the component is not buildable under that assignment.
--
-- Mirrors 'addBuildableCondition' in index conversion: a component contributes
-- its dependencies only when every node reached under the assignment has
-- @buildable: True@.
componentDeps :: Flags -> Dependencies -> Maybe [Goal]
componentDeps flags deps
  | not (depsIsBuildable deps) = Nothing
  | otherwise = concat <$> traverse go (depsExampleDependencies deps)
  where
    go (ExAny p) = Just [LibDep p anyVersion]
    go (ExFix p v) = Just [LibDep p (thisVersion (mkSimpleVersion v))]
    go (ExRange p lo hi) = Just [LibDep p (mkVersionRange lo hi)]
    go (ExFlagged f t e) = componentDeps flags (if lookupFlag flags f then t else e)
    go dep = error ("Oracle.componentDeps: unsupported dependency " ++ show dep)

-- | Whether a component is buildable, decided before solving.
--
-- Mirrors 'extractCondition' and 'testConditionForComponent': 'Just False'
-- when the component is unbuildable however the free flags are assigned,
-- 'Just True' when it is always buildable, and 'Nothing' when it depends on a
-- flag that no constraint fixes. The solver treats 'Nothing' as buildable.
staticBuildable :: Flags -> Dependencies -> Maybe Bool
staticBuildable fixed deps
  | not (depsIsBuildable deps) = Just False
  | otherwise = conj (map branch (depsExampleDependencies deps))
  where
    branch (ExFlagged f t e) = case Map.lookup f fixed of
      Just b -> staticBuildable fixed (if b then t else e)
      Nothing -> case (staticBuildable fixed t, staticBuildable fixed e) of
        (Just False, Just False) -> Just False
        (Just True, Just True) -> Just True
        _ -> Nothing
    branch _ = Just True

    conj bs
      | Just False `elem` bs = Just False
      | all (== Just True) bs = Just True
      | otherwise = Nothing

mainLibrary :: ExampleAvailable -> Maybe Dependencies
mainLibrary a = lookup ComponentLib (CD.toList (exAvDeps a))

-- | Can this instance be the target of a @build-depends@?
--
-- Installed packages always can. A source package needs a main library that
-- is not statically unbuildable (validation's
-- @PackageRequiresMissingComponent@ and @PackageRequiresUnbuildableComponent@).
providesLibrary :: [ExConstraint] -> Instance -> Bool
providesLibrary _ (Installed _) = True
providesLibrary cs (Source a) = case mainLibrary a of
  Nothing -> False
  Just lib -> staticBuildable (fixedFlags cs (exAvName a)) lib /= Just False

-- | The goals introduced by choosing an instance.
--
-- An installed package depends on exact installed units. A source package's
-- library, sub-library, foreign-library and executable components are always
-- solved; test and benchmark components only when their stanza is enabled;
-- setup components are out of scope for phase 1.
instanceGoals :: [OptionalStanza] -> Flags -> Instance -> [Goal]
instanceGoals _ _ (Installed i) = map UnitDep (exInstBuildAgainst i)
instanceGoals stanzas flags (Source a) =
  concat
    [ fromMaybe [] (componentDeps flags deps)
    | (comp, deps) <- CD.toList (exAvDeps a)
    , solved comp
    ]
  where
    solved ComponentLib = True
    solved (ComponentSubLib _) = True
    solved (ComponentFLib _) = True
    solved (ComponentExe _) = True
    solved (ComponentTest _) = TestStanzas `elem` stanzas
    solved (ComponentBench _) = BenchStanzas `elem` stanzas
    solved ComponentSetup = False

choiceGoals :: Choice -> [Goal]
choiceGoals ch = instanceGoals (chStanzas ch) (chFlags ch) (chInstance ch)

-- | Does a choice satisfy a goal?
satisfies :: [ExConstraint] -> Goal -> Choice -> Bool
satisfies _ (Target n) ch = instName (chInstance ch) == n
satisfies cs (LibDep n vr) ch =
  instName inst == n
    && withinRange (mkSimpleVersion (instVersion inst)) vr
    && providesLibrary cs inst
  where
    inst = chInstance ch
satisfies _ (UnitDep h) ch = instHash (chInstance ch) == Just h

{-------------------------------------------------------------------------------
  Constraints
-------------------------------------------------------------------------------}

-- | The package a constraint applies to. With only the top-level qualifier in
-- play, every scope except the setup-only one applies to the package it names.
constraintTarget :: ConstraintScope -> Maybe ExamplePkgName
constraintTarget (ScopeAnySetupQualifier _) = Nothing
constraintTarget scope = Just (unPackageName (scopeToPackageName scope))

versionConstraints :: [ExConstraint] -> ExamplePkgName -> [(ExConstraint, VersionRange)]
versionConstraints cs n =
  [ (c, vr)
  | c@(ExVersionConstraint scope vr) <- cs
  , constraintTarget scope == Just n
  ]

-- | The stanzas a source package has a choice about: a stanza exists only
-- when the package has at least one component of that kind.
availableStanzas :: ExampleAvailable -> [OptionalStanza]
availableStanzas a =
  ordNub
    [ s
    | (comp, _) <- CD.toList (exAvDeps a)
    , s <- case comp of
        ComponentTest _ -> [TestStanzas]
        ComponentBench _ -> [BenchStanzas]
        _ -> []
    ]

-- | The stanzas a constraint forces on for a source package. A constraint on
-- a stanza the package does not have never fires.
requiredStanzas :: [ExConstraint] -> ExampleAvailable -> [OptionalStanza]
requiredStanzas cs a =
  ordNub
    [ s
    | ExStanzaConstraint scope ss <- cs
    , constraintTarget scope == Just (exAvName a)
    , s <- ss
    , s `elem` availableStanzas a
    ]

fixedFlags :: [ExConstraint] -> ExamplePkgName -> Flags
fixedFlags cs n =
  Map.fromList
    [ (f, b)
    | ExFlagConstraint scope f b <- cs
    , constraintTarget scope == Just n
    ]

-- | Packages that can never be installed from source when the compiler's
-- wired-in units are unknown and boot library installs are not allowed
-- ('Distribution.Client.Dependency.nonReinstallablePackages').
nonReinstallable :: ExamplePkgName -> Bool
nonReinstallable n =
  n
    `elem` [ "base"
           , "ghc-bignum"
           , "ghc-internal"
           , "ghc-prim"
           , "ghc"
           , "integer-gmp"
           , "integer-simple"
           , "template-haskell"
           ]

-- | Problems with choosing an instance at all, independent of the rest of the
-- resolution. Version constraints apply to installed and source instances
-- alike, and only to packages that are actually chosen.
instanceProblems :: [ExConstraint] -> Instance -> [Problem]
instanceProblems cs inst =
  [NonReinstallableSource n | Source _ <- [inst], nonReinstallable n]
    ++ [ ConstraintViolated n (show c)
       | (c, vr) <- versionConstraints cs n
       , not (withinRange (mkSimpleVersion (instVersion inst)) vr)
       ]
  where
    n = instName inst

-- | Problems with a choice: those of its instance, plus stanza and flag
-- constraints that the chosen assignment does not respect. Installed
-- packages have neither stanzas nor flags, so those constraints only apply
-- to source instances.
choiceProblems :: [ExConstraint] -> Choice -> [Problem]
choiceProblems cs ch =
  instanceProblems cs inst
    ++ [ ConstraintViolated n (show c)
       | c@(ExStanzaConstraint scope ss) <- cs
       , constraintTarget scope == Just n
       , Source a <- [inst]
       , s <- ss
       , s `elem` availableStanzas a
       , s `notElem` chStanzas ch
       ]
    ++ [ ConstraintViolated n (show c)
       | c@(ExFlagConstraint scope f b) <- cs
       , constraintTarget scope == Just n
       , Source a <- [inst]
       , f `elem` usedFlags a
       , lookupFlag (chFlags ch) f /= b
       ]
  where
    inst = chInstance ch
    n = instName inst

{-------------------------------------------------------------------------------
  Reference resolver
-------------------------------------------------------------------------------}

data Search
  = Found Resolution
  | NotFound Int
  | Starved

-- | Search for a resolution by plain depth-first search.
--
-- The first argument is fuel: every goal processed costs one unit, and when
-- it runs out the result is 'OutOfFuel'. There are no heuristics: goals are
-- processed in the order they are introduced, and candidates are tried in
-- database order with flags enumerated True first.
resolve :: Int -> [ExConstraint] -> ExampleDb -> [ExamplePkgName] -> OracleResult
resolve fuel0 cs db targets =
  case go fuel0 Map.empty (map Target targets) of
    Found r -> Solvable r
    NotFound _ -> Unsolvable
    Starved -> OutOfFuel
  where
    instances :: Map ExamplePkgName [Instance]
    instances =
      Map.fromListWith
        (flip (++))
        [(instName i, [i]) | i <- map (either Installed Source) db]

    byHash :: Map ExamplePkgHash ExamplePkgName
    byHash = Map.fromList [(exInstHash i, exInstName i) | Left i <- db]

    -- The name a goal is about, or Nothing for a dependency on an installed
    -- unit that is not in the database (the solver marks the depender broken).
    goalName :: Goal -> Maybe ExamplePkgName
    goalName (Target n) = Just n
    goalName (LibDep n _) = Just n
    goalName (UnitDep h) = Map.lookup h byHash

    choices :: ExamplePkgName -> [Choice]
    choices n =
      [ Choice inst flags (stanzas inst)
      | inst <- Map.findWithDefault [] n instances
      , null (instanceProblems cs inst)
      , flags <- assignments inst
      ]
      where
        fixed = fixedFlags cs n
        stanzas (Installed _) = []
        stanzas (Source a) = requiredStanzas cs a
        assignments (Installed _) = [Map.empty]
        assignments (Source a) = map Map.fromList (traverse pick (usedFlags a))
        pick f = [(f, b) | b <- maybe [True, False] (: []) (Map.lookup f fixed)]

    go :: Int -> Resolution -> [Goal] -> Search
    go _ res [] = Found res
    go fuel res (g : gs)
      | fuel <= 0 = Starved
      | otherwise = case goalName g of
          Nothing -> NotFound fuel'
          Just n -> case Map.lookup n res of
            Just ch
              | satisfies cs g ch -> go fuel' res gs
              | otherwise -> NotFound fuel'
            Nothing -> tryEach n fuel' [ch | ch <- choices n, satisfies cs g ch]
      where
        fuel' = fuel - 1

        tryEach _ f [] = NotFound f
        tryEach n f (ch : chs) =
          case go f (Map.insert n ch res) (choiceGoals ch ++ gs) of
            Found r -> Found r
            Starved -> Starved
            NotFound f' -> tryEach n f' chs

{-------------------------------------------------------------------------------
  Validity check
-------------------------------------------------------------------------------}

-- | A package in a plan, described without reference to solver types.
data ResolvedPackage = ResolvedPackage
  { rpName :: ExamplePkgName
  , rpVersion :: ExamplePkgVersion
  , rpInstalledHash :: Maybe ExamplePkgHash
  , rpFlags :: [(ExampleFlagName, Bool)]
  , rpStanzas :: [OptionalStanza]
  }
  deriving (Eq, Show)

data Problem
  = MissingTarget ExamplePkgName
  | MultipleVersions ExamplePkgName
  | UnknownInstance ExamplePkgName ExamplePkgVersion
  | MissingDependency ExamplePkgName Goal
  | ConstraintViolated ExamplePkgName String
  | NonReinstallableSource ExamplePkgName
  deriving (Eq, Show)

-- | Check that a plan is a resolution of the given database and targets.
--
-- Minimality is deliberately not checked: the solver may enable stanzas or
-- include packages that are not strictly needed, and that is still a
-- resolution.
checkResolution
  :: [ExConstraint]
  -> ExampleDb
  -> [ExamplePkgName]
  -> [ResolvedPackage]
  -> [Problem]
checkResolution cs db targets plan =
  [MissingTarget t | t <- targets, t `notElem` names]
    ++ [MultipleVersions n | (n : _ : _) <- L.group (L.sort names)]
    ++ concatMap check plan
  where
    names = map rpName plan

    choicesInPlan = mapMaybe toChoice plan

    check rp = case toChoice rp of
      Nothing -> [UnknownInstance (rpName rp) (rpVersion rp)]
      Just ch ->
        choiceProblems cs ch
          ++ [ MissingDependency (rpName rp) g
             | g <- choiceGoals ch
             , not (any (satisfies cs g) choicesInPlan)
             ]

    toChoice rp = do
      inst <- lookupInstance rp
      pure (Choice inst (Map.fromList (rpFlags rp)) (rpStanzas rp))

    lookupInstance rp = case rpInstalledHash rp of
      Just h ->
        listToMaybe
          [ Installed i
          | Left i <- db
          , exInstHash i == h
          , exInstName i == rpName rp
          , exInstVersion i == rpVersion rp
          ]
      Nothing ->
        listToMaybe
          [ Source a
          | Right a <- db
          , exAvName a == rpName rp
          , exAvVersion a == rpVersion rp
          ]

-- | Describe a solver install plan in the oracle's terms.
fromSolverPlan :: SolverInstallPlan -> [ResolvedPackage]
fromSolverPlan = map conv . SolverInstallPlan.toList
  where
    conv (PreExisting ipkg) =
      let ipi = instSolverPkgIPI ipkg
          PackageIdentifier pn v = IPI.sourcePackageId ipi
       in ResolvedPackage
            { rpName = unPackageName pn
            , rpVersion = simpleVersion v
            , rpInstalledHash = Just (unUnitId (IPI.installedUnitId ipi))
            , rpFlags = []
            , rpStanzas = []
            }
    conv (Configured spkg) =
      let PackageIdentifier pn v = packageId (solverPkgSource spkg)
       in ResolvedPackage
            { rpName = unPackageName pn
            , rpVersion = simpleVersion v
            , rpInstalledHash = Nothing
            , rpFlags = [(unFlagName f, b) | (f, b) <- unFlagAssignment (solverPkgFlags spkg)]
            , rpStanzas = optStanzaSetToList (solverPkgStanzas spkg)
            }

    simpleVersion v = case versionNumbers v of
      (n : _) -> n
      [] -> error "Oracle.fromSolverPlan: empty version"

-- | Describe an oracle resolution in the same terms, for checking the oracle
-- against itself.
toResolved :: Resolution -> [ResolvedPackage]
toResolved = map conv . Map.elems
  where
    conv ch =
      ResolvedPackage
        { rpName = instName inst
        , rpVersion = instVersion inst
        , rpInstalledHash = instHash inst
        , rpFlags = Map.toList (chFlags ch)
        , rpStanzas = chStanzas ch
        }
      where
        inst = chInstance ch

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

-- | Remove setup dependencies from every source package. A package without
-- setup dependencies has build type Simple, so this is the same as never
-- having declared a custom setup.
withoutSetupDeps :: ExampleDb -> ExampleDb
withoutSetupDeps = map (fmap strip)
  where
    strip a = a{exAvDeps = CD.filterDeps (\comp _ -> comp /= ComponentSetup) (exAvDeps a)}

{-------------------------------------------------------------------------------
  Unit tests pinning the intended semantics
-------------------------------------------------------------------------------}

tests :: [TestTree]
tests =
  [ testGroup
      "resolve"
      [ solvable "chain of dependencies" [] dbChain ["A"]
      , unsolvable "fixed version that does not exist" [] dbMissingVersion ["A"]
      , solvable "flag chooses a satisfiable branch" [] dbFlag ["A"]
      , unsolvable "flag constraint forces the unsatisfiable branch" [flagFalse "A" "F"] dbFlag ["A"]
      , solvable "unbuildable branch hides an unsatisfiable dependency" [] dbUnbuildableBranch ["A"]
      , solvable "flag-gated unbuildable library still provides a library" [] dbUnbuildableBranch ["C"]
      , unsolvable "statically unbuildable library cannot be depended on" [] dbUnbuildableLib ["C"]
      , solvable "statically unbuildable library can be a target" [] dbUnbuildableLib ["A"]
      , solvable "installed package built against installed unit" [] dbInstalled ["A"]
      , unsolvable "version constraint excludes the installed unit" [ExVersionConstraint (anyQ "B") (notThisVersion (mkSimpleVersion 1))] dbInstalled ["A"]
      , unsolvable "source base is not selectable" [] [Right (exAv "base" 1 [])] ["base"]
      , solvable "installed base is selectable" [] [Left (exInst "base" 1 "base-1" [])] ["base"]
      , unsolvable "dependency on an executable-only package" [] dbExeOnly ["B"]
      , solvable "executable-only package can be a target" [] dbExeOnly ["A"]
      , solvable "stanza constraint enables test dependencies" [ExStanzaConstraint (anyQ "A") [TestStanzas]] dbTestStanza ["A"]
      , unsolvable "stanza constraint makes test dependencies required" [ExStanzaConstraint (anyQ "A") [TestStanzas]] dbBadTestStanza ["A"]
      , solvable "test dependencies are optional without a constraint" [] dbBadTestStanza ["A"]
      , solvable "stanza constraint does not apply to an installed package" [ExStanzaConstraint (anyQ "A") [TestStanzas]] dbInstalled ["A"]
      , solvable "stanza constraint on a package without tests never fires" [ExStanzaConstraint (anyQ "A") [TestStanzas]] dbChain ["A"]
      , testCase "no fuel gives no verdict" $
          verdict (resolve 0 [] dbChain ["A"]) @?= IsUnknown
      ]
  , testGroup
      "checkResolution"
      [ testCase "accepts the oracle's own resolutions" $
          for_ solvableCases $ \(cs, db, targets) ->
            case resolve fuel cs db targets of
              Solvable res -> checkResolution cs db targets (toResolved res) @?= []
              other -> assertBool ("expected a resolution, got " ++ show other) False
      , testCase "rejects a missing target" $
          checkResolution [] dbChain ["A"] [] @?= [MissingTarget "A"]
      , testCase "rejects a dropped dependency" $
          checkResolution [] dbChain ["A"] [src "A" 1 []]
            @?= [MissingDependency "A" (LibDep "B" anyVersion)]
      , testCase "rejects two versions of one package" $
          checkResolution [] dbTwoVersions ["A"] [src "A" 1 [], src "B" 1 [], src "B" 2 []]
            @?= [MultipleVersions "B"]
      , testCase "rejects a flag whose branch is unsatisfied" $
          checkResolution [] dbFlag ["A"] [src "A" 1 [("F", False)], src "B" 1 []]
            @?= [MissingDependency "A" (LibDep "B" (thisVersion (mkSimpleVersion 9)))]
      , testCase "rejects a violated version constraint" $
          let c = ExVersionConstraint (anyQ "B") (notThisVersion (mkSimpleVersion 1))
           in checkResolution [c] dbChain ["A"] [src "A" 1 [], src "B" 1 []]
                @?= [ConstraintViolated "B" (show c)]
      , testCase "rejects an instance that is not in the database" $
          checkResolution [] dbChain ["A"] [src "A" 1 [], src "B" 7 []]
            @?= [MissingDependency "A" (LibDep "B" anyVersion), UnknownInstance "B" 7]
      ]
  ]
  where
    fuel = 1000

    solvable name cs db targets =
      testCase name $ verdict (resolve fuel cs db targets) @?= IsSolvable

    unsolvable name cs db targets =
      testCase name $ verdict (resolve fuel cs db targets) @?= IsUnsolvable

    solvableCases =
      [ ([], dbChain, ["A"])
      , ([], dbFlag, ["A"])
      , ([], dbUnbuildableBranch, ["C"])
      , ([], dbInstalled, ["A"])
      , ([], dbExeOnly, ["A"])
      , ([ExStanzaConstraint (anyQ "A") [TestStanzas]], dbTestStanza, ["A"])
      ]

    anyQ = ScopeAnyQualifier . mkPackageName
    flagFalse n f = ExFlagConstraint (anyQ n) f False

    src n v flags =
      ResolvedPackage
        { rpName = n
        , rpVersion = v
        , rpInstalledHash = Nothing
        , rpFlags = flags
        , rpStanzas = []
        }

    dbChain = [Right (exAv "A" 1 [ExAny "B"]), Right (exAv "B" 1 [])]

    dbMissingVersion = [Right (exAv "A" 1 [ExFix "B" 2]), Right (exAv "B" 1 [])]

    dbTwoVersions = [Right (exAv "A" 1 [ExAny "B"]), Right (exAv "B" 1 []), Right (exAv "B" 2 [])]

    dbFlag =
      [ Right (exAv "A" 1 [exFlagged "F" [ExFix "B" 1] [ExFix "B" 9]])
      , Right (exAv "B" 1 [])
      ]

    -- A's library is unbuildable when F is True, and needs a missing B-9
    -- when F is False. Buildability depends on a free flag, so the solver
    -- treats the library as buildable and picks F = True.
    dbUnbuildableBranch =
      [ Right (exAv "A" 1 [ExFlagged "F" unbuildableDependencies (dependencies [ExFix "B" 9])])
      , Right (exAv "C" 1 [ExAny "A"])
      ]

    dbUnbuildableLib =
      [ Right (exAv "A" 1 []){exAvDeps = CD.fromLibraryDeps unbuildableDependencies}
      , Right (exAv "C" 1 [ExAny "A"])
      ]

    dbInstalled =
      let b = exInst "B" 1 "B-1-hash" []
       in [Left b, Left (exInst "A" 1 "A-1-hash" [b])]

    dbExeOnly =
      [ Right (exAvNoLibrary "A" 1 `withExe` exExe "exe" [])
      , Right (exAv "B" 1 [ExAny "A"])
      ]

    dbTestStanza =
      [ Right (exAv "A" 1 [] `withTest` exTest "test" [ExAny "B"])
      , Right (exAv "B" 1 [])
      ]

    dbBadTestStanza =
      [Right (exAv "A" 1 [] `withTest` exTest "test" [ExAny "B"])]
