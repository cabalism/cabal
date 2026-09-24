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
-- Following its Concurrent Versions extension, version uniqueness holds per
-- /scope/ rather than globally. A scope is a namespace (one per target with
-- independent goals, otherwise a single default one) and a qualifier (the top
-- level, or the setup dependencies of one package). Regular dependencies stay
-- in the scope of their depender; setup dependencies of a package @P@ live in
-- the scope @Setup P@ of the same namespace. This mirrors 'qualifyDeps' in
-- "Distribution.Solver.Modular.Dependency".
--
-- The one cross-scope rule is GHC's single instance restriction: if two scopes
-- pick the same instance of a package (same installed unit, or same source
-- version), they must be /linked/: same flags and stanzas, and their
-- dependencies must in turn resolve to the same instances. This mirrors
-- 'enforceSingleInstanceRestriction' and 'validateLinking'.
--
-- The resolver here is a plain depth-first search with no heuristics, so it is
-- easy to trust and can be compared against the modular solver in both
-- directions: if the solver finds a plan, the oracle must find one too (and the
-- solver's plan must pass 'checkResolution'), and if the solver reports that
-- there is no plan, the oracle must agree.
--
-- Not modelled: dependency cycles (the QuickCheck generator never produces
-- them), build-tool dependencies, pkg-config, language and extension
-- dependencies, and base shims.
module UnitTests.Distribution.Solver.Modular.QuickCheck.Oracle
  ( -- * Reference resolver
    Namespace (..)
  , Qualifier (..)
  , Scope
  , QName
  , Instance (..)
  , Choice (..)
  , Resolution
  , Dep (..)
  , Goal (..)
  , OracleResult (..)
  , Verdict (..)
  , verdict
  , resolve

    -- * Validity check
  , ResolvedRef (..)
  , ResolvedPackage (..)
  , Problem (..)
  , checkResolution
  , fromSolverPlan
  , toResolved

    -- * Tests
  , SolverCase (..)
  , solverCases
  , tests
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.Package (PackageIdentifier (..), mkPackageName, packageId, unPackageName)
import Distribution.Simple.Utils (ordNub)
import Distribution.Types.Flag (unFlagAssignment, unFlagName)
import Distribution.Types.LibraryVisibility (LibraryVisibility (..))
import Distribution.Types.UnitId (unUnitId)
import Distribution.Types.UnqualComponentName (mkUnqualComponentName)
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
import Distribution.Solver.Types.Flag (FlagType (..))
import Distribution.Solver.Types.InstSolverPackage (InstSolverPackage (..))
import Distribution.Solver.Types.OptionalStanza (OptionalStanza (..), optStanzaSetToList)
import Distribution.Solver.Types.PackageConstraint (ConstraintScope (..))
import qualified Distribution.Solver.Types.PackagePath as P
import Distribution.Solver.Types.SolverId (SolverId (..))
import Distribution.Solver.Types.SolverPackage (SolverPackage (..))

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import UnitTests.Distribution.Solver.Modular.DSL

{-------------------------------------------------------------------------------
  Scopes
-------------------------------------------------------------------------------}

-- | Mirrors 'Distribution.Solver.Types.PackagePath.Namespace'.
data Namespace
  = DefaultNamespace
  | Independent ExamplePkgName
  deriving (Eq, Ord, Show)

-- | Mirrors the qualifiers that can arise without build-tool dependencies or
-- base shims.
data Qualifier
  = Toplevel
  | Setup ExamplePkgName
  deriving (Eq, Ord, Show)

type Scope = (Namespace, Qualifier)

-- | A qualified package name: the solver's 'QPN'.
type QName = (Scope, ExamplePkgName)

targetScope :: Bool -> ExamplePkgName -> Scope
targetScope indep t = (if indep then Independent t else DefaultNamespace, Toplevel)

{-------------------------------------------------------------------------------
  Instances, choices and goals
-------------------------------------------------------------------------------}

-- | One selectable instance of a package name: an installed unit or a source
-- version. Each scope picks at most one instance per name.
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

-- | What the single instance restriction keys on: the solver's @PI@. An
-- installed unit and a source package of the same version are different
-- instances.
instKey :: Instance -> (ExamplePkgName, ExamplePkgVersion, Maybe ExamplePkgHash)
instKey inst = (instName inst, instVersion inst, instHash inst)

type Flags = Map ExampleFlagName Bool

-- | An instance together with the flag and stanza assignment it was chosen
-- with. This is a "core version" in the sense of the calculus.
data Choice = Choice
  { chInstance :: Instance
  , chFlags :: Flags
  , chStanzas :: [OptionalStanza]
  }
  deriving (Show)

sameChoice :: Choice -> Choice -> Bool
sameChoice a b =
  instKey (chInstance a) == instKey (chInstance b)
    && chFlags a == chFlags b
    && L.sort (chStanzas a) == L.sort (chStanzas b)

-- | Version uniqueness per scope holds by construction: the map is keyed by
-- qualified name.
type Resolution = Map QName Choice

-- | An unqualified dependency of an instance.
data Dep
  = -- | A @build-depends@ on the main library ('Nothing') or a named
    -- sub-library of a package.
    DepLib ExamplePkgName (Maybe ExampleSubLibName) VersionRange
  | -- | An installed package's dependency on an exact installed unit.
    DepUnit ExamplePkgHash
  deriving (Eq, Show)

-- | Something that must be satisfied by the resolution, in a scope.
data Goal
  = Target QName
  | LibDep QName (Maybe ExampleSubLibName) VersionRange
  | UnitDep Scope ExamplePkgHash
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
-- (DSL 'mkDefaultFlag'). The default only matters as a fallback when a plan
-- does not mention the flag.
lookupFlag :: Flags -> ExampleFlagName -> Bool
lookupFlag flags f = Map.findWithDefault True f flags

-- | The flags mentioned anywhere in a package's dependencies. These are
-- exactly the flags the solver makes a choice about.
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
componentDeps :: Flags -> Dependencies -> Maybe [Dep]
componentDeps flags deps
  | not (depsIsBuildable deps) = Nothing
  | otherwise = concat <$> traverse go (depsExampleDependencies deps)
  where
    go (ExAny p) = Just [DepLib p Nothing anyVersion]
    go (ExFix p v) = Just [DepLib p Nothing (thisVersion (mkSimpleVersion v))]
    go (ExRange p lo hi) = Just [DepLib p Nothing (mkVersionRange lo hi)]
    go (ExSubLibAny p l) = Just [DepLib p (Just l) anyVersion]
    go (ExSubLibFix p l v) = Just [DepLib p (Just l) (thisVersion (mkSimpleVersion v))]
    go (ExFlagged f t e) = componentDeps flags (if lookupFlag flags f then t else e)
    go dep = error ("Oracle.componentDeps: unsupported dependency " ++ show dep)

-- | Whether a predicate holds for a component everywhere it can be reached,
-- decided before solving.
--
-- Mirrors 'extractCondition' and 'testConditionForComponent': 'Just False'
-- when the predicate fails however the free flags are assigned, 'Just True'
-- when it always holds, and 'Nothing' when the answer depends on a flag that
-- no unqualified constraint fixes.
staticCondition :: (Dependencies -> Bool) -> Flags -> Dependencies -> Maybe Bool
staticCondition p fixed deps
  | not (p deps) = Just False
  | otherwise = conj (map branch (depsExampleDependencies deps))
  where
    branch (ExFlagged f t e) = case Map.lookup f fixed of
      Just b -> staticCondition p fixed (if b then t else e)
      Nothing -> case (staticCondition p fixed t, staticCondition p fixed e) of
        (Just False, Just False) -> Just False
        (Just True, Just True) -> Just True
        _ -> Nothing
    branch _ = Just True

    conj bs
      | Just False `elem` bs = Just False
      | all (== Just True) bs = Just True
      | otherwise = Nothing

-- | The solver treats a component as buildable unless it is statically
-- unbuildable.
staticBuildable :: Flags -> Dependencies -> Maybe Bool
staticBuildable = staticCondition depsIsBuildable

-- | The solver treats a sub-library as visible unless it is statically
-- private. The DSL always makes the main library public.
staticPrivate :: Flags -> Dependencies -> Maybe Bool
staticPrivate = staticCondition ((== LibraryVisibilityPrivate) . depsVisibility)

-- | Can this instance be the target of a @build-depends@ on its main library
-- ('Nothing') or on a named sub-library?
--
-- Installed packages provide only their main library (index conversion does
-- not yet handle installed sub-libraries). A source package needs the
-- component to exist and to be neither statically unbuildable nor, for a
-- sub-library, statically private (validation's
-- @PackageRequiresMissingComponent@, @PackageRequiresUnbuildableComponent@
-- and @PackageRequiresPrivateComponent@).
providesLibrary :: [ExConstraint] -> Maybe ExampleSubLibName -> Instance -> Bool
providesLibrary _ lib (Installed _) = isNothing lib
providesLibrary cs lib (Source a) =
  case lookup comp (CD.toList (exAvDeps a)) of
    Nothing -> False
    Just deps ->
      staticBuildable fixed deps /= Just False
        && (isNothing lib || staticPrivate fixed deps /= Just True)
  where
    comp = maybe ComponentLib (ComponentSubLib . mkUnqualComponentName) lib
    fixed = unqualifiedFlagConstraints cs (exAvName a)

-- | The dependencies introduced by choosing an instance: regular ones first,
-- setup ones second.
--
-- An installed package depends on exact installed units. A source package's
-- library, sub-library, foreign-library and executable components are always
-- solved; test and benchmark components only when their stanza is enabled.
instanceDeps :: [OptionalStanza] -> Flags -> Instance -> ([Dep], [Dep])
instanceDeps _ _ (Installed i) = (map DepUnit (exInstBuildAgainst i), [])
instanceDeps stanzas flags (Source a) =
  ( concat [deps | (comp, deps) <- comps, solved comp]
  , concat [deps | (ComponentSetup, deps) <- comps]
  )
  where
    comps =
      [ (comp, fromMaybe [] (componentDeps flags deps))
      | (comp, deps) <- CD.toList (exAvDeps a)
      ]
    solved ComponentLib = True
    solved (ComponentSubLib _) = True
    solved (ComponentFLib _) = True
    solved (ComponentExe _) = True
    solved (ComponentTest _) = TestStanzas `elem` stanzas
    solved (ComponentBench _) = BenchStanzas `elem` stanzas
    solved ComponentSetup = False

-- | Qualify the dependencies of a choice made at a qualified name: regular
-- dependencies inherit the scope, setup dependencies of @P@ go to @Setup P@
-- in the same namespace ('qualifyDeps').
choiceGoals :: QName -> Choice -> [Goal]
choiceGoals ((ns, q), p) ch =
  map (goal (ns, q)) regular ++ map (goal (ns, Setup p)) setup
  where
    (regular, setup) = instanceDeps (chStanzas ch) (chFlags ch) (chInstance ch)
    goal s (DepLib n l vr) = LibDep (s, n) l vr
    goal s (DepUnit h) = UnitDep s h

-- | Does a choice satisfy an unqualified dependency?
satisfiesDep :: [ExConstraint] -> Dep -> Choice -> Bool
satisfiesDep cs (DepLib n l vr) ch =
  instName inst == n
    && withinRange (mkSimpleVersion (instVersion inst)) vr
    && providesLibrary cs l inst
  where
    inst = chInstance ch
satisfiesDep _ (DepUnit h) ch = instHash (chInstance ch) == Just h

-- | Does a choice satisfy a goal? Scopes are matched by the caller.
satisfies :: [ExConstraint] -> Goal -> Choice -> Bool
satisfies _ (Target (_, n)) ch = instName (chInstance ch) == n
satisfies cs (LibDep (_, n) l vr) ch = satisfiesDep cs (DepLib n l vr) ch
satisfies cs (UnitDep _ h) ch = satisfiesDep cs (DepUnit h) ch

{-------------------------------------------------------------------------------
  Constraints
-------------------------------------------------------------------------------}

-- | Mirrors 'constraintScopeMatches'.
scopeMatches :: ConstraintScope -> QName -> Bool
scopeMatches (ScopeAnyQualifier pn) (_, n) = unPackageName pn == n
scopeMatches (ScopeAnySetupQualifier pn) ((_, q), n) =
  unPackageName pn == n && isSetup q
  where
    isSetup (Setup _) = True
    isSetup Toplevel = False
scopeMatches (ScopeTarget pn) ((ns, q), n) =
  unPackageName pn == n && q == Toplevel && namespaceMatches ns
  where
    namespaceMatches DefaultNamespace = True
    namespaceMatches (Independent t) = t == unPackageName pn
scopeMatches (ScopeQualified pq pn) ((_, q), n) =
  unPackageName pn == n && qualifierMatches pq q
  where
    qualifierMatches P.QualToplevel Toplevel = True
    qualifierMatches (P.QualSetup p) (Setup p') = unPackageName p == p'
    qualifierMatches _ _ = False

versionConstraints :: [ExConstraint] -> QName -> [(ExConstraint, VersionRange)]
versionConstraints cs qn =
  [(c, vr) | c@(ExVersionConstraint scope vr) <- cs, scopeMatches scope qn]

-- | Flag constraints that apply to a qualified name.
flagConstraints :: [ExConstraint] -> QName -> Flags
flagConstraints cs qn =
  Map.fromList [(f, b) | ExFlagConstraint scope f b <- cs, scopeMatches scope qn]

-- | Flag constraints with the any-qualifier scope. These are the only ones
-- index conversion uses when deciding buildability statically.
unqualifiedFlagConstraints :: [ExConstraint] -> ExamplePkgName -> Flags
unqualifiedFlagConstraints cs n =
  Map.fromList
    [ (f, b)
    | ExFlagConstraint (ScopeAnyQualifier pn) f b <- cs
    , unPackageName pn == n
    ]

-- | The values a flag may take for a package in a scope.
--
-- A flag constraint on the scope fixes the value. Otherwise an automatic flag
-- is free, and a manual flag may only be its default or a value that some
-- flag constraint on the package, in any scope, asks for
-- ('enforceManualFlags').
allowedFlagValues :: [ExConstraint] -> QName -> ExampleAvailable -> ExampleFlagName -> [Bool]
allowedFlagValues cs qn@(_, n) a f =
  case Map.lookup f (flagConstraints cs qn) of
    Just b -> [b]
    Nothing -> case find ((== f) . exFlagName) (exAvFlags a) of
      Just flag
        | exFlagType flag == Manual ->
            ordNub (exFlagDefault flag : anyScopeValues)
      _ -> [True, False]
  where
    anyScopeValues =
      [ b
      | ExFlagConstraint scope f' b <- cs
      , f' == f
      , scopeMatches scope qn || scopeNames scope == n
      ]
    scopeNames (ScopeTarget pn) = unPackageName pn
    scopeNames (ScopeQualified _ pn) = unPackageName pn
    scopeNames (ScopeAnySetupQualifier pn) = unPackageName pn
    scopeNames (ScopeAnyQualifier pn) = unPackageName pn

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

-- | The stanzas a constraint forces on for a source package in a scope. A
-- constraint on a stanza the package does not have never fires.
requiredStanzas :: [ExConstraint] -> QName -> ExampleAvailable -> [OptionalStanza]
requiredStanzas cs qn a =
  ordNub
    [ s
    | ExStanzaConstraint scope ss <- cs
    , scopeMatches scope qn
    , s <- ss
    , s `elem` availableStanzas a
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

-- | Problems with choosing an instance at a qualified name, independent of
-- the rest of the resolution. Version constraints apply to installed and
-- source instances alike, and only to packages that are actually chosen.
instanceProblems :: [ExConstraint] -> QName -> Instance -> [Problem]
instanceProblems cs qn inst =
  [NonReinstallableSource n | Source _ <- [inst], nonReinstallable n]
    ++ [ ConstraintViolated n (show c)
       | (c, vr) <- versionConstraints cs qn
       , not (withinRange (mkSimpleVersion (instVersion inst)) vr)
       ]
  where
    n = instName inst

-- | Problems with a choice: those of its instance, plus stanza and flag
-- constraints that the chosen assignment does not respect. Installed
-- packages have neither stanzas nor flags, so those constraints only apply
-- to source instances.
choiceProblems :: [ExConstraint] -> QName -> Choice -> [Problem]
choiceProblems cs qn ch =
  instanceProblems cs qn inst
    ++ [ ConstraintViolated n (show c)
       | c@(ExStanzaConstraint scope ss) <- cs
       , scopeMatches scope qn
       , Source a <- [inst]
       , s <- ss
       , s `elem` availableStanzas a
       , s `notElem` chStanzas ch
       ]
    ++ [ ConstraintViolated n (show c)
       | c@(ExFlagConstraint scope f b) <- cs
       , scopeMatches scope qn
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

-- | The search state: the choices made so far, and the link requirements the
-- single instance restriction has introduced. A link between two qualified
-- names means they must resolve to the same instance.
data State = State
  { stRes :: Resolution
  , stLinks :: Map QName (Set QName)
  }

-- | Search for a resolution by plain depth-first search.
--
-- The first argument is fuel: every goal processed costs one unit, and when
-- it runs out the result is 'OutOfFuel'. The second says whether targets are
-- independent goals. There are no heuristics: goals are processed in the
-- order they are introduced, and candidates are tried in database order with
-- flags enumerated True (or default) first.
resolve :: Int -> Bool -> [ExConstraint] -> ExampleDb -> [ExamplePkgName] -> OracleResult
resolve fuel0 indep cs db targets =
  case go fuel0 (State Map.empty Map.empty) [Target (targetScope indep t, t) | t <- targets] of
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

    -- The qualified name a goal is about, or Nothing for a dependency on an
    -- installed unit that is not in the database (the solver marks the
    -- depender broken).
    goalQName :: Goal -> Maybe QName
    goalQName (Target qn) = Just qn
    goalQName (LibDep qn _ _) = Just qn
    goalQName (UnitDep s h) = (\n -> (s, n)) <$> Map.lookup h byHash

    candidates :: State -> QName -> Goal -> [Choice]
    candidates st qn@(_, n) g =
      [ ch
      | inst <- Map.findWithDefault [] n instances
      , null (instanceProblems cs qn inst)
      , flags <- assignments inst
      , let ch = Choice inst flags (stanzas inst)
      , satisfies cs g ch
      , linkable st qn ch
      ]
      where
        stanzas (Installed _) = []
        stanzas (Source a) = requiredStanzas cs qn a
        assignments (Installed _) = [Map.empty]
        assignments (Source a) =
          map Map.fromList (traverse (\f -> [(f, b) | b <- allowedFlagValues cs qn a f]) (usedFlags a))

    -- The same instance chosen in another scope forces an identical choice,
    -- and a linked qualified name that is already resolved forces the same
    -- instance.
    linkable :: State -> QName -> Choice -> Bool
    linkable st qn ch =
      all (sameChoice ch) (sameInstanceElsewhere st qn ch)
        && and
          [ instKey (chInstance ch') == instKey (chInstance ch)
          | p <- Set.toList (Map.findWithDefault Set.empty qn (stLinks st))
          , Just ch' <- [Map.lookup p (stRes st)]
          ]

    sameInstanceElsewhere :: State -> QName -> Choice -> [Choice]
    sameInstanceElsewhere st (s, n) ch =
      [ ch'
      | ((s', n'), ch') <- Map.toList (stRes st)
      , n' == n
      , s' /= s
      , instKey (chInstance ch') == instKey (chInstance ch)
      ]

    -- Record the choice and, for every other scope holding the same
    -- instance, require the corresponding dependencies to be linked too.
    choose :: QName -> Choice -> State -> State
    choose qn@(_, n) ch st =
      st
        { stRes = Map.insert qn ch (stRes st)
        , stLinks = foldl' addLink (stLinks st) newLinks
        }
      where
        newLinks =
          [ (a, b)
          | ((s', n'), ch') <- Map.toList (stRes st)
          , n' == n
          , instKey (chInstance ch') == instKey (chInstance ch)
          , (g, g') <- zip (choiceGoals qn ch) (choiceGoals (s', n') ch')
          , Just a <- [goalQName g]
          , Just b <- [goalQName g']
          , a /= b
          ]
        addLink m (a, b) =
          Map.insertWith Set.union a (Set.singleton b) $
            Map.insertWith Set.union b (Set.singleton a) m

    go :: Int -> State -> [Goal] -> Search
    go _ st [] = Found (stRes st)
    go fuel st (g : gs)
      | fuel <= 0 = Starved
      | otherwise = case goalQName g of
          Nothing -> NotFound fuel'
          Just qn -> case Map.lookup qn (stRes st) of
            Just ch
              | satisfies cs g ch -> go fuel' st gs
              | otherwise -> NotFound fuel'
            Nothing -> tryEach qn fuel' (candidates st qn g)
      where
        fuel' = fuel - 1

        tryEach _ f [] = NotFound f
        tryEach qn f (ch : chs) =
          case go f (choose qn ch st) (choiceGoals qn ch ++ gs) of
            Found r -> Found r
            Starved -> Starved
            NotFound f' -> tryEach qn f' chs

{-------------------------------------------------------------------------------
  Validity check
-------------------------------------------------------------------------------}

-- | A reference to a package in a plan.
data ResolvedRef = ResolvedRef
  { rrName :: ExamplePkgName
  , rrVersion :: ExamplePkgVersion
  , rrHash :: Maybe ExamplePkgHash
  }
  deriving (Eq, Ord, Show)

-- | A package in a plan, described without reference to solver types. The
-- plan does not record scopes, but it records which package each dependency
-- was resolved to, and that is enough to reconstruct them.
data ResolvedPackage = ResolvedPackage
  { rpName :: ExamplePkgName
  , rpVersion :: ExamplePkgVersion
  , rpInstalledHash :: Maybe ExamplePkgHash
  , rpFlags :: [(ExampleFlagName, Bool)]
  , rpStanzas :: [OptionalStanza]
  , rpDeps :: [ResolvedRef]
  -- ^ What the regular (non-setup) components depend on.
  , rpSetupDeps :: [ResolvedRef]
  -- ^ What the setup component depends on.
  }
  deriving (Eq, Show)

rpRef :: ResolvedPackage -> ResolvedRef
rpRef rp = ResolvedRef (rpName rp) (rpVersion rp) (rpInstalledHash rp)

data Problem
  = MissingTarget ExamplePkgName
  | -- | Two instances of one name reachable in the same scope.
    MultipleVersions Scope ExamplePkgName
  | UnknownInstance ExamplePkgName ExamplePkgVersion
  | -- | A dependency edge to a package that is not in the plan.
    UnknownReference ExamplePkgName ResolvedRef
  | -- | A dependency that none of the package's edges satisfies.
    MissingDependency ExamplePkgName Dep
  | -- | A package that no target reaches.
    UnreachablePackage ExamplePkgName
  | ConstraintViolated ExamplePkgName String
  | NonReinstallableSource ExamplePkgName
  deriving (Eq, Show)

-- | Check that a plan is a resolution of the given database and targets.
--
-- Scopes are reconstructed from the plan's own edges, the way
-- 'SolverInstallPlan.dependencyInconsistencies' does it. The plan does not
-- say which copy of a target is the top-level one, so the top-level scope is
-- taken to be the closure under regular edges of the /roots/: packages that
-- nothing depends on. Every root is a target, so this closure lies within the
-- top-level scope (one closure per root with independent goals). The setup
-- scope of a package is the closure of its setup edges under regular edges.
-- Within each scope there must be one instance per name; every dependency of
-- every package must be satisfied by one of its edges; constraints are
-- checked per scope; and every package must lie in some scope.
--
-- Minimality is deliberately not checked: the solver may enable stanzas or
-- include packages that are not strictly needed, and that is still a
-- resolution.
checkResolution
  :: [ExConstraint]
  -> Bool
  -- ^ independent goals
  -> ExampleDb
  -> [ExamplePkgName]
  -> [ResolvedPackage]
  -> [Problem]
checkResolution cs indep db targets plan =
  [MissingTarget t | t <- targets, t `notElem` map rpName plan]
    ++ concatMap scopeProblems scopes
    ++ [UnreachablePackage (rpName rp) | rp <- plan, rpRef rp `Set.notMember` reached]
    ++ concatMap edgeProblems plan
  where
    byRef :: Map ResolvedRef ResolvedPackage
    byRef = Map.fromList [(rpRef rp, rp) | rp <- plan]

    -- Closure under regular edges, in plan order, ignoring unknown edges.
    closure :: [ResolvedPackage] -> [ResolvedPackage]
    closure = go Set.empty
      where
        go _ [] = []
        go seen (rp : rest)
          | rpRef rp `Set.member` seen = go seen rest
          | otherwise =
              rp : go (Set.insert (rpRef rp) seen) (mapMaybe (`Map.lookup` byRef) (rpDeps rp) ++ rest)

    referenced = Set.fromList (concatMap (\rp -> rpDeps rp ++ rpSetupDeps rp) plan)

    -- Nothing depends on a root, so it can only be there as a target.
    roots = [rp | rp <- plan, rpRef rp `Set.notMember` referenced, rpName rp `elem` targets]

    scopes :: [(Scope, [ResolvedPackage])]
    scopes =
      ( if indep
          then [((Independent (rpName rp), Toplevel), closure [rp]) | rp <- roots]
          else [((DefaultNamespace, Toplevel), closure roots)]
      )
        ++ [ ((DefaultNamespace, Setup (rpName rp)), closure (mapMaybe (`Map.lookup` byRef) (rpSetupDeps rp)))
           | rp <- plan
           , not (null (rpSetupDeps rp))
           ]

    reached = Set.fromList [rpRef rp | (_, rps) <- scopes, rp <- rps]

    scopeProblems (s, rps) =
      [MultipleVersions s n | (n : _ : _) <- L.group (L.sort (map rpName rps))]
        ++ concat
          [ case toChoice rp of
            Nothing -> [UnknownInstance (rpName rp) (rpVersion rp)]
            Just ch -> choiceProblems cs (s, rpName rp) ch
          | rp <- rps
          ]

    edgeProblems rp =
      [UnknownReference (rpName rp) r | r <- rpDeps rp ++ rpSetupDeps rp, r `Map.notMember` byRef]
        ++ case toChoice rp of
          Nothing -> []
          Just ch ->
            let (regular, setup) = instanceDeps (chStanzas ch) (chFlags ch) (chInstance ch)
                missing edges deps =
                  [ MissingDependency (rpName rp) d
                  | d <- deps
                  , not (any (satisfiesDep cs d) (mapMaybe (\r -> Map.lookup r byRef >>= toChoice) edges))
                  ]
             in missing (rpDeps rp) regular ++ missing (rpSetupDeps rp) setup

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
            , rpDeps = map ref (CD.nonSetupDeps (instSolverPkgLibDeps ipkg))
            , rpSetupDeps = map ref (CD.setupDeps (instSolverPkgLibDeps ipkg))
            }
    conv (Configured spkg) =
      let PackageIdentifier pn v = packageId (solverPkgSource spkg)
       in ResolvedPackage
            { rpName = unPackageName pn
            , rpVersion = simpleVersion v
            , rpInstalledHash = Nothing
            , rpFlags = [(unFlagName f, b) | (f, b) <- unFlagAssignment (solverPkgFlags spkg)]
            , rpStanzas = optStanzaSetToList (solverPkgStanzas spkg)
            , rpDeps = map ref (CD.nonSetupDeps (solverPkgLibDeps spkg))
            , rpSetupDeps = map ref (CD.setupDeps (solverPkgLibDeps spkg))
            }

    ref (PreExistingId (PackageIdentifier pn v) uid) =
      ResolvedRef (unPackageName pn) (simpleVersion v) (Just (unUnitId uid))
    ref (PlannedId (PackageIdentifier pn v)) =
      ResolvedRef (unPackageName pn) (simpleVersion v) Nothing

    simpleVersion v = case versionNumbers v of
      (n : _) -> n
      [] -> error "Oracle.fromSolverPlan: empty version"

-- | Describe an oracle resolution in the same terms, for checking the oracle
-- against itself. Linked copies collapse into one package, as in a plan.
toResolved :: Resolution -> [ResolvedPackage]
toResolved res = Map.elems (Map.fromList [(rpRef rp, rp) | rp <- map conv (Map.toList res)])
  where
    conv (qn@((ns, _), p), ch) =
      ResolvedPackage
        { rpName = instName inst
        , rpVersion = instVersion inst
        , rpInstalledHash = instHash inst
        , rpFlags = Map.toList (chFlags ch)
        , rpStanzas = chStanzas ch
        , rpDeps = mapMaybe (ref (fst qn)) regular
        , rpSetupDeps = mapMaybe (ref (ns, Setup p)) setup
        }
      where
        inst = chInstance ch
        (regular, setup) = instanceDeps (chStanzas ch) (chFlags ch) inst

    ref s (DepLib n _ _) = choiceRef <$> Map.lookup (s, n) res
    ref s (DepUnit h) =
      listToMaybe
        [ choiceRef ch
        | ((s', _), ch) <- Map.toList res
        , s' == s
        , instHash (chInstance ch) == Just h
        ]

    choiceRef ch =
      ResolvedRef (instName inst) (instVersion inst) (instHash inst)
      where
        inst = chInstance ch

{-------------------------------------------------------------------------------
  Unit tests pinning the intended semantics
-------------------------------------------------------------------------------}

-- | A hand-written case with the verdict both the oracle and the solver must
-- give. The QuickCheck module runs these through the solver.
data SolverCase = SolverCase
  { scName :: String
  , scIndependent :: Bool
  , scConstraints :: [ExConstraint]
  , scDb :: ExampleDb
  , scTargets :: [ExamplePkgName]
  , scVerdict :: Verdict
  }

solverCases :: [SolverCase]
solverCases =
  [ SolverCase "chain of dependencies" False [] dbChain ["A"] IsSolvable
  , SolverCase "fixed version that does not exist" False [] dbMissingVersion ["A"] IsUnsolvable
  , SolverCase "flag chooses a satisfiable branch" False [] dbFlag ["A"] IsSolvable
  , SolverCase "flag constraint forces the unsatisfiable branch" False [flagFalse "A" "F"] dbFlag ["A"] IsUnsolvable
  , SolverCase "unbuildable branch hides an unsatisfiable dependency" False [] dbUnbuildableBranch ["A"] IsSolvable
  , SolverCase "flag-gated unbuildable library still provides a library" False [] dbUnbuildableBranch ["C"] IsSolvable
  , SolverCase "statically unbuildable library cannot be depended on" False [] dbUnbuildableLib ["C"] IsUnsolvable
  , SolverCase "statically unbuildable library can be a target" False [] dbUnbuildableLib ["A"] IsSolvable
  , SolverCase "installed package built against installed unit" False [] dbInstalled ["A"] IsSolvable
  , SolverCase "version constraint excludes the installed unit" False [ExVersionConstraint (anyQ "B") (notThisVersion (mkSimpleVersion 1))] dbInstalled ["A"] IsUnsolvable
  , SolverCase "source base is not selectable" False [] [Right (exAv "base" 1 [])] ["base"] IsUnsolvable
  , SolverCase "installed base is selectable" False [] [Left (exInst "base" 1 "base-1" [])] ["base"] IsSolvable
  , SolverCase "dependency on an executable-only package" False [] dbExeOnly ["B"] IsUnsolvable
  , SolverCase "executable-only package can be a target" False [] dbExeOnly ["A"] IsSolvable
  , SolverCase "stanza constraint enables test dependencies" False [ExStanzaConstraint (anyQ "A") [TestStanzas]] dbTestStanza ["A"] IsSolvable
  , SolverCase "stanza constraint makes test dependencies required" False [ExStanzaConstraint (anyQ "A") [TestStanzas]] dbBadTestStanza ["A"] IsUnsolvable
  , SolverCase "test dependencies are optional without a constraint" False [] dbBadTestStanza ["A"] IsSolvable
  , SolverCase "stanza constraint does not apply to an installed package" False [ExStanzaConstraint (anyQ "A") [TestStanzas]] dbInstalled ["A"] IsSolvable
  , SolverCase "stanza constraint on a package without tests never fires" False [ExStanzaConstraint (anyQ "A") [TestStanzas]] dbChain ["A"] IsSolvable
  , -- Setup dependencies live in their own scope (Solver.hs db7).
    SolverCase "setup dependency may differ in version from the library dependency" False [] dbSetup ["F"] IsSolvable
  , SolverCase "setup dependency is chosen freely when the library is fixed" False [] dbSetup ["D"] IsSolvable
  , SolverCase "setup dependency is not constrained by the top-level version" False [] dbSetup ["E"] IsSolvable
  , SolverCase "two setup scopes may pick two versions" False [] dbTwoSetupScopes ["C", "D"] IsSolvable
  , SolverCase "top-level scope must agree on one version" False [] dbTwoSetupScopes ["E", "F"] IsUnsolvable
  , SolverCase "independent goals may pick two versions" True [] dbTwoSetupScopes ["E", "F"] IsSolvable
  , -- The single instance restriction (Solver.hs dbLinkedSetupDepWithManualFlag).
    SolverCase "linked setup dependency copies the top-level flag" False [ExFlagConstraint (ScopeQualified P.QualToplevel (mkPackageName "B")) "flag" False] dbLinkedManualFlag ["A"] IsSolvable
  , SolverCase "linked setup dependency cannot have a conflicting flag" False [ExFlagConstraint (ScopeQualified P.QualToplevel (mkPackageName "B")) "flag" True, ExFlagConstraint (ScopeQualified (P.QualSetup (mkPackageName "A")) (mkPackageName "B")) "flag" False] dbLinkedManualFlag ["A"] IsUnsolvable
  , SolverCase "unlinked setup dependency may have a different flag" False [ExFlagConstraint (ScopeQualified P.QualToplevel (mkPackageName "B")) "flag" True, ExFlagConstraint (ScopeQualified (P.QualSetup (mkPackageName "A")) (mkPackageName "B")) "flag" False] dbUnlinkedManualFlag ["A"] IsSolvable
  , SolverCase "linked packages must resolve dependencies alike" False [ExVersionConstraint (ScopeTarget (mkPackageName "Q")) (thisVersion (mkSimpleVersion 2)), ExVersionConstraint (ScopeQualified (P.QualSetup (mkPackageName "T")) (mkPackageName "Q")) (thisVersion (mkSimpleVersion 1))] dbLinkedDeps ["T"] IsUnsolvable
  , SolverCase "linked packages resolve dependencies alike" False [ExVersionConstraint (ScopeTarget (mkPackageName "Q")) (thisVersion (mkSimpleVersion 2))] dbLinkedDeps ["T"] IsSolvable
  , SolverCase "installed and source instances of one version may coexist across scopes" False [] dbInstalledAndSource ["T"] IsSolvable
  , -- Sub-library dependencies (Solver.hs "sub-library dependencies").
    SolverCase "missing sub-library" False [] [Right (exAv "A" 1 [ExSubLibAny "B" "sub-lib"]), Right (exAv "B" 1 [])] ["A"] IsUnsolvable
  , SolverCase "private sub-library" False [] dbPrivateSubLib ["A"] IsUnsolvable
  , SolverCase "sub-library made private by a flag constraint" False [ExFlagConstraint (anyQ "B") "make-lib-private" True] dbFlaggedSubLib ["A"] IsUnsolvable
  , SolverCase "sub-library is visible when only a flag choice could make it private" False [] dbFlaggedSubLib ["A"] IsSolvable
  , SolverCase "public sub-library of an executable-only package" False [] dbPublicSubLib ["A"] IsSolvable
  , SolverCase "choose the version that has the sub-library" False [] dbSubLibVersions ["A"] IsSolvable
  , SolverCase "choose the version whose sub-library is public" False [] dbSubLibVisibilities ["A"] IsSolvable
  , SolverCase "installed packages provide no sub-libraries" False [] [Right (exAv "A" 1 [ExSubLibAny "B" "sub-lib"]), Left (exInst "B" 1 "B-1-hash" [])] ["A"] IsUnsolvable
  ]
  where
    anyQ = ScopeAnyQualifier . mkPackageName
    flagFalse n f = ExFlagConstraint (anyQ n) f False

tests :: [TestTree]
tests =
  [ testGroup
      "resolve"
      ( [ testCase (scName c) $
          verdict (resolve fuel (scIndependent c) (scConstraints c) (scDb c) (scTargets c)) @?= scVerdict c
        | c <- solverCases
        ]
          ++ [ testCase "no fuel gives no verdict" $
                verdict (resolve 0 False [] dbChain ["A"]) @?= IsUnknown
             ]
      )
  , testGroup
      "checkResolution"
      [ testCase "accepts the oracle's own resolutions" $
          for_ [c | c <- solverCases, scVerdict c == IsSolvable] $ \c ->
            case resolve fuel (scIndependent c) (scConstraints c) (scDb c) (scTargets c) of
              Solvable res ->
                checkResolution (scConstraints c) (scIndependent c) (scDb c) (scTargets c) (toResolved res) @?= []
              other -> assertBool (scName c ++ ": expected a resolution, got " ++ show other) False
      , testCase "rejects a missing target" $
          checkResolution [] False dbChain ["A"] [] @?= [MissingTarget "A"]
      , testCase "rejects a dropped dependency" $
          checkResolution [] False dbChain ["A"] [src "A" 1 [] []]
            @?= [MissingDependency "A" (DepLib "B" Nothing anyVersion)]
      , testCase "rejects an edge to a package that is not in the plan" $
          checkResolution [] False dbChain ["A"] [src "A" 1 [] [b1]]
            @?= [UnknownReference "A" b1, MissingDependency "A" (DepLib "B" Nothing anyVersion)]
      , testCase "rejects an edge to a version without the sub-library" $
          checkResolution [] False dbSubLibVersions ["A"] [src "A" 1 [] [b2], src "B" 2 [] []]
            @?= [MissingDependency "A" (DepLib "B" (Just "sub-lib-v1") anyVersion)]
      , testCase "rejects two versions of one package in one scope" $
          checkResolution [] False dbTwoVersions ["A", "C"] [src "A" 1 [] [b1], src "C" 1 [] [b2], src "B" 1 [] [], src "B" 2 [] []]
            @?= [MultipleVersions (DefaultNamespace, Toplevel) "B"]
      , testCase "accepts two versions of one package in two scopes" $
          checkResolution [] False dbSetup ["F"] [src "F" 1 [] [a2] `withSetup` [a1], src "A" 1 [] [], src "A" 2 [] []]
            @?= []
      , testCase "rejects a flag whose branch is unsatisfied" $
          checkResolution [] False dbFlag ["A"] [src "A" 1 [("F", False)] [b1], src "B" 1 [] []]
            @?= [MissingDependency "A" (DepLib "B" Nothing (thisVersion (mkSimpleVersion 9)))]
      , testCase "rejects a violated version constraint" $
          let c = ExVersionConstraint (ScopeAnyQualifier (mkPackageName "B")) (notThisVersion (mkSimpleVersion 1))
           in checkResolution [c] False dbChain ["A"] [src "A" 1 [] [b1], src "B" 1 [] []]
                @?= [ConstraintViolated "B" (show c)]
      , testCase "rejects an instance that is not in the database" $
          checkResolution [] False dbChain ["A"] [src "A" 1 [] [b7], src "B" 7 [] []]
            @?= [UnknownInstance "B" 7, MissingDependency "A" (DepLib "B" Nothing anyVersion)]
      , testCase "rejects an unreachable package" $
          checkResolution [] False dbChain ["A"] [src "A" 1 [] [b1], src "B" 1 [] [], src "C" 1 [] []]
            @?= [UnreachablePackage "C"]
      ]
  ]
  where
    fuel = 10000

    src n v flags deps =
      ResolvedPackage
        { rpName = n
        , rpVersion = v
        , rpInstalledHash = Nothing
        , rpFlags = flags
        , rpStanzas = []
        , rpDeps = deps
        , rpSetupDeps = []
        }

    withSetup rp deps = rp{rpSetupDeps = deps}

    b1 = ResolvedRef "B" 1 Nothing
    b2 = ResolvedRef "B" 2 Nothing
    b7 = ResolvedRef "B" 7 Nothing
    a1 = ResolvedRef "A" 1 Nothing
    a2 = ResolvedRef "A" 2 Nothing

{-------------------------------------------------------------------------------
  Example databases
-------------------------------------------------------------------------------}

dbChain, dbMissingVersion, dbTwoVersions, dbFlag, dbUnbuildableBranch, dbUnbuildableLib, dbInstalled, dbExeOnly, dbTestStanza, dbBadTestStanza, dbSetup, dbTwoSetupScopes, dbLinkedManualFlag, dbUnlinkedManualFlag, dbLinkedDeps, dbInstalledAndSource, dbPrivateSubLib, dbFlaggedSubLib, dbPublicSubLib, dbSubLibVersions, dbSubLibVisibilities :: ExampleDb
dbChain = [Right (exAv "A" 1 [ExAny "B"]), Right (exAv "B" 1 [])]
dbMissingVersion = [Right (exAv "A" 1 [ExFix "B" 2]), Right (exAv "B" 1 [])]
dbTwoVersions =
  [ Right (exAv "A" 1 [ExFix "B" 1])
  , Right (exAv "C" 1 [ExFix "B" 2])
  , Right (exAv "B" 1 [])
  , Right (exAv "B" 2 [])
  ]
dbFlag =
  [ Right (exAv "A" 1 [exFlagged "F" [ExFix "B" 1] [ExFix "B" 9]])
  , Right (exAv "B" 1 [])
  ]
-- A's library is unbuildable when F is True, and needs a missing B-9 when F
-- is False. Buildability depends on a free flag, so the solver treats the
-- library as buildable and picks F = True.
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
dbBadTestStanza = [Right (exAv "A" 1 [] `withTest` exTest "test" [ExAny "B"])]
-- Solver.hs db7.
dbSetup =
  [ Right (exAv "A" 1 [])
  , Right (exAv "A" 2 [])
  , Right (exAv "D" 1 [ExFix "A" 1] `withSetupDeps` [ExAny "A"])
  , Right (exAv "E" 1 [ExAny "A"] `withSetupDeps` [ExFix "A" 1])
  , Right (exAv "F" 1 [ExFix "A" 2] `withSetupDeps` [ExFix "A" 1])
  ]
-- Solver.hs db8, plus E and F which need the two versions at the top level.
dbTwoSetupScopes =
  [ Right (exAv "A" 1 [])
  , Right (exAv "B" 1 [ExAny "A"])
  , Right (exAv "B" 2 [ExAny "A"])
  , Right (exAv "C" 1 [] `withSetupDeps` [ExFix "B" 1])
  , Right (exAv "D" 1 [] `withSetupDeps` [ExFix "B" 2])
  , Right (exAv "E" 1 [ExFix "B" 1])
  , Right (exAv "F" 1 [ExFix "B" 2])
  ]
-- Solver.hs dbLinkedSetupDepWithManualFlag: A's library and setup both use
-- B-1, so the two copies are linked and must agree on the manual flag.
dbLinkedManualFlag =
  [ Right (exAv "A" 1 [ExFix "B" 1] `withSetupDeps` [ExFix "B" 1])
  , Right
      ( declareFlags [ExFlag "flag" True Manual] $
          exAv "B" 1 [exFlagged "flag" [ExAny "b-1-true-dep"] [ExAny "b-1-false-dep"]]
      )
  , Right (exAv "b-1-true-dep" 1 [])
  , Right (exAv "b-1-false-dep" 1 [])
  ]
-- Solver.hs dbSetupDepWithManualFlag: the two copies are different versions,
-- so they are not linked and may have different flags.
dbUnlinkedManualFlag =
  [ Right (exAv "A" 1 [ExFix "B" 1] `withSetupDeps` [ExFix "B" 2])
  , Right
      ( declareFlags [ExFlag "flag" True Manual] $
          exAv "B" 1 [exFlagged "flag" [ExAny "b-1-true-dep"] [ExAny "b-1-false-dep"]]
      )
  , Right
      ( declareFlags [ExFlag "flag" True Manual] $
          exAv "B" 2 [exFlagged "flag" [ExAny "b-2-true-dep"] [ExAny "b-2-false-dep"]]
      )
  , Right (exAv "b-1-true-dep" 1 [])
  , Right (exAv "b-1-false-dep" 1 [])
  , Right (exAv "b-2-true-dep" 1 [])
  , Right (exAv "b-2-false-dep" 1 [])
  ]
-- T uses P-1 both as a library and as a setup dependency, so the two copies
-- of P-1 are linked and their dependency Q must resolve to the same version
-- in both scopes.
dbLinkedDeps =
  [ Right (exAv "T" 1 [ExFix "P" 1] `withSetupDeps` [ExFix "P" 1])
  , Right (exAv "P" 1 [ExAny "Q"])
  , Right (exAv "Q" 1 [])
  , Right (exAv "Q" 2 [])
  ]
dbPrivateSubLib =
  [ Right (exAv "A" 1 [ExSubLibAny "B" "sub-lib"])
  , Right (exAvNoLibrary "B" 1 `withSubLibrary` exSubLib "sub-lib" [])
  ]
-- The sub-library is private at its root and public under the flag's False
-- branch, so it is statically private only when a constraint sets the flag.
dbFlaggedSubLib =
  [ Right (exAv "A" 1 [ExSubLibAny "B" "sub-lib"])
  , Right
      ( exAvNoLibrary "B" 1
          `withSubLibrary` exSubLib "sub-lib" [ExFlagged "make-lib-private" (dependencies []) publicDependencies]
      )
  ]
dbPublicSubLib =
  [ Right (exAv "A" 1 [ExSubLibAny "B" "sub-lib"])
  , Right (exAvNoLibrary "B" 1 `withSubLibrary` ExSubLib "sub-lib" publicDependencies)
  ]
dbSubLibVersions =
  [ Right (exAv "A" 1 [ExSubLibAny "B" "sub-lib-v1"])
  , Right (exAv "B" 2 [] `withSubLibrary` ExSubLib "sub-lib-v2" publicDependencies)
  , Right (exAv "B" 1 [] `withSubLibrary` ExSubLib "sub-lib-v1" publicDependencies)
  ]
dbSubLibVisibilities =
  [ Right (exAv "A" 1 [ExSubLibAny "B" "sub-lib"])
  , Right (exAv "B" 2 [] `withSubLibrary` ExSubLib "sub-lib" (dependencies []))
  , Right (exAv "B" 1 [] `withSubLibrary` ExSubLib "sub-lib" publicDependencies)
  ]
-- P-1 is installed and also available from source; the installed unit and
-- the source package are different instances, so they need not be linked.
dbInstalledAndSource =
  let p = exInst "P" 1 "P-1-hash" []
   in [ Left p
      , Right (exAv "P" 1 [ExAny "Q"])
      , Right (exAv "Q" 1 [])
      , Right (exAv "T" 1 [ExFix "P" 1] `withSetupDeps` [ExFix "P" 1])
      ]
