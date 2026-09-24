{-# OPTIONS_GHC -Wno-orphans #-}

module UnitTests.Distribution.Solver.Modular.QuickCheck (tests) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Control.Arrow ((&&&))
import Data.Either (lefts)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (groupBy, isInfixOf)
import qualified Data.List as L

import Text.Show.Pretty (parseValue, valToStr)

import Language.Haskell.Extension (Extension (..), KnownExtension (..), Language (..))
import Test.QuickCheck (Arbitrary (..), Blind (..), Gen, Positive (..), classify, counterexample, elements, frequency, listOf, oneof, property, shrinkList, shrinkNothing, shuffle, sublistOf, vectorOf, (===), (==>))
import Test.QuickCheck.Instances.Cabal ()
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

import Distribution.Solver.Types.Flag (FlagType (..))
import Distribution.Types.Flag (FlagName)
import Distribution.Utils.ShortText (ShortText, fromShortText)

import Distribution.Client.Setup (defaultMaxBackjumps)

import Distribution.Types.LibraryVisibility
import Distribution.Types.PackageName
import Distribution.Types.UnqualComponentName

import Distribution.Solver.Types.ComponentDeps
  ( Component (..)
  , ComponentDep
  , ComponentDeps
  )
import qualified Distribution.Solver.Types.ComponentDeps as CD
import Distribution.Solver.Types.OptionalStanza
import Distribution.Solver.Types.PackageConstraint
import qualified Distribution.Solver.Types.PackagePath as P
import Distribution.Solver.Types.PkgConfigDb
  ( PkgConfigDb
  , pkgConfigDbFromList
  )
import Distribution.Solver.Types.Settings
import Distribution.Solver.Types.Variable
import Distribution.Verbosity
import Distribution.Version

import Distribution.Simple.Utils (ordNub)
import UnitTests.Distribution.Solver.Modular.DSL
import qualified UnitTests.Distribution.Solver.Modular.QuickCheck.Oracle as Oracle
import UnitTests.Distribution.Solver.Modular.QuickCheck.Utils
  ( ArbitraryOrd (..)
  , testPropertyWithSeed
  )

tests :: [TestTree]
tests =
  [ testPropertyWithSeed "solver does not throw exceptions" $
      \test goalOrder reorderGoals indepGoals prefOldest ->
        let r =
              solve
                (EnableBackjumping True)
                (FineGrainedConflicts True)
                reorderGoals
                (CountConflicts True)
                indepGoals
                prefOldest
                (getBlind <$> goalOrder)
                test
         in resultPlan r `seq` ()
  , -- This test checks that certain solver parameters do not affect the
    -- existence of a solution. It runs the solver twice, and only sets those
    -- parameters on the second run. The test also applies parameters that
    -- can affect the existence of a solution to both runs.
    testPropertyWithSeed "target and goal order do not affect solvability" $
      \test targetOrder mGoalOrder1 mGoalOrder2 indepGoals ->
        let r1 = solve' mGoalOrder1 test
            r2 = solve' mGoalOrder2 test{testTargets = targets2}
            solve' goalOrder =
              solve
                (EnableBackjumping True)
                (FineGrainedConflicts True)
                (ReorderGoals False)
                (CountConflicts True)
                indepGoals
                PreferInstalledOrLatest
                (getBlind <$> goalOrder)
            targets = testTargets test
            targets2 = case targetOrder of
              SameOrder -> targets
              ReverseOrder -> reverse targets
         in counterexample (showResults r1 r2) $
              noneReachedBackjumpLimit [r1, r2] ==>
                isRight (resultPlan r1) === isRight (resultPlan r2)
  , testPropertyWithSeed
      "solvable without --independent-goals => solvable with --independent-goals"
      $ \test reorderGoals ->
        let r1 = solve' (IndependentGoals False) test
            r2 = solve' (IndependentGoals True) test
            solve' indep =
              solve
                (EnableBackjumping True)
                (FineGrainedConflicts True)
                reorderGoals
                (CountConflicts True)
                indep
                PreferInstalledOrLatest
                Nothing
         in counterexample (showResults r1 r2) $
              noneReachedBackjumpLimit [r1, r2] ==>
                isRight (resultPlan r1) `implies` isRight (resultPlan r2)
  , testPropertyWithSeed "backjumping does not affect solvability" $
      \test reorderGoals indepGoals ->
        let r1 = solve' (EnableBackjumping True) test
            r2 = solve' (EnableBackjumping False) test
            solve' enableBj =
              solve
                enableBj
                (FineGrainedConflicts False)
                reorderGoals
                (CountConflicts True)
                indepGoals
                PreferInstalledOrLatest
                Nothing
         in counterexample (showResults r1 r2) $
              noneReachedBackjumpLimit [r1, r2] ==>
                isRight (resultPlan r1) === isRight (resultPlan r2)
  , testPropertyWithSeed "fine-grained conflicts does not affect solvability" $
      \test reorderGoals indepGoals ->
        let r1 = solve' (FineGrainedConflicts True) test
            r2 = solve' (FineGrainedConflicts False) test
            solve' fineGrainedConflicts =
              solve
                (EnableBackjumping True)
                fineGrainedConflicts
                reorderGoals
                (CountConflicts True)
                indepGoals
                PreferInstalledOrLatest
                Nothing
         in counterexample (showResults r1 r2) $
              noneReachedBackjumpLimit [r1, r2] ==>
                isRight (resultPlan r1) === isRight (resultPlan r2)
  , testPropertyWithSeed "PreferVersion does not affect solvability" $
      \test reorderGoals indepGoals preferVersion ->
        let r1 = solve' preferVersion test
            r2 = solve' PreferInstalledOrLatest test
            solve' prefOldest =
              solve
                (EnableBackjumping True)
                (FineGrainedConflicts True)
                reorderGoals
                (CountConflicts True)
                indepGoals
                prefOldest
                Nothing
         in counterexample (showResults r1 r2) $
              noneReachedBackjumpLimit [r1, r2] ==>
                isRight (resultPlan r1) === isRight (resultPlan r2)
  , -- The next two tests use --no-count-conflicts, because the goal order used
    -- with --count-conflicts depends on the total set of conflicts seen by the
    -- solver. The solver explores more of the tree and encounters more
    -- conflicts when it doesn't backjump. The different goal orders can lead to
    -- different solutions and cause the test to fail.
    -- TODO: Find a faster way to randomly sort goals, and then use a random
    -- goal order in these tests.

    testPropertyWithSeed
      "backjumping does not affect the result (with static goal order)"
      $ \test reorderGoals indepGoals ->
        let r1 = solve' (EnableBackjumping True) test
            r2 = solve' (EnableBackjumping False) test
            solve' enableBj =
              solve
                enableBj
                (FineGrainedConflicts False)
                reorderGoals
                (CountConflicts False)
                indepGoals
                PreferInstalledOrLatest
                Nothing
         in counterexample (showResults r1 r2) $
              noneReachedBackjumpLimit [r1, r2] ==>
                resultPlan r1 === resultPlan r2
  , testPropertyWithSeed
      "fine-grained conflicts does not affect the result (with static goal order)"
      $ \test reorderGoals indepGoals ->
        let r1 = solve' (FineGrainedConflicts True) test
            r2 = solve' (FineGrainedConflicts False) test
            solve' fineGrainedConflicts =
              solve
                (EnableBackjumping True)
                fineGrainedConflicts
                reorderGoals
                (CountConflicts False)
                indepGoals
                PreferInstalledOrLatest
                Nothing
         in counterexample (showResults r1 r2) $
              noneReachedBackjumpLimit [r1, r2] ==>
                resultPlan r1 === resultPlan r2
  , -- The reference oracle is a plain search written from the Package
    -- Calculus definition of a resolution (see the Oracle module). It has no
    -- heuristics, so it is compared with the solver in both directions.
    testPropertyWithSeed "solver agrees with the reference oracle on solvability" $
      \test reorderGoals indepGoals ->
        let r = solveWith reorderGoals indepGoals PreferInstalledOrLatest test
            v = Oracle.verdict (oracleResolve indepGoals test)
         in counterexample (showResult 1 r ++ "oracle: " ++ show v) $
              classify (v == Oracle.IsUnknown) "oracle out of fuel" $
                classify (v == Oracle.IsSolvable) "solvable" $
                  classify (hasDep isSubLibDep test) "sub-library dependency" $
                    classify (hasDep isBuildToolDep test) "build-tool dependency" $
                      classify (hasCyclicNames test) "cycle among package names" $
                        classify (hasDep isPkgConfigDep test) "pkg-config dependency" $
                          classify (hasDep isCompilerDep test) "extension or language dependency" $
                            classify (any isFlagConstraint (testConstraints test)) "flag constraint" $
                              classify (any (not . isAnyQualifier . constraintScope) (testConstraints test)) "scoped constraint" $
                                classify (hasManualFlag test) "manual flag" $
                                  classify (testAllowBootLibInstalls test) "boot library installs allowed" $
                                    classify (not (testSolveExecutables test)) "executables not solved" $
                                      classify (hasSetupDeps test) "setup dependencies" $
                                        (v /= Oracle.IsUnknown && noneReachedBackjumpLimit [r]) ==>
                                          isRight (resultPlan r) === (v == Oracle.IsSolvable)
  , testPropertyWithSeed "solver plan is a valid resolution under the oracle's validity check" $
      \test reorderGoals indepGoals prefVersion ->
        let r = solveWith reorderGoals indepGoals prefVersion test
         in case resultResolved r of
              Left _ -> property True
              Right plan ->
                counterexample (showResult 1 r ++ "resolved: " ++ show plan) $
                  oracleCheck indepGoals test plan === []
  , testPropertyWithSeed "oracle solution passes oracle validity check" $
      \test indepGoals ->
        case oracleResolve indepGoals test of
          Oracle.Solvable res ->
            let plan = Oracle.toResolved (testEnv test) res
             in counterexample ("resolved: " ++ show plan) $
                  oracleCheck indepGoals test plan === []
          _ -> property True
  , testCase "hand-written oracle cases agree with the solver" $
      for_ Oracle.solverCases $ \c ->
        let test =
              SolverTest
                { testDb = TestDb (Oracle.scDb c)
                , testTargets = map PN (Oracle.scTargets c)
                , testConstraints = Oracle.scConstraints c
                , testPreferences = []
                , testPkgConfigDb = Oracle.envPkgConfig (Oracle.scEnv c)
                , testExtensions = Oracle.envExtensions (Oracle.scEnv c)
                , testLanguages = Oracle.envLanguages (Oracle.scEnv c)
                , testAllowBootLibInstalls = Oracle.envAllowBootLibInstalls (Oracle.scEnv c)
                , testSolveExecutables = Oracle.envSolveExecutables (Oracle.scEnv c)
                }
            r =
              solveWith
                (ReorderGoals False)
                (IndependentGoals (Oracle.scIndependent c))
                PreferInstalledOrLatest
                test
            solvable = case resultPlan r of
              Right _ -> Oracle.IsSolvable
              Left _ -> Oracle.IsUnsolvable
         in (Oracle.scName c, solvable) @?= (Oracle.scName c, Oracle.scVerdict c)
  ]
  where
    solveWith reorderGoals indepGoals prefVersion =
      solve
        (EnableBackjumping True)
        (FineGrainedConflicts True)
        reorderGoals
        (CountConflicts True)
        indepGoals
        prefVersion
        Nothing

    oracleFuel :: Int
    oracleFuel = 100000

    testEnv test =
      Oracle.Env
        { Oracle.envPkgConfig = testPkgConfigDb test
        , Oracle.envExtensions = testExtensions test
        , Oracle.envLanguages = testLanguages test
        , Oracle.envAllowBootLibInstalls = testAllowBootLibInstalls test
        , Oracle.envSolveExecutables = testSolveExecutables test
        }

    -- Whether any source package in the test has a dependency of the given
    -- kind, looking inside flag branches too.
    hasDep :: (ExampleDependency -> Bool) -> SolverTest -> Bool
    hasDep p test =
      or
        [ any (go . snd) (CD.toList (exAvDeps av))
        | Right av <- unTestDb (testDb test)
        ]
      where
        go deps = any goDep (depsExampleDependencies deps)
        goDep dep@(ExFlagged _ t e) = p dep || go t || go e
        goDep dep = p dep

    isSubLibDep ExSubLibAny{} = True
    isSubLibDep ExSubLibFix{} = True
    isSubLibDep _ = False

    isBuildToolDep ExBuildToolAny{} = True
    isBuildToolDep ExBuildToolFix{} = True
    isBuildToolDep _ = False

    isPkgConfigDep ExPkg{} = True
    isPkgConfigDep _ = False

    isCompilerDep ExExt{} = True
    isCompilerDep ExLang{} = True
    isCompilerDep _ = False

    isFlagConstraint ExFlagConstraint{} = True
    isFlagConstraint _ = False

    constraintScope (ExVersionConstraint scope _) = scope
    constraintScope (ExFlagConstraint scope _ _) = scope
    constraintScope (ExStanzaConstraint scope _) = scope

    isAnyQualifier ScopeAnyQualifier{} = True
    isAnyQualifier _ = False

    hasManualFlag test =
      or
        [ exFlagType flag == Manual
        | Right av <- unTestDb (testDb test)
        , flag <- exAvFlags av
        ]

    -- Whether the package names, with an edge for every dependency of any
    -- kind under any flag assignment, contain a cycle. A rough indicator that
    -- the solver's cycle detection is exercised.
    hasCyclicNames test =
      not . null $
        [ ()
        | CyclicSCC _ <-
            stronglyConnComp
              [ (pn, pn, ordNub (concatMap (depNames . snd) (CD.toList (exAvDeps av))))
              | Right av <- unTestDb (testDb test)
              , let pn = exAvName av
              ]
        ]
      where
        depNames deps = concatMap depName (depsExampleDependencies deps)
        depName (ExFlagged _ t e) = depNames t ++ depNames e
        depName (ExAny pn) = [pn]
        depName (ExFix pn _) = [pn]
        depName (ExSubLibAny pn _) = [pn]
        depName (ExSubLibFix pn _ _) = [pn]
        depName (ExBuildToolAny pn _) = [pn]
        depName (ExBuildToolFix pn _ _) = [pn]
        depName _ = []

    hasSetupDeps test =
      or
        [ not (null (depsExampleDependencies (CD.setupDeps (exAvDeps av))))
        | Right av <- unTestDb (testDb test)
        ]

    oracleResolve (IndependentGoals indep) test =
      Oracle.resolve
        oracleFuel
        (testEnv test)
        indep
        (testConstraints test)
        (unTestDb (testDb test))
        (map unPN (testTargets test))

    oracleCheck (IndependentGoals indep) test =
      Oracle.checkResolution
        (testEnv test)
        (testConstraints test)
        indep
        (unTestDb (testDb test))
        (map unPN (testTargets test))

    noneReachedBackjumpLimit :: [Result] -> Bool
    noneReachedBackjumpLimit =
      not . any (\r -> resultPlan r == Left BackjumpLimitReached)

    showResults :: Result -> Result -> String
    showResults r1 r2 = showResult 1 r1 ++ showResult 2 r2

    showResult :: Int -> Result -> String
    showResult n result =
      unlines $
        ["", "Run " ++ show n ++ ":"]
          ++ resultLog result
          ++ ["result: " ++ show (resultPlan result)]

    implies :: Bool -> Bool -> Bool
    implies x y = not x || y

    isRight :: Either a b -> Bool
    isRight (Right _) = True
    isRight _ = False

newtype VarOrdering = VarOrdering
  { unVarOrdering :: Variable P.QPN -> Variable P.QPN -> Ordering
  }

instance Arbitrary VarOrdering where
  arbitrary = VarOrdering <$> arbitraryCompare

solve
  :: EnableBackjumping
  -> FineGrainedConflicts
  -> ReorderGoals
  -> CountConflicts
  -> IndependentGoals
  -> PreferVersion
  -> Maybe VarOrdering
  -> SolverTest
  -> Result
solve enableBj fineGrainedConflicts reorder countConflicts indep prefOldest goalOrder test =
  let (lg, result) =
        runProgress $
          exResolve
            (unTestDb (testDb test))
            (testExtensions test)
            (testLanguages test)
            Nothing
            (toPkgConfigDb <$> testPkgConfigDb test)
            (map unPN (testTargets test))
            -- The backjump limit prevents individual tests from using
            -- too much time and memory.
            (Just defaultMaxBackjumps)
            countConflicts
            fineGrainedConflicts
            (MinimizeConflictSet False)
            indep
            prefOldest
            reorder
            (AllowBootLibInstalls (testAllowBootLibInstalls test))
            OnlyConstrainedNone
            enableBj
            (SolveExecutables (testSolveExecutables test))
            (unVarOrdering <$> goalOrder)
            (testConstraints test)
            (testPreferences test)
            (mkVerbosity defaultVerbosityHandles normal)
            (EnableAllTests False)

      failure :: String -> Failure
      failure msg
        | "Backjump limit reached" `isInfixOf` msg = BackjumpLimitReached
        | otherwise = OtherFailure
   in Result
        { resultLog = lg
        , resultPlan =
            -- Force the result so that we check for internal errors when we check
            -- for success or failure. See D.C.Dependency.validateSolverResult.
            force $ either (Left . failure) (Right . extractInstallPlan) result
        , resultResolved = either (Left . failure) (Right . Oracle.fromSolverPlan) result
        }

-- | How to modify the order of the input targets.
data TargetOrder = SameOrder | ReverseOrder
  deriving (Show)

instance Arbitrary TargetOrder where
  arbitrary = elements [SameOrder, ReverseOrder]

  shrink SameOrder = []
  shrink ReverseOrder = [SameOrder]

data Result = Result
  { resultLog :: [String]
  , resultPlan :: Either Failure [(ExamplePkgName, ExamplePkgVersion)]
  , resultResolved :: Either Failure [Oracle.ResolvedPackage]
  -- ^ The same plan described for the reference oracle, including installed
  -- packages and flag assignments. Not forced.
  }

data Failure = BackjumpLimitReached | OtherFailure
  deriving (Eq, Generic, Show)

instance NFData Failure

-- | Package name.
newtype PN = PN {unPN :: String}
  deriving (Eq, Ord, Show)

instance Arbitrary PN where
  arbitrary = PN <$> elements ("base" : [[pn] | pn <- ['A' .. 'G']])

-- | Package version.
newtype PV = PV {unPV :: Int}
  deriving (Eq, Ord, Show)

instance Arbitrary PV where
  arbitrary = PV <$> elements [1 .. 10]

type TestPackage = Either ExampleInstalled ExampleAvailable

getName :: TestPackage -> PN
getName = PN . either exInstName exAvName

getVersion :: TestPackage -> PV
getVersion = PV . either exInstVersion exAvVersion

data SolverTest = SolverTest
  { testDb :: TestDb
  , testTargets :: [PN]
  , testConstraints :: [ExConstraint]
  , testPreferences :: [ExPreference]
  , testPkgConfigDb :: Maybe [(String, Maybe Int)]
  -- ^ The pkg-config database, or Nothing for no pkg-config.
  , testExtensions :: Maybe [Extension]
  -- ^ The extensions the compiler supports, or Nothing for unknown.
  , testLanguages :: Maybe [Language]
  -- ^ The languages the compiler supports, or Nothing for unknown.
  , testAllowBootLibInstalls :: Bool
  , testSolveExecutables :: Bool
  }

-- | Pretty-print the test when quickcheck calls 'show'.
instance Show SolverTest where
  show test =
    let str =
          "SolverTest {testDb = "
            ++ show (testDb test)
            ++ ", testTargets = "
            ++ show (testTargets test)
            ++ ", testConstraints = "
            ++ show (testConstraints test)
            ++ ", testPreferences = "
            ++ show (testPreferences test)
            ++ ", testPkgConfigDb = "
            ++ show (testPkgConfigDb test)
            ++ ", testExtensions = "
            ++ show (testExtensions test)
            ++ ", testLanguages = "
            ++ show (testLanguages test)
            ++ ", testAllowBootLibInstalls = "
            ++ show (testAllowBootLibInstalls test)
            ++ ", testSolveExecutables = "
            ++ show (testSolveExecutables test)
            ++ "}"
     in maybe str valToStr $ parseValue str

instance Arbitrary SolverTest where
  arbitrary = do
    db <- arbitrary
    let pkgVersions = ordNub $ map (getName &&& getVersion) (unTestDb db)
        pkgs = ordNub $ map fst pkgVersions
    Positive n <- arbitrary
    targets <- randomSubset n pkgs
    constraints <- case unTestDb db of
      [] -> return []
      dbPkgs -> boundedListOf 2 $ arbitraryConstraint dbPkgs
    prefs <- case pkgVersions of
      [] -> return []
      _ -> boundedListOf 3 $ arbitraryPreference pkgVersions
    pkgConfigDb <- arbitraryPkgConfigDb
    exts <- arbitraryCompilerList extensionPool
    langs <- arbitraryCompilerList languagePool
    allowBootLibInstalls <- frequency [(3, return False), (1, return True)]
    solveExecutables <- frequency [(3, return True), (1, return False)]
    return (SolverTest db targets constraints prefs pkgConfigDb exts langs allowBootLibInstalls solveExecutables)

  shrink test =
    [test{testDb = db} | db <- shrink (testDb test)]
      ++ [test{testTargets = targets} | targets <- shrink (testTargets test)]
      ++ [test{testConstraints = cs} | cs <- shrink (testConstraints test)]
      ++ [test{testPreferences = prefs} | prefs <- shrink (testPreferences test)]
      ++ [test{testPkgConfigDb = db} | db <- shrinkPkgConfigDb (testPkgConfigDb test)]
      ++ [test{testExtensions = exts} | exts <- shrinkCompilerList (testExtensions test)]
      ++ [test{testLanguages = langs} | langs <- shrinkCompilerList (testLanguages test)]
      ++ [test{testAllowBootLibInstalls = False} | testAllowBootLibInstalls test]
      ++ [test{testSolveExecutables = True} | not (testSolveExecutables test)]

-- | The extensions and languages that dependencies and compilers draw from.
extensionPool :: [Extension]
extensionPool =
  [EnableExtension CPP, EnableExtension RankNTypes, DisableExtension CPP, UnknownExtension "custom"]

languagePool :: [Language]
languagePool = [Haskell98, Haskell2010, UnknownLanguage "Haskell3000"]

-- | Usually a compiler supporting some of the pool (languages usually include
-- Haskell98, which every component needs by default); occasionally a compiler
-- whose support is unknown.
arbitraryCompilerList :: [a] -> Gen (Maybe [a])
arbitraryCompilerList pool =
  frequency
    [ (1, return Nothing)
    , (1, Just <$> sublistOf pool)
    , (3, Just <$> ((take 1 pool ++) <$> sublistOf (drop 1 pool)))
    ]

shrinkCompilerList :: Maybe [a] -> [Maybe [a]]
shrinkCompilerList Nothing = []
shrinkCompilerList (Just xs) = Nothing : map Just (shrinkList shrinkNothing xs)

-- | The pkg-config package names that dependencies and databases draw from.
pkgConfigNames :: [String]
pkgConfigNames = ["pc-A", "pc-B", "pc-C"]

-- | Usually a database listing some of the names, each with a version or an
-- unknown version; occasionally no pkg-config at all.
arbitraryPkgConfigDb :: Gen (Maybe [(String, Maybe Int)])
arbitraryPkgConfigDb =
  frequency
    [ (1, return Nothing)
    , (4, Just <$> (sublistOf pkgConfigNames >>= traverse withVersion))
    ]
  where
    withVersion name = (,) name <$> frequency [(1, return Nothing), (3, Just <$> elements [1 .. 3])]

shrinkPkgConfigDb :: Maybe [(String, Maybe Int)] -> [Maybe [(String, Maybe Int)]]
shrinkPkgConfigDb Nothing = []
shrinkPkgConfigDb (Just db) = Nothing : map Just (shrinkList shrinkNothing db)

toPkgConfigDb :: [(String, Maybe Int)] -> PkgConfigDb
toPkgConfigDb = pkgConfigDbFromList . map (\(name, version) -> (name, maybe "" show version))

-- | Collection of source and installed packages.
newtype TestDb = TestDb {unTestDb :: ExampleDb}
  deriving (Show)

instance Arbitrary TestDb where
  arbitrary = do
    -- Packages are generated by name group, in order. Dependencies on
    -- packages in earlier groups can name their components; dependencies on
    -- packages in later groups can only name the package, and are how
    -- dependency cycles arise. A package never depends on its own name.
    groupedPkgs <-
      shuffle . groupBy ((==) `on` fst) . nub . sort
        =<< boundedListOf 10 arbitrary
    db <- foldM nextPkgs (TestDb []) (zip groupedPkgs (drop 1 (L.tails groupedPkgs)))
    TestDb <$> shuffle (unTestDb db)
    where
      nextPkgs :: TestDb -> ([(PN, PV)], [[(PN, PV)]]) -> Gen TestDb
      nextPkgs db (pkgs, later) = TestDb . (++ unTestDb db) <$> traverse (nextPkg db (concat later)) pkgs

      nextPkg :: TestDb -> [(PN, PV)] -> (PN, PV) -> Gen TestPackage
      nextPkg db later (pn, v) = do
        installed <- arbitrary
        if installed
          then Left <$> arbitraryExInst pn v (lefts $ unTestDb db)
          else Right <$> arbitraryExAv pn v db later

  shrink (TestDb pkgs) = map TestDb $ shrink pkgs

arbitraryExAv :: PN -> PV -> TestDb -> [(PN, PV)] -> Gen ExampleAvailable
arbitraryExAv pn v db later = do
  cds <- arbitraryComponentDeps pn db later
  flags <- arbitraryFlagDeclarations cds
  return (ExAv (unPN pn) (unPV v) cds flags)

-- | Declare some of the flags a package uses, each as manual or automatic
-- with a random default. Undeclared flags are automatic with default True.
arbitraryFlagDeclarations :: ComponentDeps Dependencies -> Gen [ExFlag]
arbitraryFlagDeclarations cds = do
  names <- sublistOf (usedFlagNames cds)
  traverse (\name -> ExFlag name <$> arbitrary <*> elements [Manual, Automatic]) names

-- | The flags mentioned anywhere in a package's dependencies.
usedFlagNames :: ComponentDeps Dependencies -> [ExampleFlagName]
usedFlagNames = ordNub . concatMap (go . snd) . CD.toList
  where
    go deps = concatMap goDep (depsExampleDependencies deps)
    goDep (ExFlagged f t e) = f : go t ++ go e
    goDep _ = []

arbitraryExInst :: PN -> PV -> [ExampleInstalled] -> Gen ExampleInstalled
arbitraryExInst pn v pkgs = do
  pkgHash <- vectorOf 10 $ elements $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']
  numDeps <- min 3 <$> arbitrary
  deps <- randomSubset numDeps pkgs
  return $ ExInst (unPN pn) (unPV v) pkgHash (map exInstHash deps)

arbitraryComponentDeps :: PN -> TestDb -> [(PN, PV)] -> Gen (ComponentDeps Dependencies)
arbitraryComponentDeps _ (TestDb []) [] = return $ CD.fromLibraryDeps (dependencies [])
arbitraryComponentDeps pn db later = do
  -- dedupComponentNames removes components with duplicate names, for example,
  -- 'ComponentExe x' and 'ComponentTest x', and then CD.fromList combines
  -- duplicate unnamed components.
  cds <-
    CD.fromList . dedupComponentNames . filter (isValid . fst)
      <$> boundedListOf 5 (arbitraryComponentDep db later)
  let ownExes = [name | (ComponentExe name, _) <- CD.toList cds]
      cds' = fmap (dropInternalBuildTools ownExes) cds
  return $
    if isCompleteComponentDeps cds'
      then cds'
      else -- Add a library if the ComponentDeps isn't complete.
        CD.fromLibraryDeps (dependencies []) <> cds'
  where
    isValid :: Component -> Bool
    isValid (ComponentSubLib name) = name /= mkUnqualComponentName (unPN pn)
    isValid _ = True

    dedupComponentNames =
      nubBy ((\x y -> isJust x && isJust y && x == y) `on` componentName . fst)

    componentName :: Component -> Maybe UnqualComponentName
    componentName ComponentLib = Nothing
    componentName ComponentSetup = Nothing
    componentName (ComponentSubLib n) = Just n
    componentName (ComponentFLib n) = Just n
    componentName (ComponentExe n) = Just n
    componentName (ComponentTest n) = Just n
    componentName (ComponentBench n) = Just n

-- | Remove build-tool dependencies on executables with the same name as one
-- of the package's own executables. Cabal's package checks treat those as
-- internal and reject a version range that excludes the package itself.
dropInternalBuildTools :: [UnqualComponentName] -> Dependencies -> Dependencies
dropInternalBuildTools ownExes deps =
  deps{depsExampleDependencies = mapMaybe go (depsExampleDependencies deps)}
  where
    go (ExFlagged f t e) = Just (ExFlagged f (dropInternalBuildTools ownExes t) (dropInternalBuildTools ownExes e))
    go dep@(ExBuildToolAny _ exe)
      | mkUnqualComponentName exe `elem` ownExes = Nothing
      | otherwise = Just dep
    go dep@(ExBuildToolFix _ exe _)
      | mkUnqualComponentName exe `elem` ownExes = Nothing
      | otherwise = Just dep
    go dep = Just dep

-- | Returns true if the ComponentDeps forms a complete package, i.e., it
-- contains a library, exe, test, or benchmark.
isCompleteComponentDeps :: ComponentDeps a -> Bool
isCompleteComponentDeps = any (completesPkg . fst) . CD.toList
  where
    completesPkg ComponentLib = True
    completesPkg (ComponentExe _) = True
    completesPkg (ComponentTest _) = True
    completesPkg (ComponentBench _) = True
    completesPkg (ComponentSubLib _) = False
    completesPkg (ComponentFLib _) = False
    completesPkg ComponentSetup = False

arbitraryComponentDep :: TestDb -> [(PN, PV)] -> Gen (ComponentDep Dependencies)
arbitraryComponentDep db later = do
  comp <- arbitrary
  deps <- case comp of
    ComponentSetup -> smallListOf (arbitraryExDep db later SetupDep)
    _ -> boundedListOf 5 (arbitraryExDep db later NonSetupDep)
  visibility <- case comp of
    ComponentSubLib _ -> arbitrary
    _ -> return LibraryVisibilityPublic
  return
    ( comp
    , Dependencies
        { depsExampleDependencies = deps
        , -- TODO: Test different values for buildability.
          depsVisibility = visibility
        , depsIsBuildable = True
        }
    )

-- | Location of an 'ExampleDependency'. It determines which values are valid.
data ExDepLocation = SetupDep | NonSetupDep

arbitraryExDep :: TestDb -> [(PN, PV)] -> ExDepLocation -> Gen ExampleDependency
arbitraryExDep db@(TestDb pkgs) later level =
  let flag =
        ExFlagged
          <$> arbitraryFlagName
          <*> arbitraryDeps db later
          <*> arbitraryDeps db later
      -- Dependencies on packages that are generated later, which may close
      -- a dependency cycle.
      laterDeps =
        let laterNotBase = ordNub [pn | (pn, _) <- later, pn /= PN "base"]
         in [ExAny . unPN <$> elements laterNotBase | not (null laterNotBase)]
              ++ [(\(PN pn, PV v) -> ExFix pn v) <$> elements later | not (null later)]
      other =
        -- Package checks require dependencies on "base" to have bounds.
        let notBase = filter ((/= PN "base") . getName) pkgs
            subLibs =
              [ (getName pkg, unUnqualComponentName name, getVersion pkg)
              | pkg@(Right av) <- pkgs
              , (ComponentSubLib name, _) <- CD.toList (exAvDeps av)
              ]
         in [ExAny . unPN <$> elements (map getName notBase) | not (null notBase)]
              ++ [
                 -- existing version
                 let fixed pkg = ExFix (unPN $ getName pkg) (unPV $ getVersion pkg)
                  in fixed <$> elements pkgs
                 | not (null pkgs)
                 ]
              ++ [ -- random version of an existing package
                 ExFix . unPN . getName <$> elements pkgs <*> (unPV <$> arbitrary)
                 | not (null pkgs)
                 ]
              ++ [ (\(PN pn, lib, _) -> ExSubLibAny pn lib) <$> elements subLibs
                 | not (null subLibs)
                 ]
              ++ [ (\(PN pn, lib, PV v) -> ExSubLibFix pn lib v) <$> elements subLibs
                 | not (null subLibs)
                 ]
      -- custom-setup only supports library dependencies.
      pkgConfig = [ExPkg <$> ((,) <$> elements pkgConfigNames <*> elements [1 .. 3])]
      compiler = [ExExt <$> elements extensionPool, ExLang <$> elements languagePool]
      -- custom-setup only supports library dependencies.
      exes =
        [ (getName pkg, unUnqualComponentName name, getVersion pkg)
        | pkg@(Right av) <- pkgs
        , (ComponentExe name, _) <- CD.toList (exAvDeps av)
        ]
      buildTools =
        [ (\(PN pn, exe, _) -> ExBuildToolAny pn exe) <$> elements exes
        | not (null exes)
        ]
          ++ [ (\(PN pn, exe, PV v) -> ExBuildToolFix pn exe v) <$> elements exes
             | not (null exes)
             ]
   in oneof $
        case level of
          NonSetupDep -> flag : other ++ buildTools ++ pkgConfig ++ compiler ++ laterDeps
          SetupDep -> other ++ laterDeps

arbitraryDeps :: TestDb -> [(PN, PV)] -> Gen Dependencies
arbitraryDeps db later =
  frequency
    [ (1, return unbuildableDependencies)
    , (20, dependencies <$> smallListOf (arbitraryExDep db later NonSetupDep))
    ]

arbitraryFlagName :: Gen String
arbitraryFlagName = (: []) <$> elements ['A' .. 'E']

arbitraryConstraint :: [TestPackage] -> Gen ExConstraint
arbitraryConstraint pkgs = do
  pkg <- elements pkgs
  scope <- arbitraryScope pkgs (getName pkg)
  let flags = either (const []) (usedFlagNames . exAvDeps) pkg
  oneof $
    [ ExVersionConstraint scope <$> arbitraryVersionRange (getVersion pkg)
    , ExStanzaConstraint scope <$> sublistOf [TestStanzas, BenchStanzas]
    ]
      ++ [ExFlagConstraint scope <$> elements flags <*> arbitrary | not (null flags)]

-- | A scope for a constraint on the named package: usually any qualifier,
-- otherwise one of the target, top-level, setup, build-tool or any-setup
-- scopes. The setup and build-tool scopes name some package in the database,
-- whether or not that scope ever arises.
arbitraryScope :: [TestPackage] -> PN -> Gen ConstraintScope
arbitraryScope pkgs (PN pn) =
  frequency
    [ (5, return (ScopeAnyQualifier name))
    , (1, return (ScopeTarget name))
    , (1, return (ScopeQualified P.QualToplevel name))
    , (1, (\p -> ScopeQualified (P.QualSetup p) name) <$> someName)
    , (1, (\p -> ScopeQualified (P.QualExe p name) name) <$> someName)
    , (1, return (ScopeAnySetupQualifier name))
    ]
  where
    name = mkPackageName pn
    someName = mkPackageName . unPN . getName <$> elements pkgs

arbitraryPreference :: [(PN, PV)] -> Gen ExPreference
arbitraryPreference pkgs = do
  (PN pn, v) <- elements pkgs
  oneof
    [ ExStanzaPref pn <$> sublistOf [TestStanzas, BenchStanzas]
    , ExPkgPref pn <$> arbitraryVersionRange v
    ]

arbitraryVersionRange :: PV -> Gen VersionRange
arbitraryVersionRange (PV v) =
  let version = mkSimpleVersion v
   in elements
        [ thisVersion version
        , notThisVersion version
        , earlierVersion version
        , orLaterVersion version
        , noVersion
        ]

instance Arbitrary ReorderGoals where
  arbitrary = ReorderGoals <$> arbitrary

  shrink (ReorderGoals reorder) = [ReorderGoals False | reorder]

instance Arbitrary IndependentGoals where
  arbitrary = IndependentGoals <$> arbitrary

  shrink (IndependentGoals indep) = [IndependentGoals False | indep]

instance Arbitrary PreferVersion where
  arbitrary =
    oneof
      [ pure PreferOldest
      , pure PreferLatest
      , pure PreferInstalledOrLatest
      ]

instance Arbitrary Component where
  arbitrary =
    oneof
      [ return ComponentLib
      , ComponentSubLib <$> arbitraryUQN
      , ComponentExe <$> arbitraryUQN
      , ComponentFLib <$> arbitraryUQN
      , ComponentTest <$> arbitraryUQN
      , ComponentBench <$> arbitraryUQN
      , return ComponentSetup
      ]

  shrink ComponentLib = []
  shrink _ = [ComponentLib]

-- The "component-" prefix prevents component names and build-depends
-- dependency names from overlapping.
-- TODO: Remove the prefix once the QuickCheck tests support dependencies on
-- internal libraries.
arbitraryUQN :: Gen UnqualComponentName
arbitraryUQN =
  mkUnqualComponentName . (\c -> "component-" ++ [c]) <$> elements "ABC"

instance Arbitrary ExampleInstalled where
  arbitrary = error "arbitrary not implemented: ExampleInstalled"

  shrink ei =
    [ ei{exInstBuildAgainst = deps}
    | deps <- shrinkList shrinkNothing (exInstBuildAgainst ei)
    ]

instance Arbitrary ExampleAvailable where
  arbitrary = error "arbitrary not implemented: ExampleAvailable"

  shrink ea =
    [ea{exAvDeps = deps} | deps <- shrink (exAvDeps ea)]
      ++ [ea{exAvFlags = flags} | flags <- shrinkList shrinkNothing (exAvFlags ea)]

instance (Arbitrary a, Monoid a) => Arbitrary (ComponentDeps a) where
  arbitrary = error "arbitrary not implemented: ComponentDeps"

  shrink = filter isCompleteComponentDeps . map CD.fromList . shrink . CD.toList

instance Arbitrary ExampleDependency where
  arbitrary = error "arbitrary not implemented: ExampleDependency"

  shrink (ExAny _) = []
  shrink (ExFix "base" _) = [] -- preserve bounds on base
  shrink (ExFix pn _) = [ExAny pn]
  shrink (ExSubLibAny _ _) = []
  shrink (ExSubLibFix pn lib _) = [ExSubLibAny pn lib]
  shrink (ExBuildToolAny _ _) = []
  shrink (ExBuildToolFix pn exe _) = [ExBuildToolAny pn exe]
  shrink (ExPkg _) = []
  shrink (ExExt _) = []
  shrink (ExLang _) = []
  shrink (ExFlagged flag th el) =
    depsExampleDependencies th
      ++ depsExampleDependencies el
      ++ [ExFlagged flag th' el | th' <- shrink th]
      ++ [ExFlagged flag th el' | el' <- shrink el]
  shrink dep = error $ "Dependency not handled: " ++ show dep

instance Arbitrary Dependencies where
  arbitrary = error "arbitrary not implemented: Dependencies"

  shrink deps =
    [deps{depsVisibility = v} | v <- shrink $ depsVisibility deps]
      ++ [deps{depsIsBuildable = b} | b <- shrink $ depsIsBuildable deps]
      ++ [deps{depsExampleDependencies = ds} | ds <- shrink $ depsExampleDependencies deps]

instance Arbitrary ExConstraint where
  arbitrary = error "arbitrary not implemented: ExConstraint"

  shrink (ExStanzaConstraint scope stanzas) =
    [ExStanzaConstraint scope stanzas' | stanzas' <- shrink stanzas]
  shrink (ExVersionConstraint scope vr) =
    [ExVersionConstraint scope vr' | vr' <- shrink vr]
  shrink _ = []

instance Arbitrary ExPreference where
  arbitrary = error "arbitrary not implemented: ExPreference"

  shrink (ExStanzaPref pn stanzas) =
    [ExStanzaPref pn stanzas' | stanzas' <- shrink stanzas]
  shrink (ExPkgPref pn vr) = [ExPkgPref pn vr' | vr' <- shrink vr]

instance Arbitrary OptionalStanza where
  arbitrary = error "arbitrary not implemented: OptionalStanza"

  shrink BenchStanzas = [TestStanzas]
  shrink TestStanzas = []

instance ArbitraryOrd pn => ArbitraryOrd (Variable pn)
instance ArbitraryOrd a => ArbitraryOrd (P.Qualified a)
instance ArbitraryOrd P.PackagePath
instance ArbitraryOrd P.Qualifier
instance ArbitraryOrd P.Namespace
instance ArbitraryOrd OptionalStanza
instance ArbitraryOrd FlagName
instance ArbitraryOrd PackageName
instance ArbitraryOrd ShortText where
  arbitraryCompare = do
    strc <- arbitraryCompare
    pure $ \l r -> strc (fromShortText l) (fromShortText r)

deriving instance Generic (Variable pn)
deriving instance Generic (P.Qualified a)
deriving instance Generic P.PackagePath
deriving instance Generic P.Namespace
deriving instance Generic P.Qualifier

randomSubset :: Int -> [a] -> Gen [a]
randomSubset n xs = take n <$> shuffle xs

boundedListOf :: Int -> Gen a -> Gen [a]
boundedListOf n gen = take n <$> listOf gen

-- | Generates lists with average length less than 1.
smallListOf :: Gen a -> Gen [a]
smallListOf gen =
  frequency
    [ (fr, vectorOf n gen)
    | (fr, n) <- [(3, 0), (5, 1), (2, 2)]
    ]
