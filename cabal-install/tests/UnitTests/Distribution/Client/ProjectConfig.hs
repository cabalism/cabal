{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module UnitTests.Distribution.Client.ProjectConfig (tests) where

import Control.Monad
import Data.Either (isRight)
import Data.Foldable (for_)
import Data.List (isPrefixOf)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import System.Directory (canonicalizePath, withCurrentDirectory)
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)

import Distribution.Deprecated.ParseUtils

import Distribution.Package
import Distribution.PackageDescription
import Distribution.Simple.Utils (toUTF8BS)
import Distribution.Types.PackageVersionConstraint

import Distribution.Parsec
import Distribution.Pretty

import Distribution.Client.DistDirLayout (defaultProjectFile)
import Distribution.Client.Targets
import Distribution.Client.Types
import Distribution.Client.Types.SourceRepo
import Distribution.Verbosity

import Distribution.Solver.Types.PackageConstraint

import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectConfig.Legacy

import UnitTests.Distribution.Client.ArbitraryInstances
import UnitTests.Distribution.Client.TreeDiffInstances ()

import Data.TreeDiff.Class
import Data.TreeDiff.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: [TestTree]
tests =
  [ testGroup "ProjectConfig <-> LegacyProjectConfig round trip" $
      [ testProperty "packages" prop_roundtrip_legacytypes_packages
      , testProperty "buildonly" prop_roundtrip_legacytypes_buildonly
      , testProperty "specific" prop_roundtrip_legacytypes_specific
      ]
        ++ [ testProperty "shared" prop_roundtrip_legacytypes_shared
           , testProperty "local" prop_roundtrip_legacytypes_local
           , testProperty "all" prop_roundtrip_legacytypes_all
           ]
  , testGroup
      "individual parser tests"
      [ testProperty "RelaxedDep" prop_roundtrip_printparse_RelaxedDep
      , testProperty "RelaxDeps" prop_roundtrip_printparse_RelaxDeps
      , testProperty "RelaxDeps'" prop_roundtrip_printparse_RelaxDeps'
      ]
  , testGroup
      "ProjectConfig printing/parsing round trip"
      [ testProperty "packages" prop_roundtrip_printparse_packages
      , testProperty "buildonly" prop_roundtrip_printparse_buildonly
      , testProperty "shared" prop_roundtrip_printparse_shared
      , testProperty "local" prop_roundtrip_printparse_local
      , testProperty "specific" prop_roundtrip_printparse_specific
      , testProperty "all" prop_roundtrip_printparse_all
      ]
  , testGetProjectRootUsability
  , testFindProjectRoot
  ]

testGetProjectRootUsability :: TestTree
testGetProjectRootUsability =
  testGroup
    "getProjectRootUsability"
    [ test "relative path" file ProjectRootUsabilityPresentAndUsable
    , test "absolute path" absFile ProjectRootUsabilityPresentAndUsable
    , test "symbolic link" fileSymlink ProjectRootUsabilityPresentAndUsable
    , test "file not present" fileNotPresent ProjectRootUsabilityNotPresent
    , test "directory" brokenDirCabalProject ProjectRootUsabilityPresentAndUnusable
    , test "broken symbolic link" fileSymlinkBroken ProjectRootUsabilityPresentAndUnusable
    ]
  where
    dir = fixturesDir </> "project-root"
    file = defaultProjectFile
    absFile = dir </> file
    fileNotPresent = file <.> "not-present"
    fileSymlink = file <.> "symlink"
    fileSymlinkBroken = fileSymlink <.> "broken"
    brokenDirCabalProject = "cabal" <.> "project" <.> "dir" <.> "broken"
    test name fileName expectedState =
      testCase name $
        withCurrentDirectory dir $
          getProjectRootUsability fileName
            >>= (@?= expectedState)

testFindProjectRoot :: TestTree
testFindProjectRoot =
  testGroup
    "findProjectRoot"
    [ test "defaults" (cd dir) Nothing Nothing (succeeds dir file)
    , test "defaults in lib" (cd libDir) Nothing Nothing (succeeds dir file)
    , test "explicit file" (cd dir) Nothing (Just file) (succeeds dir file)
    , test "explicit file in lib" (cd libDir) Nothing (Just file) (succeeds dir file)
    , test "other file" (cd dir) Nothing (Just fileOther) (succeeds dir fileOther)
    , test "other file in lib" (cd libDir) Nothing (Just fileOther) (succeeds dir fileOther)
    , test "symbolic link" (cd dir) Nothing (Just fileSymlink) (succeeds dir fileSymlink)
    , test "symbolic link in lib" (cd libDir) Nothing (Just fileSymlink) (succeeds dir fileSymlink)
    , test "broken symbolic link" (cd dir) Nothing (Just fileSymlinkBroken) (failsWith $ BadProjectRootFileBroken fileSymlinkBroken)
    , test "broken symbolic link in lib" (cd libDir) Nothing (Just fileSymlinkBroken) (failsWith $ BadProjectRootFileBroken fileSymlinkBroken)
    , -- Deprecated use-case
      test "absolute file" Nothing Nothing (Just absFile) (succeeds dir file)
    , test "nested file" (cd dir) Nothing (Just nixFile) (succeeds dir nixFile)
    , test "nested file in lib" (cd libDir) Nothing (Just nixFile) (succeeds dir nixFile)
    , test "explicit dir" Nothing (Just dir) Nothing (succeeds dir file)
    , test "explicit dir & file" Nothing (Just dir) (Just file) (succeeds dir file)
    , test "explicit dir & nested file" Nothing (Just dir) (Just nixFile) (succeeds dir nixFile)
    , test "explicit dir & nested other file" Nothing (Just dir) (Just nixOther) (succeeds dir nixOther)
    , test "explicit dir & absolute file" Nothing (Just dir) (Just absFile) (succeedsWith ProjectRootExplicitAbsolute dir absFile)
    ]
  where
    dir = fixturesDir </> "project-root"
    libDir = dir </> "lib"

    file = defaultProjectFile
    fileOther = file <.> "other"
    absFile = dir </> file

    nixFile = "nix" </> file
    nixOther = nixFile <.> "other"

    fileSymlink = file <.> "symlink"
    fileSymlinkBroken = fileSymlink <.> "broken"

    missing path = Just (path <.> "does_not_exist")

    test name wrap projectDir projectFile validate =
      testCaseSteps name $ \step -> fromMaybe id wrap $ do
        result <- findProjectRoot (mkVerbosity defaultVerbosityHandles silent) projectDir projectFile
        _ <- validate result

        when (isRight result) $ do
          for_ projectDir $ \path -> do
            step "missing project dir"
            fails =<< findProjectRoot (mkVerbosity defaultVerbosityHandles silent) (missing path) projectFile

          for_ projectFile $ \path -> do
            step "missing project file"
            fails =<< findProjectRoot (mkVerbosity defaultVerbosityHandles silent) projectDir (missing path)

    cd d = Just (withCurrentDirectory d)

    succeeds = succeedsWith ProjectRootExplicit

    succeedsWith mk projectDir projectFile result = case result of
      Left err -> assertFailure $ "Expected ProjectRoot, but found " <> show err
      Right pr -> pr @?= mk projectDir projectFile

    fails result = case result of
      Left _ -> pure ()
      Right x -> assertFailure $ "Expected an error, but found " <> show x

    failsWith expectedError result = case result of
      Left actualError ->
        if actualError == expectedError
          then pure ()
          else
            assertFailure $
              "Expected an error "
                <> show expectedError
                <> ", but found "
                <> show actualError
      Right x -> assertFailure $ "Expected an error, but found " <> show x

fixturesDir :: FilePath
fixturesDir =
  unsafePerformIO $
    canonicalizePath ("tests" </> "fixtures")
{-# NOINLINE fixturesDir #-}

------------------------------------------------
-- Round trip: conversion to/from legacy types
--

roundtrip :: (Eq a, ToExpr a, Show b) => (a -> b) -> (b -> a) -> a -> Property
roundtrip f f_inv x =
  counterexample (show y) $
    x `ediffEq` f_inv y -- no counterexample with y, as they not have ToExpr
  where
    y = f x

roundtrip_legacytypes :: ProjectConfig -> Property
roundtrip_legacytypes =
  roundtrip
    convertToLegacyProjectConfig
    convertLegacyProjectConfig

prop_roundtrip_legacytypes_all :: ProjectConfig -> Property
prop_roundtrip_legacytypes_all config =
  roundtrip_legacytypes
    config
      { projectConfigProvenance = mempty
      }

prop_roundtrip_legacytypes_packages :: ProjectConfig -> Property
prop_roundtrip_legacytypes_packages config =
  roundtrip_legacytypes
    config
      { projectConfigBuildOnly = mempty
      , projectConfigShared = mempty
      , projectConfigProvenance = mempty
      , projectConfigLocalPackages = mempty
      , projectConfigSpecificPackage = mempty
      }

prop_roundtrip_legacytypes_buildonly :: ProjectConfigBuildOnly -> Property
prop_roundtrip_legacytypes_buildonly config =
  roundtrip_legacytypes
    mempty{projectConfigBuildOnly = config}

prop_roundtrip_legacytypes_shared :: ProjectConfigShared -> Property
prop_roundtrip_legacytypes_shared config =
  roundtrip_legacytypes
    mempty{projectConfigShared = config}

prop_roundtrip_legacytypes_local :: PackageConfig -> Property
prop_roundtrip_legacytypes_local config =
  roundtrip_legacytypes
    mempty{projectConfigLocalPackages = config}

prop_roundtrip_legacytypes_specific :: Map PackageName PackageConfig -> Property
prop_roundtrip_legacytypes_specific config =
  roundtrip_legacytypes
    mempty{projectConfigSpecificPackage = MapMappend config}

--------------------------------------------
-- Round trip: printing and parsing config
--

roundtrip_printparse :: ProjectConfig -> Property
roundtrip_printparse config =
  case fmap convertLegacyProjectConfig (parseLegacyProjectConfig "unused" (toUTF8BS str)) of
    ParseOk _ x ->
      counterexample ("shown:\n" ++ str) $
        x `ediffEq` config{projectConfigProvenance = mempty}
    ParseFailed err -> counterexample ("shown:\n" ++ str ++ "\nERROR: " ++ show err) False
  where
    str :: String
    str = showLegacyProjectConfig (convertToLegacyProjectConfig config)

prop_roundtrip_printparse_all :: ProjectConfig -> Property
prop_roundtrip_printparse_all config =
  roundtrip_printparse
    config
      { projectConfigBuildOnly =
          hackProjectConfigBuildOnly (projectConfigBuildOnly config)
      , projectConfigShared =
          hackProjectConfigShared (projectConfigShared config)
      }

prop_roundtrip_printparse_packages
  :: [PackageLocationString]
  -> [PackageLocationString]
  -> [SourceRepoList]
  -> [PackageVersionConstraint]
  -> Property
prop_roundtrip_printparse_packages pkglocstrs1 pkglocstrs2 repos named =
  roundtrip_printparse
    mempty
      { projectPackages = map getPackageLocationString pkglocstrs1
      , projectPackagesOptional = map getPackageLocationString pkglocstrs2
      , projectPackagesRepo = repos
      , projectPackagesNamed = named
      }

prop_roundtrip_printparse_buildonly :: ProjectConfigBuildOnly -> Property
prop_roundtrip_printparse_buildonly config =
  roundtrip_printparse
    mempty
      { projectConfigBuildOnly = hackProjectConfigBuildOnly config
      }

hackProjectConfigBuildOnly :: ProjectConfigBuildOnly -> ProjectConfigBuildOnly
hackProjectConfigBuildOnly config =
  config
    { -- These fields are only command line transitory things, not
      -- something to be recorded persistently in a config file
      projectConfigOnlyDeps = mempty
    , projectConfigOnlyDownload = mempty
    , projectConfigDryRun = mempty
    }

prop_roundtrip_printparse_shared :: ProjectConfigShared -> Property
prop_roundtrip_printparse_shared config =
  roundtrip_printparse
    mempty
      { projectConfigShared = hackProjectConfigShared config
      }

hackProjectConfigShared :: ProjectConfigShared -> ProjectConfigShared
hackProjectConfigShared config =
  config
    { projectConfigProjectFile = mempty -- not present within project files
    , projectConfigProjectDir = mempty -- ditto
    , projectConfigConfigFile = mempty -- ditto
    , projectConfigConstraints =
        -- TODO: [required eventually] parse ambiguity in constraint
        -- "pkgname -any" as either any version or disabled flag "any".
        let ambiguous (UserConstraint _ (PackagePropertyFlags flags), _) =
              (not . null)
                [ () | (name, False) <- unFlagAssignment flags, "any" `isPrefixOf` unFlagName name
                ]
            ambiguous _ = False
         in filter (not . ambiguous) (projectConfigConstraints config)
    }

prop_roundtrip_printparse_local :: PackageConfig -> Property
prop_roundtrip_printparse_local config =
  roundtrip_printparse
    mempty
      { projectConfigLocalPackages = config
      }

prop_roundtrip_printparse_specific
  :: Map PackageName (NonMEmpty PackageConfig)
  -> Property
prop_roundtrip_printparse_specific config =
  roundtrip_printparse
    mempty
      { projectConfigSpecificPackage = MapMappend (fmap getNonMEmpty config)
      }

----------------------------
-- Individual Parser tests
--

prop_roundtrip_printparse_RelaxedDep :: RelaxedDep -> Property
prop_roundtrip_printparse_RelaxedDep rdep =
  counterexample (prettyShow rdep) $
    eitherParsec (prettyShow rdep) == Right rdep

prop_roundtrip_printparse_RelaxDeps :: RelaxDeps -> Property
prop_roundtrip_printparse_RelaxDeps rdep =
  counterexample (prettyShow rdep) $
    Right rdep `ediffEq` eitherParsec (prettyShow rdep)

prop_roundtrip_printparse_RelaxDeps' :: RelaxDeps -> Property
prop_roundtrip_printparse_RelaxDeps' rdep =
  counterexample rdep' $
    Right rdep `ediffEq` eitherParsec rdep'
  where
    rdep' = go (prettyShow rdep)

    -- replace 'all' tokens by '*'
    go :: String -> String
    go [] = []
    go "all" = "*"
    go ('a' : 'l' : 'l' : c : rest) | c `elem` ":," = '*' : go (c : rest)
    go rest =
      let (x, y) = break (`elem` ":,") rest
          (x', y') = span (`elem` ":,^") y
       in x ++ x' ++ go y'
