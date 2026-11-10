{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Functionality for invoking Haskell scripts with the correct
-- package database setup.
module Test.Cabal.Script
  ( ScriptEnv (..)
  , mkScriptEnv
  , writeScriptEnv
  , readScriptEnv
  , runnerGhcArgs
  , runnerCommand
  , runghc
  ) where

import Test.Cabal.Run

import Distribution.Backpack
import Distribution.Compat.Binary (decodeFileOrFail', encode)
import Distribution.InstalledPackageInfo qualified as IPI
import Distribution.Package (packageName)
import Distribution.Pretty (prettyShow)
import Distribution.Simple.BuildPaths (mkSharedLibName)
import Distribution.Simple.Compiler
import Distribution.Simple.Configure (configCompilerEx, getInstalledPackages)
import Distribution.Simple.PackageIndex (lookupUnitId)
import Distribution.Simple.Program
import Distribution.Simple.Program.Builtin
import Distribution.Simple.Program.GHC
import Distribution.Simple.Setup (pattern Flag)
import Distribution.Simple.Utils (writeFileAtomic)
import Distribution.System
import Distribution.Types.ModuleRenaming
import Distribution.Types.PackageName (mkPackageName)
import Distribution.Types.UnitId (UnitId, mkUnitId, unsafeMkDefUnitId)
import Distribution.Utils.NubList
import Distribution.Utils.Path
import Distribution.Verbosity
import Distribution.Version (Version, mkVersion')

import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid qualified as M
import System.Directory (doesFileExist, findExecutable)
import System.Exit (die)
import System.FilePath qualified as FilePath
import System.Info (fullCompilerVersion)

-- | The runner environment, which contains all of the important
-- parameters for invoking GHC.  Mostly subset of 'LocalBuildInfo'.
data ScriptEnv = ScriptEnv
  { runnerProgramDb :: ProgramDb
  , runnerPackageDbStack :: PackageDBStackCWD
  , runnerVerbosity :: VerbosityFlags
  , runnerPlatform :: Platform
  , runnerCompiler :: Compiler
  , runnerPackages :: [(OpenUnitId, ModuleRenaming)]
  , runnerWithSharedLib :: Bool
  }
  deriving (Show)

-- | Create the 'ScriptEnv' for running test scripts with the packages that
-- the @cabal-tests@ component of this package was built with.
--
-- The package databases and package ids are those that the @script-env-gen@
-- code generator found when @cabal-tests@ was built. They are of no use with
-- another GHC, so the GHC has to be the version that built this package, the
-- first of these that is:
--
-- * the given path, if any,
-- * @ghc-VERSION@ on the search path,
-- * @ghc@ on the search path.
mkScriptEnv
  :: Verbosity
  -> [FilePath]
  -- ^ The package databases that this package was built with
  -> [String]
  -- ^ The ids of the packages that test scripts can use
  -> Maybe FilePath
  -- ^ The path of the GHC that built this package, if not on the search path
  -> IO ScriptEnv
mkScriptEnv verbosity packageDbs packageIds mbGhcPath = do
  ghcPath <- findBuildGhc verbosity mbGhcPath
  (comp, platform, progDb0) <-
    configCompilerEx (Just GHC) (Just ghcPath) Nothing defaultProgramDb verbosity
  progDb <- configureAllKnownPrograms verbosity progDb0
  let dbStack = GlobalPackageDB : map SpecificPackageDB packageDbs
      unitIds = map mkUnitId packageIds
  withSharedLib <- haveCabalShared verbosity comp platform dbStack progDb unitIds
  return $
    ScriptEnv
      { runnerVerbosity = verbosityFlags verbosity
      , runnerProgramDb = progDb
      , runnerPackageDbStack = dbStack
      , runnerPlatform = platform
      , runnerCompiler = comp
      , -- NB: the set of packages available to test.hs scripts will COINCIDE
        -- with the dependencies of the cabal-tests component
        runnerPackages =
          [ (DefiniteUnitId (unsafeMkDefUnitId unitId), defaultRenaming)
          | unitId <- unitIds
          ]
      , runnerWithSharedLib = withSharedLib
      }

-- | Find the GHC of the version that built this package.
findBuildGhc :: Verbosity -> Maybe FilePath -> IO FilePath
findBuildGhc verbosity mbGhcPath = do
  found <- catMaybes <$> traverse findExecutable candidates
  versions <- traverse (programFindVersion ghcProgram verbosity) found
  case [path | (path, Just version) <- zip found versions, version == buildGhcVersion] of
    path : _ -> return path
    [] ->
      die . unlines $
        [ "Cannot find GHC " ++ prettyShow buildGhcVersion ++ " to run test scripts with."
        , "It has to be the version that built cabal-tests. Tried: " ++ unwords candidates
        , "Use --with-script-ghc to say where it is."
        ]
  where
    candidates =
      fromMaybe ["ghc-" ++ prettyShow buildGhcVersion, "ghc"] $
        pure <$> mbGhcPath

-- | The version of the GHC that built this package.
buildGhcVersion :: Version
buildGhcVersion = mkVersion' fullCompilerVersion

-- | Whether the Cabal library that test scripts use has a shared library.
haveCabalShared
  :: Verbosity
  -> Compiler
  -> Platform
  -> PackageDBStackCWD
  -> ProgramDb
  -> [UnitId]
  -> IO Bool
haveCabalShared verbosity comp platform dbStack progDb unitIds = do
  index <-
    getInstalledPackages
      verbosity
      comp
      Nothing
      (fmap (fmap makeSymbolicPath) dbStack)
      progDb
  let cabalLibs =
        [ ipi
        | Just ipi <- map (lookupUnitId index) unitIds
        , packageName ipi == mkPackageName "Cabal"
        ]
      sharedLibs =
        [ dir FilePath.</> mkSharedLibName platform (compilerId comp) (IPI.installedUnitId ipi)
        | ipi <- cabalLibs
        , dir <- IPI.libraryDynDirs ipi ++ IPI.libraryDirs ipi
        ]
  or <$> traverse doesFileExist sharedLibs

-- | Save a 'ScriptEnv' for 'readScriptEnv', so that each test script gets it
-- without working it out again.
writeScriptEnv :: FilePath -> ScriptEnv -> IO ()
writeScriptEnv file senv =
  writeFileAtomic file . encode $
    ( runnerProgramDb senv
    , runnerPackageDbStack senv
    , runnerPlatform senv
    , runnerCompiler senv
    , runnerPackages senv
    , runnerWithSharedLib senv
    )

-- | Read the 'ScriptEnv' saved by 'writeScriptEnv'.
readScriptEnv :: Verbosity -> FilePath -> IO ScriptEnv
readScriptEnv verbosity file = do
  decoded <- decodeFileOrFail' file
  case decoded of
    Left err -> die $ "Cannot read the script environment " ++ file ++ ": " ++ err
    Right (progDb, dbStack, platform, comp, packages, withSharedLib) ->
      return $
        ScriptEnv
          { runnerVerbosity = verbosityFlags verbosity
          , runnerProgramDb = progDb
          , runnerPackageDbStack = dbStack
          , runnerPlatform = platform
          , runnerCompiler = comp
          , runnerPackages = packages
          , runnerWithSharedLib = withSharedLib
          }

-- | Run a script with 'runghc', under the 'ScriptEnv'.
runghc
  :: ScriptEnv
  -> Maybe FilePath
  -> [(String, Maybe String)]
  -> FilePath
  -> [String]
  -> IO Result
runghc senv mb_cwd env_overrides script_path args = do
  (real_path, real_args) <- runnerCommand senv mb_cwd env_overrides script_path args
  run mb_cwd env_overrides real_path real_args Nothing

-- | Compute the command line which should be used to run a Haskell
-- script with 'runghc'.
runnerCommand
  :: ScriptEnv
  -> Maybe FilePath
  -> [(String, Maybe String)]
  -> FilePath
  -> [String]
  -> IO (FilePath, [String])
runnerCommand senv mb_cwd _env_overrides script_path args = do
  (prog, _) <- requireProgram verbosity runghcProgram (runnerProgramDb senv)
  return
    ( programPath prog
    , runghc_args ++ ["--"] ++ map ("--ghc-arg=" ++) ghc_args ++ [script_path] ++ args
    )
  where
    verbosity = mkVerbosity defaultVerbosityHandles $ runnerVerbosity senv
    runghc_args = []
    ghc_args = runnerGhcArgs senv mb_cwd

-- | Compute the GHC flags to invoke 'runghc' with under a 'ScriptEnv'.
runnerGhcArgs :: ScriptEnv -> Maybe FilePath -> [String]
runnerGhcArgs senv mb_cwd =
  renderGhcOptions (runnerCompiler senv) (runnerPlatform senv) ghc_options
  where
    ghc_options =
      M.mempty
        { ghcOptPackageDBs = fmap (fmap makeSymbolicPath) (runnerPackageDbStack senv)
        , ghcOptPackages = toNubListR (runnerPackages senv)
        , ghcOptHideAllPackages = Flag True
        , -- Avoid picking stray module files that look
          -- like our imports...
          ghcOptSourcePathClear = Flag True
        , -- ... yet retain the current directory as an included
          -- directory, e.g. so that we can compile a Setup.hs
          -- script which imports a locally defined module.
          -- See the PackageTests/SetupDep test.
          ghcOptSourcePath = toNubListR $
            case mb_cwd of
              Nothing -> []
              Just{} -> [sameDirectory]
        }
