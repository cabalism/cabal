{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      :  Distribution.Client.CmdHaddockModule
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- The @cabal haddock-module@ external command: the Haddock page of one
-- module of a local package, rendered against the library cabal has already
-- built rather than by documenting the whole component.
--
-- Haddock documents every home module reachable from its targets, so the
-- only way to document one module quickly is to make it the only home
-- module: GHC gets the package's own library as a dependency, from a copy of
-- the in-place package db in which the library's hidden modules are exposed,
-- and the one source file as its target. Everything else about the run, the
-- extensions, CPP, include dirs and package dbs, comes from the component's
-- saved configuration, the same one the build used.
--
-- This runs as @cabal-haddock-module@, an executable cabal finds on the
-- @PATH@, so it gets the arguments after the command name and the path of
-- the invoking cabal in @CABAL_EXTERNAL_CABAL_PATH@, nothing else.
module Distribution.Client.CmdHaddockModule
  ( haddockModuleCommand
  , haddockModuleAction
  , haddockModuleMain
  , HaddockModuleFlags (..)
  , defaultHaddockModuleFlags
  , HaddockModuleTarget (..)
  , parseHaddockModuleTarget
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.DistDirLayout (DistDirLayout (..))
import Distribution.Client.Errors
import Distribution.Client.GlobalFlags (defaultGlobalFlags)
import Distribution.Client.NixStyleOptions
  ( NixStyleFlags (..)
  , cfgVerbosity
  , defaultNixStyleFlags
  , nixStyleOptions
  )
import Distribution.Client.ProjectConfig (commandLineFlagsToProjectConfig)
import Distribution.Client.ProjectConfig.Types (BuildTimeSettings (..))
import Distribution.Client.ProjectOrchestration
  ( CurrentCommand (..)
  , ProjectBaseContext (..)
  , establishProjectBaseContext
  )
import Distribution.Client.ProjectPlanning (configureCompiler)
import Distribution.Client.RebuildMonad (runRebuild)
import Distribution.Client.Types.PackageLocation (PackageLocation (..))
import Distribution.Client.Types.PackageSpecifier (PackageSpecifier (..))
import Distribution.Solver.Types.SourcePackage (SourcePackage (..))

import Distribution.Backpack (OpenUnitId (..))
import Distribution.Compat.ResponseFile (expandResponse)
import Distribution.InstalledPackageInfo (showInstalledPackageInfo)
import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.ModuleName (ModuleName)
import qualified Distribution.ModuleName as ModuleName
import Distribution.Package (PackageName, packageId, packageName)
import Distribution.PackageDescription
  ( BuildInfo (..)
  , Library (libBuildInfo, libName)
  , PackageDescription (..)
  , explicitLibModules
  )
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.ReadE (succeedReadE)
import Distribution.Simple.BuildPaths
  ( autogenComponentModulesDir
  , autogenPackageModulesDir
  , getSourceFiles
  , haddockPackageLibraryName
  , haddockPackageLibraryName'
  )
import Distribution.Simple.Command
  ( CommandParse (..)
  , CommandUI (..)
  , OptionField
  , ShowOrParseArgs
  , commandParseArgs
  , commandShowOptions
  , option
  , reqArg
  , usageAlternatives
  )
import Distribution.Simple.Compiler
  ( Compiler (..)
  , PackageDBX (..)
  , coercePackageDB
  )
import Distribution.Simple.Configure
  ( ConfigStateFileError (..)
  , tryGetPersistBuildConfig
  )
import Distribution.Simple.Flag (Flag, flagToList, fromFlagOrDefault, toFlag, pattern Flag)
import qualified Distribution.Simple.GHC as GHC
import Distribution.Simple.Haddock
  ( Directory (..)
  , HaddockArgs (..)
  , Output (..)
  , getGhcLibDir
  , getHaddockProg
  , haddockPackagePaths
  , runHaddock
  )
import Distribution.Simple.LocalBuildInfo
  ( ComponentLocalBuildInfo (..)
  , LocalBuildInfo (..)
  , buildDir
  , componentBuildDir
  , mbWorkDirLBI
  )
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.Program (ConfiguredProgram)
import Distribution.Simple.Program.Find (defaultProgramSearchPath, findProgramOnSearchPath)
import Distribution.Simple.Program.GHC (GhcOptions (..))
import qualified Distribution.Simple.Program.HcPkg as HcPkg
import Distribution.Simple.Setup (trueArg)
import Distribution.Simple.Utils
  ( TempFileOptions (..)
  , VerboseException
  , cabalVersion
  , createDirectoryIfMissingVerbose
  , dieWithException
  , info
  , isUserException
  , rawSystemExitCode
  , topHandler
  , warn
  , withOutputMarker
  , withTempDirectoryEx
  , writeUTF8File
  )
import Distribution.Types.ComponentLocalBuildInfo (componentIsIndefinite)
import Distribution.Types.ComponentName (ComponentName (..))
import Distribution.Types.ExposedModule (ExposedModule (..))
import Distribution.Types.LibraryName (LibraryName (..))
import Distribution.Types.LocalBuildInfo (componentNameCLBIs)
import Distribution.Types.ModuleRenaming (defaultRenaming)
import Distribution.Types.UnitId (unsafeMkDefUnitId)
import Distribution.Utils.NubList (toNubListR)
import Distribution.Utils.Path
  ( coerceSymbolicPath
  , getSymbolicPath
  , interpretSymbolicPath
  , makeSymbolicPath
  )
import Distribution.Verbosity (VerbosityLevel (..), normal, verbosityFlags, verbosityLevel)

import Data.List ((\\))
import Data.Monoid (Any (..))
import System.Directory (doesDirectoryExist, doesFileExist, makeAbsolute)
import System.Environment (lookupEnv)
import System.FilePath (equalFilePath, takeDirectory, (<.>), (</>))
import System.IO (hPutStr, stderr)

-------------------------------------------------------------------------------
-- Command
-------------------------------------------------------------------------------

haddockModuleCommand :: CommandUI (NixStyleFlags HaddockModuleFlags)
haddockModuleCommand = haddockModuleCommandWith haddockModuleOptions defaultHaddockModuleFlags

-- | The command with other extra flags; what 'cabal build' is given is
-- rendered from the same command with no extra flags at all.
haddockModuleCommandWith :: (ShowOrParseArgs -> [OptionField a]) -> a -> CommandUI (NixStyleFlags a)
haddockModuleCommandWith extraOptions extraDefaults =
  CommandUI
    { commandName = "haddock-module"
    , commandSynopsis = "Build the Haddock page of one module of a local package."
    , commandUsage = usageAlternatives "haddock-module" ["[FLAGS] [PACKAGE:]MODULE"]
    , commandDescription = Just $ \_ ->
        "Renders the Haddock page of MODULE on its own, against the library "
          ++ "that cabal has already built, which is much faster than "
          ++ "documenting the whole component as 'cabal haddock MODULE' does. "
          ++ "The library is built first unless --no-build is given. "
          ++ "MODULE may be any module of a local package's main library, "
          ++ "hidden ones included; name the package when more than one has "
          ++ "the module.\n\n"
          ++ "The page goes under <builddir>/haddock-module/<package>/ by "
          ++ "default. Links to other modules go to the documentation cabal "
          ++ "has generated for them, if any, so they resolve once 'cabal "
          ++ "haddock' has run for the package and its dependencies.\n\n"
          ++ "This is an external command: cabal runs the cabal-haddock-module "
          ++ "executable that cabal-install installs next to cabal. Global "
          ++ "flags are not passed to it; give --builddir, --project-file and "
          ++ "the like after the command name.\n"
    , commandNotes = Just $ \pname ->
        "Examples:\n"
          ++ "  "
          ++ pname
          ++ " haddock-module Foo.Bar\n"
          ++ "    Document module Foo.Bar of the local package that has it.\n"
          ++ "  "
          ++ pname
          ++ " haddock-module --no-build pkg:Foo.Bar\n"
          ++ "    Document module Foo.Bar of package pkg as last built.\n"
    , commandDefaultFlags = defaultNixStyleFlags extraDefaults
    , commandOptions = nixStyleOptions extraOptions
    }

data HaddockModuleFlags = HaddockModuleFlags
  { haddockModuleNoBuild :: Flag Bool
  , haddockModuleOutputDir :: Flag FilePath
  }
  deriving (Eq, Show)

defaultHaddockModuleFlags :: HaddockModuleFlags
defaultHaddockModuleFlags =
  HaddockModuleFlags
    { haddockModuleNoBuild = mempty
    , haddockModuleOutputDir = mempty
    }

haddockModuleOptions :: ShowOrParseArgs -> [OptionField HaddockModuleFlags]
haddockModuleOptions _ =
  [ option
      []
      ["no-build"]
      "Do not build the library first; document the module as last built."
      haddockModuleNoBuild
      (\v flags -> flags{haddockModuleNoBuild = v})
      trueArg
  , option
      []
      ["output-dir"]
      "Directory to put the page in (default: <builddir>/haddock-module/<package>)."
      haddockModuleOutputDir
      (\v flags -> flags{haddockModuleOutputDir = v})
      (reqArg "DIR" (succeedReadE Flag) flagToList)
  ]

-- | A module, qualified by its package when there is any doubt.
data HaddockModuleTarget = HaddockModuleTarget
  { targetPackage :: Maybe PackageName
  , targetModule :: ModuleName
  }
  deriving (Eq, Show)

parseHaddockModuleTarget :: String -> Maybe HaddockModuleTarget
parseHaddockModuleTarget s = case break (== ':') s of
  (m, "") -> HaddockModuleTarget Nothing <$> simpleParsec m
  (p, ':' : m) -> HaddockModuleTarget . Just <$> simpleParsec p <*> simpleParsec m
  _ -> Nothing

-------------------------------------------------------------------------------
-- Entry point of the executable
-------------------------------------------------------------------------------

-- | What @cabal-haddock-module@ does with its arguments: the same parsing
-- cabal does for its own commands, with the usage worded for the user who
-- typed @cabal haddock-module@.
haddockModuleMain :: [String] -> IO ()
haddockModuleMain args0 =
  topHandler (isUserException (Proxy @(VerboseException CabalInstallException))) $ do
    args <- expandResponse args0
    case commandParseArgs haddockModuleCommand False args of
      CommandHelp help -> putStr (help "cabal")
      CommandList opts -> putStr (unlines opts)
      CommandErrors errs -> do
        hPutStr stderr (intercalate "\n" errs)
        exitWith (ExitFailure 1)
      CommandReadyToGo (mkFlags, targets) ->
        haddockModuleAction (mkFlags (commandDefaultFlags haddockModuleCommand)) targets

-------------------------------------------------------------------------------
-- Action
-------------------------------------------------------------------------------

haddockModuleAction :: NixStyleFlags HaddockModuleFlags -> [String] -> IO ()
haddockModuleAction flags@NixStyleFlags{extraFlags = HaddockModuleFlags{..}} targetStrings = do
  HaddockModuleTarget{targetPackage, targetModule} <- case targetStrings of
    [s] | Just t <- parseHaddockModuleTarget s -> return t
    _ -> dieWithException verbosity (HaddockModuleBadTarget targetStrings)

  -- The project, as cheaply as cabal can establish it: no solving, no plan.
  let cliConfig = commandLineFlagsToProjectConfig defaultGlobalFlags flags mempty
  baseCtx <- establishProjectBaseContext verbosity cliConfig OtherCommand
  let layout = distDirLayout baseCtx
      tmpFileOpts = TempFileOptions{optKeepTempFiles = buildSettingKeepTempFiles (buildSettings baseCtx)}

  -- The package, from the project's package descriptions rather than from
  -- anything built, so that the message for a wrong module is the same
  -- whether or not the library has been built.
  let localLibs =
        [ (packageName pd, lib)
        | SpecificSourcePackage SourcePackage{srcpkgDescription, srcpkgSource = LocalUnpackedPackage _} <-
            localPackages baseCtx
        , let pd = flattenPackageDescription srcpkgDescription
        , Just lib <- [library pd]
        ]
      owners = [p | (p, lib) <- localLibs, targetModule `elem` explicitLibModules lib]
  pkg <- case targetPackage of
    Just p
      | p `elem` map fst localLibs -> return p
      | otherwise -> dieWithException verbosity (HaddockModuleNoSuchPackage (prettyShow p))
    Nothing -> case owners of
      [p] -> return p
      [] -> dieWithException verbosity (HaddockModuleNotFound (prettyShow targetModule))
      ps -> dieWithException verbosity (HaddockModuleAmbiguous (prettyShow targetModule) (map prettyShow ps))

  -- Build the library, so the page shows the source as it is now. Every
  -- option cabal build also takes goes to it, so the configuration matches.
  unless (fromFlagOrDefault False haddockModuleNoBuild) $ do
    cabalExe <-
      lookupEnv "CABAL_EXTERNAL_CABAL_PATH" >>= \case
        Just exe -> return exe
        Nothing ->
          findProgramOnSearchPath verbosity defaultProgramSearchPath "cabal" >>= \case
            Just (exe, _) -> return exe
            Nothing -> dieWithException verbosity HaddockModuleNoCabal
    let forwardUI = haddockModuleCommandWith (const []) ()
        -- The defaults render too, and not all of them as 'cabal build'
        -- options, so only what differs from them goes.
        forwarded =
          commandShowOptions forwardUI flags{extraFlags = ()}
            \\ commandShowOptions forwardUI (commandDefaultFlags forwardUI)
        buildArgs = ["build", "-v0"] ++ forwarded ++ ["lib:" ++ prettyShow pkg]
    info verbosity $ "Running " ++ unwords (cabalExe : buildArgs)
    exit <- rawSystemExitCode verbosity Nothing cabalExe buildArgs Nothing
    unless (exit == ExitSuccess) $ exitWith exit

  -- The compiler as the project configures it, from cabal's cache.
  (compiler, platform, progdb) <-
    runRebuild (distProjectRootDirectory layout) $
      configureCompiler verbosity layout (projectConfig baseCtx)

  -- The library as built and registered in the in-place package db. Its
  -- registration says where its build directory is.
  let inplaceDb = distPackageDB layout (compilerId compiler)
      notBuilt = dieWithException verbosity (HaddockModuleNotBuilt (prettyShow pkg))
  inplaceDbPath <- case inplaceDb of
    SpecificPackageDB p -> return p
    _ -> dieWithException verbosity (HaddockModuleUnsupported "the build directory has no in-place package db.")
  hasInplaceDb <- doesDirectoryExist inplaceDbPath
  unless hasInplaceDb notBuilt
  inplaceIndex <- GHC.getPackageDBContents verbosity Nothing (coercePackageDB inplaceDb) progdb
  ipi <- case [p | p <- PackageIndex.allPackages inplaceIndex, packageName p == pkg, IPI.sourceLibName p == LMainLibName] of
    [p] -> return p
    _ -> notBuilt
  distDir <- case IPI.importDirs ipi of
    (d : _) -> return (takeDirectory d)
    [] -> dieWithException verbosity (HaddockModuleUnsupported "the library has no modules.")

  -- The component's configuration, as the build saved it. Only the Cabal
  -- that wrote it can read it, which is the one cabal links when it builds a
  -- Simple package in process, and the one this executable links.
  lbi <-
    tryGetPersistBuildConfig Nothing (makeSymbolicPath distDir) >>= \case
      Right lbi -> return lbi
      Left (ConfigStateFileBadVersion oldCabal _ _) ->
        dieWithException verbosity (HaddockModuleCabalVersionMismatch (prettyShow oldCabal) ("Cabal-" ++ prettyShow cabalVersion))
      Left ConfigStateFileMissing{} -> notBuilt
      Left _ -> dieWithException verbosity (HaddockModuleUnsupported "the library's saved configuration cannot be read; rebuild it.")
  let pkgDir = mbWorkDirLBI lbi
      pd = localPkgDescr lbi
  lib <- maybe (dieWithException verbosity (HaddockModuleNoSuchPackage (prettyShow pkg))) return (library pd)
  clbi <- case componentNameCLBIs lbi (CLibName LMainLibName) of
    [c] -> return c
    _ -> dieWithException verbosity (HaddockModuleUnsupported "the library has no single configured component.")
  when (componentUnitId clbi /= IPI.installedUnitId ipi) $
    dieWithException verbosity (HaddockModuleUnsupported "the library's registration is stale; rebuild it.")
  when (componentIsIndefinite clbi) $
    dieWithException verbosity (HaddockModuleUnsupported "the library is an indefinite Backpack component.")
  let bi = libBuildInfo lib
      libModules = map exposedName (IPI.exposedModules ipi) ++ IPI.hiddenModules ipi
  unless (targetModule `elem` libModules) $
    dieWithException verbosity (HaddockModuleUnsupported ("the built library has no module " ++ prettyShow targetModule ++ "; rebuild it."))

  -- The module's source, looked up as the build looks it up, so a
  -- preprocessed module is found in the build directory.
  srcFile <-
    getSourceFiles
      verbosity
      pkgDir
      ( coerceSymbolicPath (componentBuildDir lbi clbi)
          : hsSourceDirs bi
          ++ [autogenComponentModulesDir lbi clbi, autogenPackageModulesDir lbi]
      )
      [targetModule]
      >>= \case
        [(_, f)] -> return f
        _ -> dieWithException verbosity (HaddockModuleUnsupported ("no source file for " ++ prettyShow targetModule ++ "."))

  outDir <- case haddockModuleOutputDir of
    Flag d -> makeAbsolute d
    _ -> return (distDirectory layout </> "haddock-module" </> prettyShow pkg)

  createDirectoryIfMissingVerbose verbosity True (distTempDirectory layout)
  withTempDirectoryEx tmpFileOpts (distTempDirectory layout) "haddock-module" $ \tmpDir -> do
    -- A package db like the in-place one, but with the library's hidden
    -- modules exposed, so the module can import any sibling as it does
    -- inside the component.
    let tmpDb = tmpDir </> "packagedb"
        ghcPkg = GHC.hcPkgInfo progdb
        expose p
          | IPI.installedUnitId p == IPI.installedUnitId ipi =
              p
                { IPI.exposedModules = IPI.exposedModules p ++ map (`ExposedModule` Nothing) (IPI.hiddenModules p)
                , IPI.hiddenModules = []
                }
          | otherwise = p
    HcPkg.init ghcPkg verbosity tmpDb
    for_ (PackageIndex.allPackages inplaceIndex) $ \p ->
      writeUTF8File (tmpDb </> prettyShow (IPI.installedUnitId p) <.> "conf") (showInstalledPackageInfo (expose p))
    HcPkg.recache ghcPkg verbosity Nothing (SpecificPackageDB (makeSymbolicPath tmpDb))

    -- GHC's options are the component's, except that the component is now
    -- a dependency: no unit id of its own, no source path for its modules
    -- (a module found on the source path would be a home module, compiled
    -- and documented again), the temporary db in place of the in-place one,
    -- and nothing written into the real build directory.
    let opts0 = GHC.componentGhcOptions (verbosityLevel verbosity) lbi bi clbi (buildDir lbi)
        artifacts = toFlag (makeSymbolicPath (tmpDir </> "build"))
        swapDb = \case
          SpecificPackageDB p
            | equalFilePath (interpretSymbolicPath pkgDir p) inplaceDbPath ->
                SpecificPackageDB (makeSymbolicPath tmpDb)
          db -> db
        opts =
          opts0
            { ghcOptSourcePath = mempty
            , ghcOptThisUnitId = mempty
            , ghcOptThisComponentId = mempty
            , ghcOptInstantiatedWith = []
            , ghcOptPackages =
                ghcOptPackages opts0
                  <> toNubListR [(DefiniteUnitId (unsafeMkDefUnitId (IPI.installedUnitId ipi)), defaultRenaming)]
            , ghcOptPackageDBs = map swapDb (ghcOptPackageDBs opts0)
            , ghcOptObjDir = artifacts
            , ghcOptHiDir = artifacts
            , ghcOptHieDir = artifacts
            , ghcOptStubDir = artifacts
            , ghcOptOutputDir = artifacts
            , ghcOptBytecodeDir = artifacts
            }

    -- Links go to the documentation of the dependencies and, when cabal
    -- haddock has run for it, of the package itself.
    deps <- case PackageIndex.dependencyClosure (installedPkgs lbi) (map fst (componentPackageDeps clbi)) of
      Left closure -> return (PackageIndex.allPackages closure)
      Right _ -> dieWithException verbosity (HaddockModuleUnsupported "the library's dependencies are inconsistent; rebuild it.")
    ownDocs <- or <$> traverse doesFileExist (IPI.haddockInterfaces ipi)
    (interfaces, missingDocs) <- haddockPackagePaths (deps ++ [ipi | ownDocs]) Nothing
    for_ missingDocs (warn verbosity)

    let args =
          mempty
            { argOutput = Flag [Html]
            , argOutputDir = Dir outDir
            , argPackageName = Flag (packageId pd)
            , argComponentName = Flag (haddockPackageLibraryName' (packageName pd) (libName lib))
            , argTitle = Flag (haddockPackageLibraryName pd lib)
            , argInterfaces = interfaces
            , argGhcOptions = opts
            , argTargets = [getSymbolicPath srcFile]
            , argQuickJump = Flag False
            , argLinkedSource = Flag False
            , argVerbose = Any (verbosityLevel verbosity >= Verbose)
            }
    (haddockProg, _) <- getHaddockProg verbosity progdb compiler args (Flag False)
    libDirArgs <- getGhcLibDir verbosity lbi
    runHaddock verbosity pkgDir tmpFileOpts compiler platform (haddockProg :: ConfiguredProgram) True (libDirArgs <> args)

  putStr $ withOutputMarker (verbosityFlags verbosity) (outDir </> moduleHtmlFile targetModule ++ "\n")
  where
    verbosity = cfgVerbosity normal flags

-- | The file haddock writes a module's page to, under its output directory.
moduleHtmlFile :: ModuleName -> FilePath
moduleHtmlFile m = intercalate "-" (ModuleName.components m) <.> "html"
