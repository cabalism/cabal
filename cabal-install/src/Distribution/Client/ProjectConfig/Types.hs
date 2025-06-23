{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

-- | Handling project configuration, types.
module Distribution.Client.ProjectConfig.Types
  ( -- * Types for project config
    ProjectConfig (..)
  , ProjectConfigToParse (..)
  , ProjectConfigBuildOnly (..)
  , ProjectConfigShared (..)
  , ProjectConfigProvenance (..)
  , PackageConfig (..)

    -- * Resolving configuration
  , SolverSettings (..)
  , BuildTimeSettings (..)
  , ParStratX (..)
  , isParallelBuild
  , ParStrat

    -- * Extra useful Monoids
  , MapLast (..)
  , MapMappend (..)
  ) where

import Text.PrettyPrint hiding ((<>))
import Distribution.Client.Compat.Prelude
import Prelude ()

import qualified Data.ByteString.Char8 as BS
import Distribution.Client.BuildReports.Types
  ( ReportLevel (..)
  )
import Distribution.Client.Dependency.Types
  ( PreSolver
  )
import Distribution.Client.Targets
  ( UserConstraint
  )
import Distribution.Client.Types.AllowNewer (AllowNewer (..), AllowOlder (..))
import Distribution.Client.Types.Repo (LocalRepo, RemoteRepo)
import Distribution.Client.Types.SourceRepo (SourceRepoList)
import Distribution.Client.Types.WriteGhcEnvironmentFilesPolicy (WriteGhcEnvironmentFilesPolicy)

import Distribution.Client.IndexUtils.ActiveRepos
  ( ActiveRepos
  )
import Distribution.Client.IndexUtils.IndexState
  ( TotalIndexState
  )

import Distribution.Client.CmdInstall.ClientInstallFlags
  ( ClientInstallFlags (..)
  )

import Distribution.Solver.Types.ConstraintSource
import Distribution.Solver.Types.Settings

import Distribution.Package
  ( PackageId
  , PackageName
  , UnitId
  )
import Distribution.PackageDescription
  ( FlagAssignment
  )
import Distribution.Simple.Compiler
  ( Compiler
  , CompilerFlavor
  , DebugInfoLevel (..)
  , OptimisationLevel (..)
  , PackageDBCWD
  , ProfDetailLevel
  )
import Distribution.Simple.InstallDirs
  ( InstallDirs
  , PathTemplate
  )
import Distribution.Simple.Setup
  ( DumpBuildInfo (..)
  , Flag
  , HaddockTarget (..)
  , TestShowDetails (..)
  )
import Distribution.System
  ( Platform
  )
import Distribution.Types.PackageVersionConstraint
  ( PackageVersionConstraint
  )
import Distribution.Utils.NubList
  ( NubList
  )
import Distribution.Version
  ( Version
  )

import qualified Data.Map as Map
import Distribution.Solver.Types.ProjectConfigPath (ProjectConfigPath)
import Distribution.Types.ParStrat

-------------------------------
-- Project config types
--

-- | The project configuration is configuration that is parsed but parse
-- configuration may import more configuration. Holds the unparsed contents of
-- an imported file contributing to the project config.
newtype ProjectConfigToParse = ProjectConfigToParse BS.ByteString

-- | This type corresponds directly to what can be written in the
-- @cabal.project@ file. Other sources of configuration can also be injected
-- into this type, such as the user-wide config file and the
-- command line of @cabal configure@ or @cabal build@.
--
-- Since it corresponds to the external project file it is an instance of
-- 'Monoid' and all the fields can be empty. This also means there has to
-- be a step where we resolve configuration. At a minimum resolving means
-- applying defaults but it can also mean merging information from multiple
-- sources. For example for package-specific configuration the project file
-- can specify configuration that applies to all local packages, and then
-- additional configuration for a specific package.
--
-- Future directions: multiple profiles, conditionals. If we add these
-- features then the gap between configuration as written in the config file
-- and resolved settings we actually use will become even bigger.
data ProjectConfig = ProjectConfig
  { projectPackages :: [String]
  -- ^ Packages in this project, including local dirs, local .cabal files
  -- local and remote tarballs. When these are file globs, they must
  -- match at least one package.
  , projectPackagesOptional :: [String]
  -- ^ Like 'projectConfigPackageGlobs' but /optional/ in the sense that
  -- file globs are allowed to match nothing. The primary use case for
  -- this is to be able to say @optional-packages: */@ to automagically
  -- pick up deps that we unpack locally without erroring when
  -- there aren't any.
  , projectPackagesRepo :: [SourceRepoList]
  -- ^ Packages in this project from remote source repositories.
  , projectPackagesNamed :: [PackageVersionConstraint]
  -- ^ Packages in this project from hackage repositories.
  , -- See respective types for an explanation of what these
    -- values are about:
    projectConfigBuildOnly :: ProjectConfigBuildOnly
  , projectConfigShared :: ProjectConfigShared
  , projectConfigProvenance :: Set ProjectConfigProvenance
  , projectConfigAllPackages :: PackageConfig
  -- ^ Configuration to be applied to *all* packages,
  -- whether named in `cabal.project` or not.
  , projectConfigLocalPackages :: PackageConfig
  -- ^ Configuration to be applied to *local* packages; i.e.,
  -- any packages which are explicitly named in `cabal.project`.
  , projectConfigSpecificPackage :: MapMappend PackageName PackageConfig
  }
  deriving (Eq, Generic)

instance Show ProjectConfig where
  show ProjectConfig{..} = render $
    (text "ProjectConfig {" <+> text (" projectPackages = " ++ show projectPackages))
    $+$
    vcat
      [ text (", projectPackagesOptional = " ++ show projectPackagesOptional)
      , text (", projectPackagesRepo = " ++ show projectPackagesRepo)
      , text (", projectPackagesNamed = " ++ show projectPackagesNamed)
      , text (", projectConfigBuildOnly = " ++ show projectConfigBuildOnly)
      , text (", projectConfigShared = " ++ show projectConfigShared)
      , text (", projectConfigProvenance = " ++ show projectConfigProvenance)
      , text (", projectConfigAllPackages = " ++ show projectConfigAllPackages)
      , text (", projectConfigLocalPackages = " ++ show projectConfigLocalPackages)
      , text (", projectConfigSpecificPackage = " ++ show projectConfigSpecificPackage)
      ]

-- | That part of the project configuration that only affects /how/ we build
-- and not the /value/ of the things we build. This means this information
-- does not need to be tracked for changes since it does not affect the
-- outcome.
data ProjectConfigBuildOnly = ProjectConfigBuildOnly
  { projectConfigVerbosity :: Flag Verbosity
  , projectConfigDryRun :: Flag Bool
  , projectConfigOnlyDeps :: Flag Bool
  , projectConfigOnlyDownload :: Flag Bool
  , projectConfigSummaryFile :: NubList PathTemplate
  , projectConfigLogFile :: Flag PathTemplate
  , projectConfigBuildReports :: Flag ReportLevel
  , projectConfigReportPlanningFailure :: Flag Bool
  , projectConfigSymlinkBinDir :: Flag FilePath
  , projectConfigNumJobs :: Flag (Maybe Int)
  , projectConfigUseSemaphore :: Flag Bool
  , projectConfigKeepGoing :: Flag Bool
  , projectConfigOfflineMode :: Flag Bool
  , projectConfigKeepTempFiles :: Flag Bool
  , projectConfigHttpTransport :: Flag String
  , projectConfigIgnoreExpiry :: Flag Bool
  , projectConfigCacheDir :: Flag FilePath
  , projectConfigLogsDir :: Flag FilePath
  , projectConfigClientInstallFlags :: ClientInstallFlags
  }
  deriving (Eq, Generic)

instance Show ProjectConfigBuildOnly where
  show ProjectConfigBuildOnly{..} = render $
    (text "ProjectConfigBuildOnly {" <+> text (" projectConfigVerbosity = " ++ show projectConfigVerbosity))
    $+$
    vcat
      [ text (", projectConfigDryRun = " ++ show projectConfigDryRun)
      , text (", projectConfigOnlyDeps = " ++ show projectConfigOnlyDeps)
      , text (", projectConfigOnlyDownload = " ++ show projectConfigOnlyDownload)
      , text (", projectConfigSummaryFile = " ++ show projectConfigSummaryFile)
      , text (", projectConfigLogFile = " ++ show projectConfigLogFile)
      , text (", projectConfigBuildReports = " ++ show projectConfigBuildReports)
      , text (", projectConfigReportPlanningFailure = " ++ show projectConfigReportPlanningFailure)
      , text (", projectConfigSymlinkBinDir = " ++ show projectConfigSymlinkBinDir)
      , text (", projectConfigNumJobs = " ++ show projectConfigNumJobs)
      , text (", projectConfigUseSemaphore = " ++ show projectConfigUseSemaphore)
      , text (", projectConfigKeepGoing = " ++ show projectConfigKeepGoing)
      , text (", projectConfigOfflineMode = " ++ show projectConfigOfflineMode)
      , text (", projectConfigKeepTempFiles = " ++ show projectConfigKeepTempFiles)
      , text (", projectConfigHttpTransport = " ++ show projectConfigHttpTransport)
      , text (", projectConfigIgnoreExpiry = " ++ show projectConfigIgnoreExpiry)
      , text (", projectConfigCacheDir = " ++ show projectConfigCacheDir)
      , text (", projectConfigLogsDir = " ++ show projectConfigLogsDir)
      , text (", projectConfigClientInstallFlags = " ++ show projectConfigClientInstallFlags)
      ]

-- | Project configuration that is shared between all packages in the project.
-- In particular this includes configuration that affects the solver.
data ProjectConfigShared = ProjectConfigShared
  { projectConfigDistDir :: Flag FilePath
  , projectConfigConfigFile :: Flag FilePath
  , projectConfigProjectDir :: Flag FilePath
  , projectConfigProjectFile :: Flag FilePath
  , projectConfigIgnoreProject :: Flag Bool
  , projectConfigHcFlavor :: Flag CompilerFlavor
  , projectConfigHcPath :: Flag FilePath
  , projectConfigHcPkg :: Flag FilePath
  , projectConfigHaddockIndex :: Flag PathTemplate
  , -- Only makes sense for manual mode, not --local mode
    -- too much control!
    -- projectConfigUserInstall       :: Flag Bool,

    projectConfigInstallDirs :: InstallDirs (Flag PathTemplate)
  , projectConfigPackageDBs :: [Maybe PackageDBCWD]
  , -- configuration used both by the solver and other phases
    projectConfigRemoteRepos :: NubList RemoteRepo
  -- ^ Available Hackage servers.
  , projectConfigLocalNoIndexRepos :: NubList LocalRepo
  , projectConfigActiveRepos :: Flag ActiveRepos
  , projectConfigIndexState :: Flag TotalIndexState
  , projectConfigStoreDir :: Flag FilePath
  , -- solver configuration
    projectConfigConstraints :: [(UserConstraint, ConstraintSource)]
  , projectConfigPreferences :: [PackageVersionConstraint]
  , projectConfigCabalVersion :: Flag Version -- TODO: [required eventually] unused
  , projectConfigSolver :: Flag PreSolver
  , projectConfigAllowOlder :: Maybe AllowOlder
  , projectConfigAllowNewer :: Maybe AllowNewer
  , projectConfigWriteGhcEnvironmentFilesPolicy
      :: Flag WriteGhcEnvironmentFilesPolicy
  , projectConfigMaxBackjumps :: Flag Int
  , projectConfigReorderGoals :: Flag ReorderGoals
  , projectConfigCountConflicts :: Flag CountConflicts
  , projectConfigFineGrainedConflicts :: Flag FineGrainedConflicts
  , projectConfigMinimizeConflictSet :: Flag MinimizeConflictSet
  , projectConfigStrongFlags :: Flag StrongFlags
  , projectConfigAllowBootLibInstalls :: Flag AllowBootLibInstalls
  , projectConfigOnlyConstrained :: Flag OnlyConstrained
  , projectConfigPerComponent :: Flag Bool
  , projectConfigIndependentGoals :: Flag IndependentGoals
  , projectConfigPreferOldest :: Flag PreferOldest
  , projectConfigProgPathExtra :: NubList FilePath
  , projectConfigMultiRepl :: Flag Bool
  -- More things that only make sense for manual mode, not --local mode
  -- too much control!
  -- projectConfigShadowPkgs        :: Flag Bool,
  -- projectConfigReinstall         :: Flag Bool,
  -- projectConfigAvoidReinstalls   :: Flag Bool,
  -- projectConfigOverrideReinstall :: Flag Bool,
  -- projectConfigUpgradeDeps       :: Flag Bool
  }
  deriving (Eq, Generic)

instance Show ProjectConfigShared where
  show ProjectConfigShared{..} = render $
    (text "ProjectConfigShared {" <+> text (" projectConfigDistDir = " ++ show projectConfigDistDir))
    $+$
    vcat
      [ text (", projectConfigConfigFile = " ++ show projectConfigConfigFile)
      , text (", projectConfigProjectDir = " ++ show projectConfigProjectDir)
      , text (", projectConfigProjectFile = " ++ show projectConfigProjectFile)
      , text (", projectConfigIgnoreProject = " ++ show projectConfigIgnoreProject)
      , text (", projectConfigHcFlavor = " ++ show projectConfigHcFlavor)
      , text (", projectConfigHcPath = " ++ show projectConfigHcPath)
      , text (", projectConfigHcPkg = " ++ show projectConfigHcPkg)
      , text (", projectConfigHaddockIndex = " ++ show projectConfigHaddockIndex)
      , text (", projectConfigInstallDirs = " ++ show projectConfigInstallDirs)
      , text (", projectConfigPackageDBs = " ++ show projectConfigPackageDBs)
      , text (", projectConfigRemoteRepos = " ++ show projectConfigRemoteRepos)
      , text (", projectConfigLocalNoIndexRepos = " ++ show projectConfigLocalNoIndexRepos)
      , text (", projectConfigActiveRepos = " ++ show projectConfigActiveRepos)
      , text (", projectConfigIndexState = " ++ show projectConfigIndexState)
      , text (", projectConfigStoreDir = " ++ show projectConfigStoreDir)
      , text (", projectConfigConstraints = " ++ show projectConfigConstraints)
      , text (", projectConfigPreferences = " ++ show projectConfigPreferences)
      , text (", projectConfigCabalVersion = " ++ show projectConfigCabalVersion)
      , text (", projectConfigSolver = " ++ show projectConfigSolver)
      , text (", projectConfigAllowOlder = " ++ show projectConfigAllowOlder)
      , text (", projectConfigAllowNewer = " ++ show projectConfigAllowNewer)
      , text (", projectConfigWriteGhcEnvironmentFilesPolicy = " ++ show projectConfigWriteGhcEnvironmentFilesPolicy)
      , text (", projectConfigMaxBackjumps = " ++ show projectConfigMaxBackjumps)
      , text (", projectConfigReorderGoals = " ++ show projectConfigReorderGoals)
      , text (", projectConfigCountConflicts = " ++ show projectConfigCountConflicts)
      , text (", projectConfigFineGrainedConflicts = " ++ show projectConfigFineGrainedConflicts)
      , text (", projectConfigMinimizeConflictSet = " ++ show projectConfigMinimizeConflictSet)
      , text (", projectConfigStrongFlags = " ++ show projectConfigStrongFlags)
      , text (", projectConfigAllowBootLibInstalls = " ++ show projectConfigAllowBootLibInstalls)
      , text (", projectConfigOnlyConstrained = " ++ show projectConfigOnlyConstrained)
      , text (", projectConfigPerComponent = " ++ show projectConfigPerComponent)
      , text (", projectConfigIndependentGoals = " ++ show projectConfigIndependentGoals)
      , text (", projectConfigPreferOldest = " ++ show projectConfigPreferOldest)
      , text (", projectConfigProgPathExtra = " ++ show projectConfigProgPathExtra)
      , text (", projectConfigMultiRepl = " ++ show projectConfigMultiRepl)
      ]

-- | Specifies the provenance of project configuration, whether defaults were
-- used or if the configuration was read from an explicit file path.
data ProjectConfigProvenance
  = -- | The configuration is implicit due to no explicit configuration
    -- being found. See 'Distribution.Client.ProjectConfig.readProjectConfig'
    -- for how implicit configuration is determined.
    Implicit
  | -- | The path the project configuration was explicitly read from.
    -- | The configuration was explicitly read from the specified 'ProjectConfigPath'.
    Explicit ProjectConfigPath
  deriving (Eq, Ord, Show, Generic)

-- | Project configuration that is specific to each package, that is where we
-- can in principle have different values for different packages in the same
-- project.
data PackageConfig = PackageConfig
  { packageConfigProgramPaths :: MapLast String FilePath
  , packageConfigProgramArgs :: MapMappend String [String]
  , packageConfigProgramPathExtra :: NubList FilePath
  , packageConfigFlagAssignment :: FlagAssignment
  , packageConfigVanillaLib :: Flag Bool
  , packageConfigSharedLib :: Flag Bool
  , packageConfigStaticLib :: Flag Bool
  , packageConfigDynExe :: Flag Bool
  , packageConfigFullyStaticExe :: Flag Bool
  , packageConfigProf :: Flag Bool -- TODO: [code cleanup] sort out
  , packageConfigProfLib :: Flag Bool --      this duplication
  , packageConfigProfShared :: Flag Bool
  , packageConfigProfExe :: Flag Bool --      and consistency
  , packageConfigProfDetail :: Flag ProfDetailLevel
  , packageConfigProfLibDetail :: Flag ProfDetailLevel
  , packageConfigConfigureArgs :: [String]
  , packageConfigOptimization :: Flag OptimisationLevel
  , packageConfigProgPrefix :: Flag PathTemplate
  , packageConfigProgSuffix :: Flag PathTemplate
  , packageConfigExtraLibDirs :: [FilePath]
  , packageConfigExtraLibDirsStatic :: [FilePath]
  , packageConfigExtraFrameworkDirs :: [FilePath]
  , packageConfigExtraIncludeDirs :: [FilePath]
  , packageConfigGHCiLib :: Flag Bool
  , packageConfigSplitSections :: Flag Bool
  , packageConfigSplitObjs :: Flag Bool
  , packageConfigStripExes :: Flag Bool
  , packageConfigStripLibs :: Flag Bool
  , packageConfigTests :: Flag Bool
  , packageConfigBenchmarks :: Flag Bool
  , packageConfigCoverage :: Flag Bool
  , packageConfigRelocatable :: Flag Bool
  , packageConfigDebugInfo :: Flag DebugInfoLevel
  , packageConfigDumpBuildInfo :: Flag DumpBuildInfo
  , packageConfigRunTests :: Flag Bool -- TODO: [required eventually] use this
  , packageConfigDocumentation :: Flag Bool -- TODO: [required eventually] use this
  -- Haddock options
  , packageConfigHaddockHoogle :: Flag Bool -- TODO: [required eventually] use this
  , packageConfigHaddockHtml :: Flag Bool -- TODO: [required eventually] use this
  , packageConfigHaddockHtmlLocation :: Flag String -- TODO: [required eventually] use this
  , packageConfigHaddockForeignLibs :: Flag Bool -- TODO: [required eventually] use this
  , packageConfigHaddockExecutables :: Flag Bool -- TODO: [required eventually] use this
  , packageConfigHaddockTestSuites :: Flag Bool -- TODO: [required eventually] use this
  , packageConfigHaddockBenchmarks :: Flag Bool -- TODO: [required eventually] use this
  , packageConfigHaddockInternal :: Flag Bool -- TODO: [required eventually] use this
  , packageConfigHaddockCss :: Flag FilePath -- TODO: [required eventually] use this
  , packageConfigHaddockLinkedSource :: Flag Bool -- TODO: [required eventually] use this
  , packageConfigHaddockQuickJump :: Flag Bool -- TODO: [required eventually] use this
  , packageConfigHaddockHscolourCss :: Flag FilePath -- TODO: [required eventually] use this
  , packageConfigHaddockContents :: Flag PathTemplate -- TODO: [required eventually] use this
  , packageConfigHaddockIndex :: Flag PathTemplate -- TODO: [required eventually] use this
  , packageConfigHaddockBaseUrl :: Flag String -- TODO: [required eventually] use this
  , packageConfigHaddockResourcesDir :: Flag String -- TODO: [required eventually] use this
  , packageConfigHaddockOutputDir :: Flag FilePath -- TODO: [required eventually] use this
  , packageConfigHaddockUseUnicode :: Flag Bool -- TODO: [required eventually] use this
  , packageConfigHaddockForHackage :: Flag HaddockTarget
  , -- Test options
    packageConfigTestHumanLog :: Flag PathTemplate
  , packageConfigTestMachineLog :: Flag PathTemplate
  , packageConfigTestShowDetails :: Flag TestShowDetails
  , packageConfigTestKeepTix :: Flag Bool
  , packageConfigTestWrapper :: Flag FilePath
  , packageConfigTestFailWhenNoTestSuites :: Flag Bool
  , packageConfigTestTestOptions :: [PathTemplate]
  , -- Benchmark options
    packageConfigBenchmarkOptions :: [PathTemplate]
  }
  deriving (Eq, Generic)

instance Show PackageConfig where
  show PackageConfig{..} = render $
    (text "PackageConfig {" <+> text (" packageConfigProgramPaths = " ++ show packageConfigProgramPaths))
    $+$
    vcat
      [ text (", packageConfigProgramArgs = " ++ show packageConfigProgramArgs)
      , text (", packageConfigProgramPathExtra = " ++ show packageConfigProgramPathExtra)
      , text (", packageConfigFlagAssignment = " ++ show packageConfigFlagAssignment)
      , text (", packageConfigVanillaLib = " ++ show packageConfigVanillaLib)
      , text (", packageConfigSharedLib = " ++ show packageConfigSharedLib)
      , text (", packageConfigStaticLib = " ++ show packageConfigStaticLib)
      , text (", packageConfigDynExe = " ++ show packageConfigDynExe)
      , text (", packageConfigFullyStaticExe = " ++ show packageConfigFullyStaticExe)
      , text (", packageConfigProf = " ++ show packageConfigProf)
      , text (", packageConfigProfLib = " ++ show packageConfigProfLib)
      , text (", packageConfigProfShared = " ++ show packageConfigProfShared)
      , text (", packageConfigProfExe = " ++ show packageConfigProfExe)
      , text (", packageConfigProfDetail = " ++ show packageConfigProfDetail)
      , text (", packageConfigProfLibDetail = " ++ show packageConfigProfLibDetail)
      , text (", packageConfigConfigureArgs = " ++ show packageConfigConfigureArgs)
      , text (", packageConfigOptimization = " ++ show packageConfigOptimization)
      , text (", packageConfigProgPrefix = " ++ show packageConfigProgPrefix)
      , text (", packageConfigProgSuffix = " ++ show packageConfigProgSuffix)
      , text (", packageConfigExtraLibDirs = " ++ show packageConfigExtraLibDirs)
      , text (", packageConfigExtraLibDirsStatic = " ++ show packageConfigExtraLibDirsStatic)
      , text (", packageConfigExtraFrameworkDirs = " ++ show packageConfigExtraFrameworkDirs)
      , text (", packageConfigExtraIncludeDirs = " ++ show packageConfigExtraIncludeDirs)
      , text (", packageConfigGHCiLib = " ++ show packageConfigGHCiLib)
      , text (", packageConfigSplitSections = " ++ show packageConfigSplitSections)
      , text (", packageConfigSplitObjs = " ++ show packageConfigSplitObjs)
      , text (", packageConfigStripExes = " ++ show packageConfigStripExes)
      , text (", packageConfigStripLibs = " ++ show packageConfigStripLibs)
      , text (", packageConfigTests = " ++ show packageConfigTests)
      , text (", packageConfigBenchmarks = " ++ show packageConfigBenchmarks)
      , text (", packageConfigCoverage = " ++ show packageConfigCoverage)
      , text (", packageConfigRelocatable = " ++ show packageConfigRelocatable)
      , text (", packageConfigDebugInfo = " ++ show packageConfigDebugInfo)
      , text (", packageConfigDumpBuildInfo = " ++ show packageConfigDumpBuildInfo)
      , text (", packageConfigRunTests = " ++ show packageConfigRunTests)
      , text (", packageConfigDocumentation = " ++ show packageConfigDocumentation)
      , text (", packageConfigHaddockHoogle = " ++ show packageConfigHaddockHoogle)
      , text (", packageConfigHaddockHtml = " ++ show packageConfigHaddockHtml)
      , text (", packageConfigHaddockHtmlLocation = " ++ show packageConfigHaddockHtmlLocation)
      , text (", packageConfigHaddockForeignLibs = " ++ show packageConfigHaddockForeignLibs)
      , text (", packageConfigHaddockExecutables = " ++ show packageConfigHaddockExecutables)
      , text (", packageConfigHaddockTestSuites = " ++ show packageConfigHaddockTestSuites)
      , text (", packageConfigHaddockBenchmarks = " ++ show packageConfigHaddockBenchmarks)
      , text (", packageConfigHaddockInternal = " ++ show packageConfigHaddockInternal)
      , text (", packageConfigHaddockCss = " ++ show packageConfigHaddockCss)
      , text (", packageConfigHaddockLinkedSource = " ++ show packageConfigHaddockLinkedSource)
      , text (", packageConfigHaddockQuickJump = " ++ show packageConfigHaddockQuickJump)
      , text (", packageConfigHaddockHscolourCss = " ++ show packageConfigHaddockHscolourCss)
      , text (", packageConfigHaddockContents = " ++ show packageConfigHaddockContents)
      , text (", packageConfigHaddockIndex = " ++ show packageConfigHaddockIndex)
      , text (", packageConfigHaddockBaseUrl = " ++ show packageConfigHaddockBaseUrl)
      , text (", packageConfigHaddockResourcesDir = " ++ show packageConfigHaddockResourcesDir)
      , text (", packageConfigHaddockOutputDir = " ++ show packageConfigHaddockOutputDir)
      , text (", packageConfigHaddockUseUnicode = " ++ show packageConfigHaddockUseUnicode)
      , text (", packageConfigHaddockForHackage = " ++ show packageConfigHaddockForHackage)
      , text (", packageConfigTestHumanLog = " ++ show packageConfigTestHumanLog)
      , text (", packageConfigTestMachineLog = " ++ show packageConfigTestMachineLog)
      , text (", packageConfigTestShowDetails = " ++ show packageConfigTestShowDetails)
      , text (", packageConfigTestKeepTix = " ++ show packageConfigTestKeepTix)
      , text (", packageConfigTestWrapper = " ++ show packageConfigTestWrapper)
      , text (", packageConfigTestFailWhenNoTestSuites = " ++ show packageConfigTestFailWhenNoTestSuites)
      , text (", packageConfigTestTestOptions = " ++ show packageConfigTestTestOptions)
      , text (", packageConfigBenchmarkOptions = " ++ show packageConfigBenchmarkOptions)
      ]

instance Binary ProjectConfig
instance Binary ProjectConfigBuildOnly
instance Binary ProjectConfigShared
instance Binary ProjectConfigProvenance
instance Binary PackageConfig

instance Structured ProjectConfig
instance Structured ProjectConfigBuildOnly
instance Structured ProjectConfigShared
instance Structured ProjectConfigProvenance
instance Structured PackageConfig

-- | Newtype wrapper for 'Map' that provides a 'Monoid' instance that takes
-- the last value rather than the first value for overlapping keys.
newtype MapLast k v = MapLast {getMapLast :: Map k v}
  deriving (Eq, Show, Functor, Generic, Binary)

instance (Structured k, Structured v) => Structured (MapLast k v)

instance Ord k => Monoid (MapLast k v) where
  mempty = MapLast Map.empty
  mappend = (<>)

instance Ord k => Semigroup (MapLast k v) where
  MapLast a <> MapLast b = MapLast $ Map.union b a

-- rather than Map.union which is the normal Map monoid instance

-- | Newtype wrapper for 'Map' that provides a 'Monoid' instance that
-- 'mappend's values of overlapping keys rather than taking the first.
newtype MapMappend k v = MapMappend {getMapMappend :: Map k v}
  deriving (Eq, Show, Functor, Generic, Binary)

instance (Structured k, Structured v) => Structured (MapMappend k v)

instance (Semigroup v, Ord k) => Monoid (MapMappend k v) where
  mempty = MapMappend Map.empty
  mappend = (<>)

instance (Semigroup v, Ord k) => Semigroup (MapMappend k v) where
  MapMappend a <> MapMappend b = MapMappend (Map.unionWith (<>) a b)

-- rather than Map.union which is the normal Map monoid instance

instance Monoid ProjectConfig where
  mempty = gmempty
  mappend = (<>)

instance Semigroup ProjectConfig where
  (<>) = gmappend

instance Monoid ProjectConfigBuildOnly where
  mempty = gmempty
  mappend = (<>)

instance Semigroup ProjectConfigBuildOnly where
  (<>) = gmappend

instance Monoid ProjectConfigShared where
  mempty = gmempty
  mappend = (<>)

instance Semigroup ProjectConfigShared where
  (<>) = gmappend

instance Monoid PackageConfig where
  mempty = gmempty
  mappend = (<>)

instance Semigroup PackageConfig where
  (<>) = gmappend

----------------------------------------
-- Resolving configuration to settings
--

-- | Resolved configuration for the solver. The idea is that this is easier to
-- use than the raw configuration because in the raw configuration everything
-- is optional (monoidial). In the 'BuildTimeSettings' every field is filled
-- in, if only with the defaults.
--
-- Use 'resolveSolverSettings' to make one from the project config (by
-- applying defaults etc).
data SolverSettings = SolverSettings
  { solverSettingRemoteRepos :: [RemoteRepo]
  -- ^ Available Hackage servers.
  , solverSettingLocalNoIndexRepos :: [LocalRepo]
  , solverSettingConstraints :: [(UserConstraint, ConstraintSource)]
  , solverSettingPreferences :: [PackageVersionConstraint]
  , solverSettingFlagAssignment :: FlagAssignment
  -- ^ For all local packages
  , solverSettingFlagAssignments :: Map PackageName FlagAssignment
  , solverSettingCabalVersion :: Maybe Version -- TODO: [required eventually] unused
  , solverSettingSolver :: PreSolver
  , solverSettingAllowOlder :: AllowOlder
  , solverSettingAllowNewer :: AllowNewer
  , solverSettingMaxBackjumps :: Maybe Int
  , solverSettingReorderGoals :: ReorderGoals
  , solverSettingCountConflicts :: CountConflicts
  , solverSettingFineGrainedConflicts :: FineGrainedConflicts
  , solverSettingMinimizeConflictSet :: MinimizeConflictSet
  , solverSettingStrongFlags :: StrongFlags
  , solverSettingAllowBootLibInstalls :: AllowBootLibInstalls
  , solverSettingOnlyConstrained :: OnlyConstrained
  , solverSettingIndexState :: Maybe TotalIndexState
  , solverSettingActiveRepos :: Maybe ActiveRepos
  , solverSettingIndependentGoals :: IndependentGoals
  , solverSettingPreferOldest :: PreferOldest
  -- Things that only make sense for manual mode, not --local mode
  -- too much control!
  -- solverSettingShadowPkgs        :: Bool,
  -- solverSettingReinstall         :: Bool,
  -- solverSettingAvoidReinstalls   :: Bool,
  -- solverSettingOverrideReinstall :: Bool,
  -- solverSettingUpgradeDeps       :: Bool
  }
  deriving (Eq, Show, Generic)

instance Binary SolverSettings
instance Structured SolverSettings

-- | Resolved configuration for things that affect how we build and not the
-- value of the things we build. The idea is that this is easier to use than
-- the raw configuration because in the raw configuration everything is
-- optional (monoidial). In the 'BuildTimeSettings' every field is filled in,
-- if only with the defaults.
--
-- Use 'resolveBuildTimeSettings' to make one from the project config (by
-- applying defaults etc).
data BuildTimeSettings = BuildTimeSettings
  { buildSettingDryRun :: Bool
  , buildSettingOnlyDeps :: Bool
  , buildSettingOnlyDownload :: Bool
  , buildSettingSummaryFile :: [PathTemplate]
  , buildSettingLogFile
      :: Maybe
          ( Compiler
            -> Platform
            -> PackageId
            -> UnitId
            -> FilePath
          )
  , buildSettingLogVerbosity :: Verbosity
  , buildSettingBuildReports :: ReportLevel
  , buildSettingReportPlanningFailure :: Bool
  , buildSettingSymlinkBinDir :: [FilePath]
  , buildSettingNumJobs :: ParStratInstall
  , buildSettingKeepGoing :: Bool
  , buildSettingOfflineMode :: Bool
  , buildSettingKeepTempFiles :: Bool
  , buildSettingRemoteRepos :: [RemoteRepo]
  , buildSettingLocalNoIndexRepos :: [LocalRepo]
  , buildSettingCacheDir :: FilePath
  , buildSettingHttpTransport :: Maybe String
  , buildSettingIgnoreExpiry :: Bool
  , buildSettingProgPathExtra :: [FilePath]
  , buildSettingHaddockOpen :: Bool
  }

instance Show BuildTimeSettings where
  show BuildTimeSettings{..} = render $
    (text "BuildTimeSettings {" <+> text (" buildSettingDryRun = " ++ show buildSettingDryRun))
    $+$
    vcat
      [ text (", buildSettingOnlyDeps = " ++ show buildSettingOnlyDeps)
      , text (", buildSettingOnlyDownload = " ++ show buildSettingOnlyDownload)
      , text (", buildSettingSummaryFile = " ++ show buildSettingSummaryFile)
        -- ++ ", buildSettingLogFile = " ++ show buildSettingLogFile
      , text (", buildSettingLogVerbosity = " ++ show buildSettingLogVerbosity)
      , text (", buildSettingBuildReports = " ++ show buildSettingBuildReports)
      , text (", buildSettingReportPlanningFailure = " ++ show buildSettingReportPlanningFailure)
      , text (", buildSettingSymlinkBinDir = " ++ show buildSettingSymlinkBinDir)
      , text (", buildSettingNumJobs = " ++ show buildSettingNumJobs)
      , text (", buildSettingKeepGoing = " ++ show buildSettingKeepGoing)
      , text (", buildSettingOfflineMode = " ++ show buildSettingOfflineMode)
      , text (", buildSettingKeepTempFiles = " ++ show buildSettingKeepTempFiles)
      , text (", buildSettingRemoteRepos = " ++ show buildSettingRemoteRepos)
      , text (", buildSettingLocalNoIndexRepos = " ++ show buildSettingLocalNoIndexRepos)
      ]
