{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module UnitTests.Distribution.Client.ArbitraryInstances
  ( adjustSize
  , shortListOf
  , shortListOf1
  , arbitraryFlag
  , ShortToken (..)
  , arbitraryShortToken
  , NonMEmpty (..)
  , NoShrink (..)

    -- * Shrinker
  , Shrinker
  , runShrinker
  , shrinker
  , shrinkerPP
  , shrinkerAla

    -- * Newtype wrappers
  , PackageLocationString (..)

    -- * Utility
  , runReadP
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Data.Char (isLetter)
import Data.List ((\\))
import Data.Monoid (Last (..))

import Distribution.Simple.Setup
import qualified Data.Map as Map
import qualified Distribution.Deprecated.ReadP as Parse
import System.FilePath (normalise)

import Distribution.PackageDescription
import qualified Distribution.Simple.InstallDirs as InstallDirs
import Distribution.Simple.Program.Db
import Distribution.Simple.Program.Types
import Distribution.System (OS (Windows), buildOS)
import Distribution.Client.BuildReports.Types (BuildReport, InstallOutcome, Outcome, ReportLevel (..))
import Distribution.Client.Glob (FilePathRoot (..), Glob (..), GlobPiece (..), RootedGlob (..))
import Distribution.Client.IndexUtils.ActiveRepos (ActiveRepoEntry (..), ActiveRepos (..), CombineStrategy (..))
import Distribution.Client.IndexUtils.IndexState (RepoIndexState (..), TotalIndexState, makeTotalIndexState)
import Distribution.Client.IndexUtils.Timestamp (Timestamp, epochTimeToTimestamp)
import Distribution.Client.Targets
import Distribution.Client.Types.AllowNewer
import Distribution.Client.Types.OverwritePolicy (OverwritePolicy)
import Distribution.Solver.Types.OptionalStanza (OptionalStanza (..), OptionalStanzaMap, OptionalStanzaSet, optStanzaSetFromList, optStanzaTabulate)
import Distribution.Client.CmdInstall.ClientInstallFlags
import Distribution.Client.Dependency.Types
import Distribution.Client.Types
import Distribution.Client.Types.SourceRepo
import Distribution.Utils.NubList

import Distribution.Solver.Types.ConstraintSource
import Distribution.Solver.Types.PackageConstraint
import Distribution.Solver.Types.ProjectConfigPath
import Distribution.Solver.Types.Settings

import Distribution.Client.ProjectConfig

import Data.Coerce (Coercible, coerce)
import Network.URI (URI (..), URIAuth (..), isUnreserved)
import Test.QuickCheck
  ( Arbitrary (..)
  , Gen
  , NonEmptyList (..)
  , arbitraryBoundedEnum
  , choose
  , elements
  , frequency
  , genericShrink
  , liftArbitrary
  , listOf
  , oneof
  , resize
  , shrinkBoundedEnum
  , sized
  , suchThat
  , vectorOf
  , getPositive
  , getNonNegative
  , Positive (..)
  )
import Test.QuickCheck.GenericArbitrary (genericArbitrary)
import Test.QuickCheck.Instances.Cabal ()

-- note: there are plenty of instances defined in ProjectConfig test file.
-- they should be moved here or into Cabal-quickcheck

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

data Shrinker a = Shrinker a [a]

instance Functor Shrinker where
  fmap f (Shrinker x xs) = Shrinker (f x) (map f xs)

instance Applicative Shrinker where
  pure x = Shrinker x []

  Shrinker f fs <*> Shrinker x xs = Shrinker (f x) (map f xs ++ map ($ x) fs)

runShrinker :: Shrinker a -> [a]
runShrinker (Shrinker _ xs) = xs

shrinker :: Arbitrary a => a -> Shrinker a
shrinker x = Shrinker x (shrink x)

shrinkerAla :: (Coercible a b, Arbitrary b) => (a -> b) -> a -> Shrinker a
shrinkerAla pack = shrinkerPP pack coerce

-- | shrinker with pre and post functions.
shrinkerPP :: Arbitrary b => (a -> b) -> (b -> a) -> a -> Shrinker a
shrinkerPP pack unpack x = Shrinker x (map unpack (shrink (pack x)))

-------------------------------------------------------------------------------
-- Non-Cabal instances
-------------------------------------------------------------------------------

instance Arbitrary URI where
  arbitrary =
    URI
      <$> elements ["file:", "http:", "https:"]
      <*> (Just <$> arbitrary)
      <*> (('/' :) <$> arbitraryURIToken)
      <*> (('?' :) <$> arbitraryURIToken)
      <*> pure ""

instance Arbitrary URIAuth where
  arbitrary =
    pure (URIAuth "") -- no password as this does not roundtrip
      <*> arbitraryURIToken
      <*> arbitraryURIPort

arbitraryURIToken :: Gen String
arbitraryURIToken =
  shortListOf1 6 (elements (filter isUnreserved ['\0' .. '\255']))

arbitraryURIPort :: Gen String
arbitraryURIPort =
  oneof [pure "", (':' :) <$> shortListOf1 4 (choose ('0', '9'))]

-------------------------------------------------------------------------------
-- cabal-install (and Cabal) types
-------------------------------------------------------------------------------

adjustSize :: (Int -> Int) -> Gen a -> Gen a
adjustSize adjust gen = sized (\n -> resize (adjust n) gen)

shortListOf :: Int -> Gen a -> Gen [a]
shortListOf bound gen =
  sized $ \n -> do
    k <- choose (0, (n `div` 2) `min` bound)
    vectorOf k gen

shortListOf1 :: Int -> Gen a -> Gen [a]
shortListOf1 bound gen =
  sized $ \n -> do
    k <- choose (1, 1 `max` ((n `div` 2) `min` bound))
    vectorOf k gen

newtype ShortToken = ShortToken {getShortToken :: String}
  deriving (Show)

instance Arbitrary ShortToken where
  arbitrary =
    ShortToken
      <$> ( shortListOf1 5 (choose ('#', '~'))
              `suchThat` all (`notElem` "{}")
              `suchThat` (not . ("[]" `isPrefixOf`))
          )

  -- TODO: [code cleanup] need to replace parseHaskellString impl to stop
  -- accepting Haskell list syntax [], ['a'] etc, just allow String syntax.
  -- Workaround, don't generate [] as this does not round trip.

  shrink (ShortToken cs) =
    [ShortToken cs' | cs' <- shrink cs, not (null cs')]

arbitraryShortToken :: Gen String
arbitraryShortToken = getShortToken <$> arbitrary

newtype NonMEmpty a = NonMEmpty {getNonMEmpty :: a}
  deriving (Eq, Ord, Show)

instance (Arbitrary a, Monoid a, Eq a) => Arbitrary (NonMEmpty a) where
  arbitrary = NonMEmpty <$> (arbitrary `suchThat` (/= mempty))
  shrink (NonMEmpty x) = [NonMEmpty x' | x' <- shrink x, x' /= mempty]

newtype NoShrink a = NoShrink {getNoShrink :: a}
  deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (NoShrink a) where
  arbitrary = NoShrink <$> arbitrary
  shrink _ = []

instance Arbitrary Timestamp where
  -- note: no negative timestamps
  --
  -- >>> utcTimeToPOSIXSeconds $ UTCTime (fromGregorian 100000 01 01) 0
  -- >>> 3093527980800s
  --
  arbitrary = epochTimeToTimestamp . (`mod` 3093527980800) . abs <$> arbitrary

instance Arbitrary RepoIndexState where
  arbitrary =
    frequency
      [ (1, pure IndexStateHead)
      , (50, IndexStateTime <$> arbitrary)
      ]

instance Arbitrary TotalIndexState where
  arbitrary = makeTotalIndexState <$> arbitrary <*> arbitrary

instance Arbitrary WriteGhcEnvironmentFilesPolicy where
  arbitrary = arbitraryBoundedEnum

arbitraryFlag :: Gen a -> Gen (Flag a)
arbitraryFlag = fmap (fmap Last) liftArbitrary

instance Arbitrary RepoName where
  -- TODO: rename refinement?
  arbitrary = RepoName <$> (mk `suchThat` \x -> not $ "--" `isPrefixOf` x)
    where
      mk = (:) <$> lead <*> rest
      lead =
        elements
          [c | c <- ['\NUL' .. '\255'], isAlpha c || c `elem` "_-."]
      rest =
        listOf
          ( elements
              [c | c <- ['\NUL' .. '\255'], isAlphaNum c || c `elem` "_-."]
          )

instance Arbitrary ReportLevel where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary OverwritePolicy where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary InstallMethod where
  arbitrary = arbitraryBoundedEnum

-------------------------------------------------------------------------------
-- ActiveRepos
-------------------------------------------------------------------------------

instance Arbitrary ActiveRepos where
  arbitrary = ActiveRepos <$> shortListOf 5 arbitrary

instance Arbitrary ActiveRepoEntry where
  arbitrary =
    frequency
      [ (10, ActiveRepo <$> arbitrary <*> arbitrary)
      , (1, ActiveRepoRest <$> arbitrary)
      ]

instance Arbitrary CombineStrategy where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

-------------------------------------------------------------------------------
-- AllowNewer
-------------------------------------------------------------------------------

instance Arbitrary AllowNewer where
  arbitrary = AllowNewer <$> arbitrary

instance Arbitrary AllowOlder where
  arbitrary = AllowOlder <$> arbitrary

instance Arbitrary RelaxDeps where
  arbitrary =
    oneof
      [ pure mempty
      , mkRelaxDepSome <$> shortListOf1 3 arbitrary
      , pure RelaxDepsAll
      ]

instance Arbitrary RelaxDepMod where
  arbitrary = elements [RelaxDepModNone, RelaxDepModCaret]

  shrink RelaxDepModCaret = [RelaxDepModNone]
  shrink _ = []

instance Arbitrary RelaxDepScope where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary RelaxDepSubject where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary RelaxedDep where
  arbitrary = genericArbitrary
  shrink = genericShrink

-------------------------------------------------------------------------------
-- UserConstraint
-------------------------------------------------------------------------------

instance Arbitrary UserConstraintScope where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary UserQualifier where
  arbitrary =
    oneof
      [ pure UserQualToplevel
      , UserQualSetup <$> arbitrary
      -- -- TODO: Re-enable UserQualExe tests once we decide on a syntax.
      -- , UserQualExe <$> arbitrary <*> arbitrary
      ]

instance Arbitrary UserConstraint where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary PackageProperty where
  arbitrary =
    oneof
      [ PackagePropertyVersion <$> arbitrary
      , pure PackagePropertyInstalled
      , pure PackagePropertySource
      , PackagePropertyFlags . mkFlagAssignment <$> shortListOf1 3 arbitrary
      , PackagePropertyStanzas . (\x -> [x]) <$> arbitrary
      ]

instance Arbitrary OptionalStanza where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary OptionalStanzaSet where
  arbitrary = fmap optStanzaSetFromList arbitrary

instance Arbitrary a => Arbitrary (OptionalStanzaMap a) where
  arbitrary = do
    x1 <- arbitrary
    x2 <- arbitrary
    return $ optStanzaTabulate $ \case
      TestStanzas -> x1
      BenchStanzas -> x2

-------------------------------------------------------------------------------
-- BuildReport
-------------------------------------------------------------------------------

instance Arbitrary BuildReport where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary InstallOutcome where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Outcome where
  arbitrary = genericArbitrary
  shrink = genericShrink

-------------------------------------------------------------------------------
-- Glob
-------------------------------------------------------------------------------

instance Arbitrary RootedGlob where
  arbitrary =
    (RootedGlob <$> arbitrary <*> arbitrary)
      `suchThat` validFilePathGlob

  shrink (RootedGlob root pathglob) =
    [ RootedGlob root' pathglob'
    | (root', pathglob') <- shrink (root, pathglob)
    , validFilePathGlob (RootedGlob root' pathglob')
    ]

validFilePathGlob :: RootedGlob -> Bool
validFilePathGlob (RootedGlob FilePathRelative pathglob) =
  case pathglob of
    GlobDirTrailing -> False
    GlobDir [Literal "~"] _ -> False
    GlobDir [Literal (d : ":")] _
      | isLetter d -> False
    _ -> True
validFilePathGlob _ = True

instance Arbitrary FilePathRoot where
  arbitrary =
    frequency
      [ (3, pure FilePathRelative)
      , (1, pure (FilePathRoot unixroot))
      , (1, FilePathRoot <$> windrive)
      , (1, pure FilePathHomeDir)
      ]
    where
      unixroot = "/"
      windrive = do d <- choose ('A', 'Z'); return (d : ":\\")

  shrink FilePathRelative = []
  shrink (FilePathRoot _) = [FilePathRelative]
  shrink FilePathHomeDir = [FilePathRelative]

instance Arbitrary Glob where
  arbitrary = sized $ \sz ->
    oneof $
      take
        (max 1 sz)
        [ pure GlobDirTrailing
        , GlobFile . getGlobPieces <$> arbitrary
        , (GlobDir . getGlobPieces <$> arbitrary)
            <*> resize (sz `div` 2) arbitrary
        ]

  shrink GlobDirTrailing = []
  shrink (GlobFile glob) =
    GlobDirTrailing
      : [GlobFile (getGlobPieces glob') | glob' <- shrink (GlobPieces glob)]
  shrink (GlobDir glob pathglob) =
    pathglob
      : GlobFile glob
      : [ GlobDir (getGlobPieces glob') pathglob'
        | (glob', pathglob') <- shrink (GlobPieces glob, pathglob)
        ]
  shrink (GlobDirRecursive glob) =
    GlobDirTrailing
      : [GlobFile (getGlobPieces glob') | glob' <- shrink (GlobPieces glob)]

newtype GlobPieces = GlobPieces {getGlobPieces :: [GlobPiece]}
  deriving (Eq)

instance Arbitrary GlobPieces where
  arbitrary = GlobPieces . mergeLiterals <$> shortListOf1 5 arbitrary

  shrink (GlobPieces glob) =
    [ GlobPieces (mergeLiterals (getNonEmpty glob'))
    | glob' <- shrink (NonEmpty glob)
    ]

mergeLiterals :: [GlobPiece] -> [GlobPiece]
mergeLiterals (Literal a : Literal b : ps) = mergeLiterals (Literal (a ++ b) : ps)
mergeLiterals (Union as : ps) = Union (map mergeLiterals as) : mergeLiterals ps
-- Two consecutive wildcards are semantically equivalent to a single one, but
-- would syntactically produce a recursive wildcard when pretty-printed, so
-- whenever we end up generating two or more consecutive wildcards, we merge
-- them together to avoid this problem.
mergeLiterals (WildCard : WildCard : ps) = mergeLiterals (WildCard : ps)
mergeLiterals (p : ps) = p : mergeLiterals ps
mergeLiterals [] = []

instance Arbitrary GlobPiece where
  arbitrary = sized $ \sz ->
    frequency
      [ (3, Literal <$> shortListOf1 10 (elements globLiteralChars))
      , (1, pure WildCard)
      , (1, Union <$> resize (sz `div` 2) (shortListOf1 5 (shortListOf1 5 arbitrary)))
      ]

  shrink (Literal str) =
    [ Literal str'
    | str' <- shrink str
    , not (null str')
    , all (`elem` globLiteralChars) str'
    ]
  shrink WildCard = []
  shrink (Union as) =
    [ Union (map getGlobPieces (getNonEmpty as'))
    | as' <- shrink (NonEmpty (map GlobPieces as))
    ]

globLiteralChars :: [Char]
globLiteralChars = ['\0' .. '\128'] \\ "*{},/\\"

instance Arbitrary ProjectConfig where
  arbitrary =
    (ProjectConfig . map getPackageLocationString <$> arbitrary)
      <*> (map getPackageLocationString <$> arbitrary)
      <*> shortListOf 3 arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> ( MapMappend . fmap getNonMEmpty . Map.fromList
              <$> shortListOf 3 arbitrary
          )

  -- package entries with no content are equivalent to
  -- the entry not existing at all, so exclude empty

  shrink
    ProjectConfig
      { projectPackages = x0
      , projectPackagesOptional = x1
      , projectPackagesRepo = x2
      , projectPackagesNamed = x3
      , projectConfigBuildOnly = x4
      , projectConfigShared = x5
      , projectConfigProvenance = x6
      , projectConfigLocalPackages = x7
      , projectConfigSpecificPackage = x8
      , projectConfigAllPackages = x9
      } =
      [ ProjectConfig
        { projectPackages = x0'
        , projectPackagesOptional = x1'
        , projectPackagesRepo = x2'
        , projectPackagesNamed = x3'
        , projectConfigBuildOnly = x4'
        , projectConfigShared = x5'
        , projectConfigProvenance = x6'
        , projectConfigLocalPackages = x7'
        , projectConfigSpecificPackage =
            MapMappend
              (fmap getNonMEmpty x8')
        , projectConfigAllPackages = x9'
        }
      | ((x0', x1', x2', x3'), (x4', x5', x6', x7', x8', x9')) <-
          shrink
            ( (x0, x1, x2, x3)
            , (x4, x5, x6, x7, fmap NonMEmpty (getMapMappend x8), x9)
            )
      ]

newtype PackageLocationString = PackageLocationString {getPackageLocationString :: String}
  deriving (Show)

instance Arbitrary PackageLocationString where
  arbitrary =
    PackageLocationString
      <$> oneof
        [ show . getNonEmpty <$> (arbitrary :: Gen (NonEmptyList String))
        , arbitraryGlobLikeStr
        , show <$> (arbitrary :: Gen URI)
        ]
        `suchThat` (\xs -> not ("{" `isPrefixOf` xs))

arbitraryGlobLikeStr :: Gen String
arbitraryGlobLikeStr = outerTerm
  where
    outerTerm =
      concat
        <$> shortListOf1
          4
          ( frequency
              [ (2, token)
              , (1, braces <$> innerTerm)
              ]
          )
    innerTerm =
      intercalate ","
        <$> shortListOf1
          3
          ( frequency
              [ (3, token)
              , (1, braces <$> innerTerm)
              ]
          )
    token = shortListOf1 4 (elements (['#' .. '~'] \\ "{,}"))
    braces s = "{" ++ s ++ "}"

instance Arbitrary ClientInstallFlags where
  arbitrary =
    ClientInstallFlags
      <$> arbitrary
      <*> arbitraryFlag arbitraryShortToken
      <*> arbitrary
      <*> arbitrary
      <*> arbitraryFlag arbitraryShortToken

instance Arbitrary ProjectConfigBuildOnly where
  arbitrary =
    ProjectConfigBuildOnly
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> (toNubList <$> shortListOf 2 arbitrary)
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> (fmap getShortToken <$> arbitrary)
      <*> arbitraryNumJobs
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> (fmap getShortToken <$> arbitrary)
      <*> arbitrary
      <*> (fmap getShortToken <$> arbitrary)
      <*> (fmap getShortToken <$> arbitrary)
      <*> arbitrary
      <*> arbitrary
    where
      arbitraryNumJobs = fmap (fmap getPositive) <$> arbitrary

  shrink
    ProjectConfigBuildOnly
      { projectConfigVerbosity = x00
      , projectConfigDryRun = x01
      , projectConfigOnlyDeps = x02
      , projectConfigOnlyDownload = x18
      , projectConfigSummaryFile = x03
      , projectConfigLogFile = x04
      , projectConfigBuildReports = x05
      , projectConfigReportPlanningFailure = x06
      , projectConfigSymlinkBinDir = x07
      , projectConfigNumJobs = x09
      , projectConfigUseSemaphore = x19
      , projectConfigKeepGoing = x10
      , projectConfigOfflineMode = x11
      , projectConfigKeepTempFiles = x12
      , projectConfigHttpTransport = x13
      , projectConfigIgnoreExpiry = x14
      , projectConfigCacheDir = x15
      , projectConfigLogsDir = x16
      , projectConfigClientInstallFlags = x17
      , projectConfigBuildTimings = x20
      } =
      [ ProjectConfigBuildOnly
        { projectConfigVerbosity = x00'
        , projectConfigDryRun = x01'
        , projectConfigOnlyDeps = x02'
        , projectConfigOnlyDownload = x18'
        , projectConfigSummaryFile = x03'
        , projectConfigLogFile = x04'
        , projectConfigBuildReports = x05'
        , projectConfigReportPlanningFailure = x06'
        , projectConfigSymlinkBinDir = x07'
        , projectConfigNumJobs = postShrink_NumJobs x09'
        , projectConfigUseSemaphore = x19'
        , projectConfigKeepGoing = x10'
        , projectConfigOfflineMode = x11'
        , projectConfigKeepTempFiles = x12'
        , projectConfigHttpTransport = x13
        , projectConfigIgnoreExpiry = x14'
        , projectConfigCacheDir = x15
        , projectConfigLogsDir = x16
        , projectConfigClientInstallFlags = x17'
        , projectConfigBuildTimings = x20'
        }
      | ( (x00', x01', x02', x03', x04')
          , (x05', x06', x07', x09')
          , (x10', x11', x12', x14')
          , (x17', x18', x19', x20')
          ) <-
          shrink
            ( (x00, x01, x02, x03, x04)
            , (x05, x06, x07, preShrink_NumJobs x09)
            , (x10, x11, x12, x14)
            , (x17, x18, x19, x20)
            )
      ]
      where
        preShrink_NumJobs = fmap (fmap Positive)
        postShrink_NumJobs = fmap (fmap getPositive)

instance Arbitrary ProjectConfigShared where
  arbitrary = do
    projectConfigDistDir <- arbitraryFlag arbitraryShortToken
    projectConfigConfigFile <- arbitraryFlag arbitraryShortToken
    projectConfigProjectDir <- arbitraryFlag arbitraryShortToken
    projectConfigProjectFile <- arbitraryFlag arbitraryShortToken
    projectConfigProjectFileParser <- arbitraryFlag arbitrary
    projectConfigIgnoreProject <- arbitrary
    projectConfigHcFlavor <- arbitrary
    projectConfigHcPath <- arbitraryFlag arbitraryShortToken
    projectConfigHcPkg <- arbitraryFlag arbitraryShortToken
    projectConfigHaddockIndex <- arbitrary
    projectConfigInstallDirs <- fixInstallDirs <$> arbitrary
    projectConfigPackageDBs <- shortListOf 2 arbitrary
    projectConfigRemoteRepos <- arbitrary
    projectConfigLocalNoIndexRepos <- arbitrary
    projectConfigActiveRepos <- arbitrary
    projectConfigIndexState <- arbitrary
    projectConfigStoreDir <- arbitraryFlag arbitraryShortToken
    projectConfigConstraints <- arbitraryConstraints
    projectConfigPreferences <- shortListOf 2 arbitrary
    projectConfigCabalVersion <- arbitrary
    projectConfigSolver <- arbitrary
    projectConfigAllowOlder <- arbitrary
    projectConfigAllowNewer <- arbitrary
    projectConfigWriteGhcEnvironmentFilesPolicy <- arbitrary
    projectConfigMaxBackjumps <- arbitrary
    projectConfigReorderGoals <- arbitrary
    projectConfigCountConflicts <- arbitrary
    projectConfigFineGrainedConflicts <- arbitrary
    projectConfigMinimizeConflictSet <- arbitrary
    projectConfigStrongFlags <- arbitrary
    projectConfigAllowBootLibInstalls <- arbitrary
    projectConfigOnlyConstrained <- arbitrary
    projectConfigPerComponent <- arbitrary
    projectConfigIndependentGoals <- arbitrary
    projectConfigPreferVersion <- arbitrary
    projectConfigProgPathExtra <- toNubList <$> listOf arbitraryShortToken
    projectConfigMultiRepl <- arbitrary
    return ProjectConfigShared{..}
    where
      arbitraryConstraints :: Gen [(UserConstraint, ConstraintSource)]
      arbitraryConstraints =
        fmap (,projectConfigConstraintSource) <$> arbitrary
      fixInstallDirs x = x{InstallDirs.includedir = mempty, InstallDirs.mandir = mempty, InstallDirs.flibdir = mempty}

  shrink ProjectConfigShared{..} =
    runShrinker $
      pure ProjectConfigShared
        <*> shrinker projectConfigDistDir
        <*> shrinker projectConfigConfigFile
        <*> shrinker projectConfigProjectDir
        <*> shrinker projectConfigProjectFile
        <*> shrinker projectConfigProjectFileParser
        <*> shrinker projectConfigIgnoreProject
        <*> shrinker projectConfigHcFlavor
        <*> shrinkerAla (fmap NonEmpty) projectConfigHcPath
        <*> shrinkerAla (fmap NonEmpty) projectConfigHcPkg
        <*> shrinker projectConfigHaddockIndex
        <*> shrinker projectConfigInstallDirs
        <*> shrinker projectConfigPackageDBs
        <*> shrinker projectConfigRemoteRepos
        <*> shrinker projectConfigLocalNoIndexRepos
        <*> shrinker projectConfigActiveRepos
        <*> shrinker projectConfigIndexState
        <*> shrinker projectConfigStoreDir
        <*> shrinkerPP preShrink_Constraints postShrink_Constraints projectConfigConstraints
        <*> shrinker projectConfigPreferences
        <*> shrinker projectConfigCabalVersion
        <*> shrinker projectConfigSolver
        <*> shrinker projectConfigAllowOlder
        <*> shrinker projectConfigAllowNewer
        <*> shrinker projectConfigWriteGhcEnvironmentFilesPolicy
        <*> shrinker projectConfigMaxBackjumps
        <*> shrinker projectConfigReorderGoals
        <*> shrinker projectConfigCountConflicts
        <*> shrinker projectConfigFineGrainedConflicts
        <*> shrinker projectConfigMinimizeConflictSet
        <*> shrinker projectConfigStrongFlags
        <*> shrinker projectConfigAllowBootLibInstalls
        <*> shrinker projectConfigOnlyConstrained
        <*> shrinker projectConfigPerComponent
        <*> shrinker projectConfigIndependentGoals
        <*> shrinker projectConfigPreferVersion
        <*> shrinker projectConfigProgPathExtra
        <*> shrinker projectConfigMultiRepl
    where
      preShrink_Constraints = map fst
      postShrink_Constraints = map (,projectConfigConstraintSource)

projectConfigConstraintSource :: ConstraintSource
projectConfigConstraintSource = ConstraintSourceProjectConfig nullProjectConfigPath

instance Arbitrary ProjectFileParser where
  arbitrary = elements [ParsecParser, LegacyParser, FallbackParser, CompareParser]

instance Arbitrary ProjectConfigProvenance where
  arbitrary = elements [Implicit, Explicit (ProjectConfigPath $ "cabal.project" :| [])]

instance Arbitrary PackageConfig where
  arbitrary =
    ( PackageConfig . MapLast . Map.fromList
        <$> shortListOf
          10
          ( (,)
              <$> arbitraryProgramName
              <*> arbitraryShortToken
          )
    )
      <*> ( MapMappend . Map.fromList
              <$> shortListOf
                10
                ( (,)
                    <$> arbitraryProgramName
                    <*> listOf arbitraryShortToken
                )
          )
      <*> (toNubList <$> listOf arbitraryShortToken)
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> shortListOf 5 arbitraryShortToken
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> shortListOf 5 arbitraryShortToken
      <*> shortListOf 5 arbitraryShortToken
      <*> shortListOf 5 arbitraryShortToken
      <*> shortListOf 5 arbitraryShortToken
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitraryFlag arbitraryShortToken
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitraryFlag arbitraryShortToken
      <*> arbitrary
      <*> arbitrary
      <*> arbitraryFlag arbitraryShortToken
      <*> arbitrary
      <*> arbitrary
      <*> arbitraryFlag arbitraryShortToken
      <*> arbitraryFlag arbitraryShortToken
      <*> arbitraryFlag arbitraryShortToken
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitraryFlag arbitraryShortToken
      <*> arbitrary
      <*> shortListOf 5 arbitrary
      <*> shortListOf 5 arbitrary
    where
      arbitraryProgramName :: Gen String
      arbitraryProgramName =
        elements
          [ programName prog
          | (prog, _) <- knownPrograms defaultProgramDb
          ]

  shrink
    PackageConfig
      { packageConfigProgramPaths = x00
      , packageConfigProgramArgs = x01
      , packageConfigProgramPathExtra = x02
      , packageConfigFlagAssignment = x03
      , packageConfigVanillaLib = x04
      , packageConfigSharedLib = x05
      , packageConfigStaticLib = x42
      , packageConfigBytecodeLib = x43
      , packageConfigDynExe = x06
      , packageConfigFullyStaticExe = x50
      , packageConfigProf = x07
      , packageConfigProfLib = x08
      , packageConfigProfShared = x08_1
      , packageConfigProfExe = x09
      , packageConfigProfDetail = x10
      , packageConfigProfLibDetail = x11
      , packageConfigConfigureArgs = x12
      , packageConfigOptimization = x13
      , packageConfigProgPrefix = x14
      , packageConfigProgSuffix = x15
      , packageConfigExtraLibDirs = x16
      , packageConfigExtraLibDirsStatic = x53
      , packageConfigExtraFrameworkDirs = x17
      , packageConfigExtraIncludeDirs = x18
      , packageConfigGHCiLib = x19
      , packageConfigSplitSections = x20
      , packageConfigSplitObjs = x20_1
      , packageConfigStripExes = x21
      , packageConfigStripLibs = x22
      , packageConfigTests = x23
      , packageConfigBenchmarks = x24
      , packageConfigCoverage = x25
      , packageConfigRelocatable = x26
      , packageConfigDebugInfo = x27
      , packageConfigDumpBuildInfo = x27_1
      , packageConfigRunTests = x28
      , packageConfigDocumentation = x29
      , packageConfigHaddockHoogle = x30
      , packageConfigHaddockHtml = x31
      , packageConfigHaddockHtmlLocation = x32
      , packageConfigHaddockForeignLibs = x33
      , packageConfigHaddockExecutables = x33_1
      , packageConfigHaddockTestSuites = x34
      , packageConfigHaddockBenchmarks = x35
      , packageConfigHaddockInternal = x36
      , packageConfigHaddockCss = x37
      , packageConfigHaddockLinkedSource = x38
      , packageConfigHaddockQuickJump = x59
      , packageConfigHaddockHscolourCss = x39
      , packageConfigHaddockContents = x40
      , packageConfigHaddockForHackage = x41
      , packageConfigHaddockIndex = x54
      , packageConfigHaddockBaseUrl = x55
      , packageConfigHaddockResourcesDir = x56
      , packageConfigHaddockOutputDir = x57
      , packageConfigHaddockUseUnicode = x58
      , packageConfigTestHumanLog = x44
      , packageConfigTestMachineLog = x45
      , packageConfigTestShowDetails = x46
      , packageConfigTestKeepTix = x47
      , packageConfigTestWrapper = x48
      , packageConfigTestFailWhenNoTestSuites = x49
      , packageConfigTestTestOptions = x51
      , packageConfigBenchmarkOptions = x52
      } =
      [ PackageConfig
        { packageConfigProgramPaths = postShrink_Paths x00'
        , packageConfigProgramArgs = postShrink_Args x01'
        , packageConfigProgramPathExtra = x02'
        , packageConfigFlagAssignment = x03'
        , packageConfigVanillaLib = x04'
        , packageConfigSharedLib = x05'
        , packageConfigStaticLib = x42'
        , packageConfigBytecodeLib = x43'
        , packageConfigDynExe = x06'
        , packageConfigFullyStaticExe = x50'
        , packageConfigProf = x07'
        , packageConfigProfLib = x08'
        , packageConfigProfShared = x08_1'
        , packageConfigProfExe = x09'
        , packageConfigProfDetail = x10'
        , packageConfigProfLibDetail = x11'
        , packageConfigConfigureArgs = map getNonEmpty x12'
        , packageConfigOptimization = x13'
        , packageConfigProgPrefix = x14'
        , packageConfigProgSuffix = x15'
        , packageConfigExtraLibDirs = map getNonEmpty x16'
        , packageConfigExtraLibDirsStatic = map getNonEmpty x53'
        , packageConfigExtraFrameworkDirs = map getNonEmpty x17'
        , packageConfigExtraIncludeDirs = map getNonEmpty x18'
        , packageConfigGHCiLib = x19'
        , packageConfigSplitSections = x20'
        , packageConfigSplitObjs = x20_1'
        , packageConfigStripExes = x21'
        , packageConfigStripLibs = x22'
        , packageConfigTests = x23'
        , packageConfigBenchmarks = x24'
        , packageConfigCoverage = x25'
        , packageConfigRelocatable = x26'
        , packageConfigDebugInfo = x27'
        , packageConfigDumpBuildInfo = x27_1'
        , packageConfigRunTests = x28'
        , packageConfigDocumentation = x29'
        , packageConfigHaddockHoogle = x30'
        , packageConfigHaddockHtml = x31'
        , packageConfigHaddockHtmlLocation = x32'
        , packageConfigHaddockForeignLibs = x33'
        , packageConfigHaddockExecutables = x33_1'
        , packageConfigHaddockTestSuites = x34'
        , packageConfigHaddockBenchmarks = x35'
        , packageConfigHaddockInternal = x36'
        , packageConfigHaddockCss = fmap getNonEmpty x37'
        , packageConfigHaddockLinkedSource = x38'
        , packageConfigHaddockQuickJump = x59'
        , packageConfigHaddockHscolourCss = fmap getNonEmpty x39'
        , packageConfigHaddockContents = x40'
        , packageConfigHaddockForHackage = x41'
        , packageConfigHaddockIndex = x54'
        , packageConfigHaddockBaseUrl = x55'
        , packageConfigHaddockResourcesDir = x56'
        , packageConfigHaddockOutputDir = x57'
        , packageConfigHaddockUseUnicode = x58'
        , packageConfigTestHumanLog = x44'
        , packageConfigTestMachineLog = x45'
        , packageConfigTestShowDetails = x46'
        , packageConfigTestKeepTix = x47'
        , packageConfigTestWrapper = x48'
        , packageConfigTestFailWhenNoTestSuites = x49'
        , packageConfigTestTestOptions = x51'
        , packageConfigBenchmarkOptions = x52'
        }
      | ( ( (x00', x01', x02', x03', x04')
            , (x05', x42', x43', x06', x50', x07', x08', x08_1', x09')
            , (x10', x11', x12', x13', x14')
            , (x15', x16', x53', x17', x18', x19')
            )
          , ( (x20', x20_1', x21', x22', x23', x24')
              , (x25', x26', x27', x27_1', x28', x29')
              , (x30', x31', x32', (x33', x33_1'), x34')
              , (x35', x36', x37', x38', x59', x39')
              , (x40', x41')
              , (x44', x45', x46', x47', x48', x49', x51', x52', x54', x55')
              , x56'
              , x57'
              , x58'
              )
          ) <-
          shrink
            (
              ( (preShrink_Paths x00, preShrink_Args x01, x02, x03, x04)
              , (x05, x42, x43, x06, x50, x07, x08, x08_1, x09)
              , (x10, x11, map NonEmpty x12, x13, x14)
              ,
                ( x15
                , map NonEmpty x16
                , map NonEmpty x53
                , map NonEmpty x17
                , map NonEmpty x18
                , x19
                )
              )
            ,
              ( (x20, x20_1, x21, x22, x23, x24)
              , (x25, x26, x27, x27_1, x28, x29)
              , (x30, x31, x32, (x33, x33_1), x34)
              , (x35, x36, fmap NonEmpty x37, x38, x59, fmap NonEmpty x39)
              , (x40, x41)
              , (x44, x45, x46, x47, x48, x49, x51, x52, x54, x55)
              , x56
              , x57
              , x58
              )
            )
      ]
      where
        preShrink_Paths =
          Map.map NonEmpty
            . Map.mapKeys NoShrink
            . getMapLast
        postShrink_Paths =
          MapLast
            . Map.map getNonEmpty
            . Map.mapKeys getNoShrink
        preShrink_Args =
          Map.map (NonEmpty . map NonEmpty)
            . Map.mapKeys NoShrink
            . getMapMappend
        postShrink_Args =
          MapMappend
            . Map.map (map getNonEmpty . getNonEmpty)
            . Map.mapKeys getNoShrink

instance f ~ [] => Arbitrary (SourceRepositoryPackage f) where
  arbitrary =
    SourceRepositoryPackage
      <$> arbitrary
      <*> (getShortToken <$> arbitrary)
      <*> (fmap getShortToken <$> arbitrary)
      <*> (fmap getShortToken <$> arbitrary)
      <*> (fmap getShortToken <$> shortListOf 3 arbitrary)
      <*> (fmap getShortToken <$> shortListOf 3 arbitrary)

  shrink SourceRepositoryPackage{..} =
    runShrinker $
      pure SourceRepositoryPackage
        <*> shrinker srpType
        <*> shrinkerAla ShortToken srpLocation
        <*> shrinkerAla (fmap ShortToken) srpTag
        <*> shrinkerAla (fmap ShortToken) srpBranch
        <*> shrinkerAla (fmap ShortToken) srpSubdir
        <*> shrinkerAla (fmap ShortToken) srpCommand

instance Arbitrary RemoteRepo where
  arbitrary =
    RemoteRepo
      <$> arbitrary
      <*> arbitrary -- URI
      <*> arbitrary
      <*> listOf arbitraryRootKey
      <*> fmap getNonNegative arbitrary
      <*> pure False
    where
      arbitraryRootKey =
        shortListOf1
          5
          ( oneof
              [ choose ('0', '9')
              , choose ('a', 'f')
              ]
          )

instance Arbitrary LocalRepo where
  arbitrary =
    LocalRepo
      <$> arbitrary
      <*> elements
        ( (if buildOS == Windows then map (normalise . ("C:" ++)) else id)
            ["/tmp/foo", "/tmp/bar"]
        ) -- TODO: generate valid absolute paths
      <*> arbitrary

instance Arbitrary PreSolver where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary ReorderGoals where
  arbitrary = ReorderGoals <$> arbitrary

instance Arbitrary CountConflicts where
  arbitrary = CountConflicts <$> arbitrary

instance Arbitrary FineGrainedConflicts where
  arbitrary = FineGrainedConflicts <$> arbitrary

instance Arbitrary MinimizeConflictSet where
  arbitrary = MinimizeConflictSet <$> arbitrary

instance Arbitrary IndependentGoals where
  arbitrary = IndependentGoals <$> arbitrary

instance Arbitrary PreferVersion where
  arbitrary =
    oneof
      [ pure PreferOldest
      , pure PreferLatest
      , pure PreferInstalledOrLatest
      ]

instance Arbitrary StrongFlags where
  arbitrary = StrongFlags <$> arbitrary

instance Arbitrary AllowBootLibInstalls where
  arbitrary = AllowBootLibInstalls <$> arbitrary

instance Arbitrary OnlyConstrained where
  arbitrary =
    oneof
      [ pure OnlyConstrainedAll
      , pure OnlyConstrainedNone
      ]

-- | Helper to parse a given string
--
-- Succeeds only if there is a unique complete parse
runReadP :: Parse.ReadP a -> String -> Maybe a
runReadP parser s = case [x | (x, "") <- Parse.readP_to_S parser s] of
  [x'] -> Just x'
  _ -> Nothing
