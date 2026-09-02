{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Resolving @[TARGETS]@ into the fully-qualified target forms that
-- @cabal target@ reports, and a cheaper oracle for asking whether a single
-- string names a target at all.
--
-- The expensive half ('resolveTargetForms') needs a full 'ElaboratedInstallPlan'
-- and so runs the solver. The cheap half ('newTargetOracle') needs only the
-- project's local packages, which is enough to answer \"is this a target?\" and
-- is therefore usable before a plan exists.
module Distribution.Client.TargetForms
  ( -- * Full resolution
    resolveTargetForms
  , printTargetForms

    -- * Probing individual strings
  , TargetMatch (..)
  , TargetOracle (..)
  , newTargetOracle
  , knownTargetOracle

    -- * Diagnostics
  , runnableComponents
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import qualified Data.Map as Map
import Distribution.Client.CmdErrorMessages
  ( reportTargetProblems
  )
import Distribution.Client.InstallPlan
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.NixStyleOptions
  ( NixStyleFlags (..)
  )
import Distribution.Client.ProjectOrchestration
import Distribution.Client.ProjectPlanning
import Distribution.Client.Setup
  ( GlobalFlags
  )
import Distribution.Client.TargetProblem
  ( TargetProblem'
  )
import Distribution.Client.TargetSelector
  ( ComponentKindFilter
  , DirActions (..)
  , getKnownTargets
  , getTargetStringFileStatus
  , parseTargetString
  , resolveTargetSelector
  )
import Distribution.Client.Types
  ( PackageSpecifier
  , UnresolvedSourcePackage
  )
import Distribution.Package
import Distribution.Simple.Utils
  ( noticeDoc
  , safeHead
  , sortNub
  )
import Text.PrettyPrint
import qualified Text.PrettyPrint as Pretty

-------------------------------------------------------------------------------
-- Full resolution
-------------------------------------------------------------------------------

-- | The pipeline behind @cabal target@: establish the project, build the
-- install plan, read the target selectors and resolve them into a
-- 'TargetsMap'.
--
-- The two selection functions are taken as arguments rather than imported so
-- that this module does not depend on any @Cmd*@ module; callers pass the pair
-- their command already uses.
resolveTargetForms
  :: Verbosity
  -> (forall k. TargetSelector -> [AvailableTarget k] -> Either TargetProblem' [k])
  -> (forall k. SubComponentTarget -> AvailableTarget k -> Either TargetProblem' k)
  -> NixStyleFlags ()
  -> GlobalFlags
  -> [String]
  -> IO (TargetsMap, ElaboratedInstallPlan)
resolveTargetForms verbosity selectPackageTargets selectComponentTarget flags globalFlags targetStrings = do
  ProjectBaseContext
    { distDirLayout
    , cabalDirLayout
    , projectConfig
    , localPackages
    } <-
    establishProjectBaseContext verbosity cliConfig OtherCommand

  (_, elaboratedPlan, _, _, _) <-
    rebuildInstallPlan
      verbosity
      distDirLayout
      cabalDirLayout
      projectConfig
      localPackages
      Nothing

  targetSelectors <-
    either (reportTargetSelectorProblems verbosity) return
      =<< readTargetSelectors localPackages Nothing targetStrings

  targets :: TargetsMap <-
    either (reportBuildTargetProblems verbosity) return $
      resolveTargetsFromSolver
        selectPackageTargets
        selectComponentTarget
        elaboratedPlan
        Nothing
        targetSelectors

  return (targets, elaboratedPlan)
  where
    cliConfig =
      commandLineFlagsToProjectConfig
        globalFlags
        flags
        mempty

reportBuildTargetProblems :: Verbosity -> [TargetProblem'] -> IO a
reportBuildTargetProblems verbosity = reportTargetProblems verbosity "target"

printTargetForms :: Verbosity -> [String] -> TargetsMap -> ElaboratedInstallPlan -> IO ()
printTargetForms verbosity targetStrings targets elaboratedPlan =
  noticeDoc verbosity $
    vcat
      [ text "Fully qualified target forms" Pretty.<> colon
      , nest 1 $ vcat [text "-" <+> text tf | tf <- targetForms]
      , found
      ]
  where
    found =
      let n = length targets
          t = if n == 1 then "target" else "targets"
          query = intercalate ", " targetStrings
       in text "Found" <+> int n <+> text t <+> text "matching" <+> text query Pretty.<> char '.'

    localPkgs =
      [x | Configured x@ElaboratedConfiguredPackage{elabLocalToProject = True} <- InstallPlan.toList elaboratedPlan]

    targetForm ct x =
      let pkgId@PackageIdentifier{pkgName = n} = elabPkgSourceId x
       in render $ pretty n Pretty.<> colon Pretty.<> text (showComponentTarget pkgId ct)

    targetForms =
      sort $
        catMaybes
          [ targetForm ct <$> pkg
          | (u :: UnitId, xs) <- Map.toAscList targets
          , let pkg = safeHead $ filter ((== u) . elabUnitId) localPkgs
          , (ct :: ComponentTarget, _) <- xs
          ]

-------------------------------------------------------------------------------
-- Probing individual strings
-------------------------------------------------------------------------------

-- | What a single string turned out to be.
data TargetMatch
  = -- | It names something in the project.
    MatchSelector TargetSelector
  | -- | It does not name anything in the project but is an existing file, so
    -- it may be a script. This is the same tie-break
    -- 'Distribution.Client.ScriptUtils.withContextAndSelectors' applies.
    MatchScript FilePath
  deriving (Eq, Ord, Show)

-- | Probes one string at a time. Build it once per command with
-- 'newTargetOracle' and reuse it: the 'KnownTargets' it closes over is the
-- expensive part.
newtype TargetOracle m = TargetOracle
  { probeTarget :: String -> m (Maybe TargetMatch)
  }

-- | Build an oracle over the project's local packages.
--
-- This is 'Distribution.Client.TargetSelector.readTargetSelectorsWith' taken
-- apart so that 'getKnownTargets' — which walks every local package and
-- flattens its description — happens once rather than once per string, and so
-- that a failure to resolve is reported per string instead of failing the
-- whole batch.
newTargetOracle
  :: Monad m
  => DirActions m
  -> [PackageSpecifier UnresolvedSourcePackage]
  -> Maybe ComponentKindFilter
  -- ^ Used only to disambiguate an otherwise ambiguous string.
  -> m (TargetOracle m)
newTargetOracle dirActions pkgs mfilter = do
  knowntargets <- getKnownTargets dirActions pkgs
  return . TargetOracle $ \s ->
    case parseTargetString s of
      Nothing -> asScript s
      Just t -> do
        t' <- getTargetStringFileStatus dirActions t
        case resolveTargetSelector knowntargets mfilter t' of
          Right selector | isLocalSelector selector -> return (Just (MatchSelector selector))
          _ -> asScript s
  where
    asScript s = do
      exists <- doesFileExist dirActions s
      return $ if exists then Just (MatchScript s) else Nothing

-- | Did this selector actually match something in the project?
--
-- Resolution succeeds for any bare word, handing back a 'TargetPackageNamed'
-- that stands for \"a package by this name, from wherever\" and is only
-- checked later against the plan. That is the right answer for a command that
-- can reach beyond the project, but it is not what \"is this string a target?\"
-- means here: taking it as a yes would make every unrecognised word a target.
-- 'TargetComponentUnknown' is unverified in the same way.
isLocalSelector :: TargetSelector -> Bool
isLocalSelector selector = case selector of
  TargetPackage{} -> True
  TargetAllPackages{} -> True
  TargetComponent{} -> True
  TargetPackageNamed{} -> False
  TargetComponentUnknown{} -> False

-- | An oracle recognising a fixed set of names, for doctests and tests.
knownTargetOracle :: Applicative m => [String] -> TargetOracle m
knownTargetOracle known = TargetOracle $ \s ->
  pure $
    if s `elem` known
      then Just (MatchSelector (TargetPackageNamed (mkPackageName s) Nothing))
      else Nothing

-------------------------------------------------------------------------------
-- Diagnostics
-------------------------------------------------------------------------------

-- | The executable-like components (executables, test suites and benchmarks)
-- that the given selectors reach, using only the local packages so that no
-- install plan is needed.
--
-- This is for enriching messages — naming the candidates when no target was
-- given, say. It deliberately plays no part in deciding whether a string is a
-- target: a package with two buildable executables is still a target, and
-- demoting it to an argument would replace a good \"matches multiple\" error
-- with a baffling one.
runnableComponents
  :: [PackageSpecifier UnresolvedSourcePackage]
  -> [TargetSelector]
  -> [ComponentName]
runnableComponents pkgs selectors =
  case resolveTargetsFromLocalPackages selectRunnable selectComponentTargetBasic pkgs selectors of
    Left _ -> []
    Right targets ->
      sortNub
        [ cname
        | (_, cts) <- Map.toList targets
        , (ComponentTarget cname _, _) <- cts
        ]

selectRunnable
  :: forall k
   . TargetSelector
  -> [AvailableTarget k]
  -> Either TargetProblem' [k]
selectRunnable _ targets =
  Right . selectBuildableTargets $
    concatMap (`filterTargetsKind` targets) [ExeKind, TestKind, BenchKind]
