{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Resolving @[TARGETS]@ into the fully-qualified target forms that
-- @cabal target@ reports.
--
-- This needs a full 'ElaboratedInstallPlan' and so runs the solver. To ask the
-- cheaper question of whether a string names a target at all, which needs only
-- the project's local packages and so works before a plan exists, see
-- "Distribution.Client.TargetArgs".
module Distribution.Client.TargetForms
  ( resolveTargetForms
  , printTargetForms
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
import Distribution.Package
import Distribution.Simple.Utils
  ( noticeDoc
  , safeHead
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
