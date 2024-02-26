{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Distribution.Client.CmdTarget
  ( targetCommand
  , targetAction
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.CmdErrorMessages
import Distribution.Client.ProjectFlags
  ( removeIgnoreProjectOption
  )
import Distribution.Client.ProjectOrchestration
import Distribution.Client.TargetProblem
  ( TargetProblem'
  )
import qualified Data.Map as Map
import Distribution.Client.Errors
import Distribution.Client.NixStyleOptions
  ( NixStyleFlags (..)
  , defaultNixStyleFlags
  , nixStyleOptions
  )
import Distribution.Client.ScriptUtils
  ( AcceptNoTargets (..)
  , TargetContext (..)
  , updateContextAndWriteProjectFile
  , withContextAndSelectors
  )
import Distribution.Client.Setup
  ( ConfigFlags (..)
  , GlobalFlags
  , yesNoOpt
  )
import Distribution.Simple.Command
  ( CommandUI (..)
  , option
  , usageAlternatives
  )
import Distribution.Simple.Flag (Flag (..), fromFlagOrDefault, toFlag)
import Distribution.Simple.Utils
  ( dieWithException
  , wrapText
  )
import Distribution.Verbosity
  ( normal
  )
import Distribution.Client.CmdBuild (selectPackageTargets, selectComponentTarget)

-------------------------------------------------------------------------------
-- Command
-------------------------------------------------------------------------------

targetCommand :: CommandUI (NixStyleFlags TargetFlags)
targetCommand =
  CommandUI
    { commandName = "v2-target"
    , commandSynopsis = "List target forms."
    , commandUsage = usageAlternatives "v2-target" ["[TARGETS] [FLAGS]"]
    , commandDescription = Just $ \_ ->
        wrapText
          "List all target forms, abbreviated and explicit."
    , commandNotes = Nothing
    , commandDefaultFlags = defaultNixStyleFlags defaultTargetFlags
    , commandOptions =
        removeIgnoreProjectOption
          . nixStyleOptions
            ( \showOrParseArgs ->
                [ option
                    []
                    ["explicit-only"]
                    "No abbreviations, only explicit forms."
                    targetOnlyExplicit
                    (\e flags -> flags{targetOnlyExplicit = e})
                    (yesNoOpt showOrParseArgs)
                ]
            )
    }

-------------------------------------------------------------------------------
-- Flags
-------------------------------------------------------------------------------

data TargetFlags = TargetFlags
  { targetOnlyExplicit :: Flag Bool
  }

defaultTargetFlags :: TargetFlags
defaultTargetFlags =
  TargetFlags
    { targetOnlyExplicit = toFlag False
    }

-------------------------------------------------------------------------------
-- Action
-------------------------------------------------------------------------------

targetAction :: NixStyleFlags TargetFlags -> [String] -> GlobalFlags -> IO ()
targetAction flags@NixStyleFlags{..} targetStrings globalFlags = do
  withContextAndSelectors RejectNoTargets Nothing flags targetStrings globalFlags BuildCommand $ \targetCtx ctx targetSelectors -> do
    let targetAction'  = TargetActionConfigure

    baseCtx <- case targetCtx of
      ProjectContext -> return ctx
      GlobalContext -> return ctx
      ScriptContext path exemeta -> updateContextAndWriteProjectFile ctx path exemeta

    buildCtx <-
      runProjectPreBuildPhase verbosity baseCtx $ \elaboratedPlan -> do
        -- Interpret the targets on the command line as build targets
        -- (as opposed to say repl or haddock targets).
        targets <-
          either (reportBuildTargetProblems verbosity) return $
            resolveTargets
              selectPackageTargets
              selectComponentTarget
              elaboratedPlan
              Nothing
              targetSelectors

        let elaboratedPlan' =
              pruneInstallPlanToTargets
                targetAction'
                targets
                elaboratedPlan
        elaboratedPlan'' <-
          if buildSettingOnlyDeps (buildSettings baseCtx)
            then
              either (reportCannotPruneDependencies verbosity) return $
                pruneInstallPlanToDependencies
                  (Map.keysSet targets)
                  elaboratedPlan'
            else return elaboratedPlan'

        return (elaboratedPlan'', targets)

    --trace ("XXXX-Targets: " ++ show targetsMap) $
    printPlanTargetForms verbosity baseCtx buildCtx
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)

reportBuildTargetProblems :: Verbosity -> [TargetProblem'] -> IO a
reportBuildTargetProblems verbosity problems =
  reportTargetProblems verbosity "target" problems

reportCannotPruneDependencies :: Verbosity -> CannotPruneDependencies -> IO a
reportCannotPruneDependencies verbosity =
  dieWithException verbosity . ReportCannotPruneDependencies . renderCannotPruneDependencies
