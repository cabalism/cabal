{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Distribution.Client.CmdTarget
  ( targetCommand
  , targetAction
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.CmdErrorMessages
  ( Plural (..)
  , renderComponentKind
  )
import Distribution.Client.DistDirLayout
  ( DistDirLayout (..)
  , ProjectRoot (..)
  )
import Distribution.Client.NixStyleOptions
  ( NixStyleFlags (..)
  , defaultNixStyleFlags
  )
import Distribution.Client.ProjectConfig
  ( ProjectConfig
  , commandLineFlagsToProjectConfig
  , projectConfigConfigFile
  , projectConfigShared
  , withGlobalConfig
  , withProjectOrGlobalConfig
  )
import Distribution.Client.ProjectFlags
  ( ProjectFlags (..)
  , defaultProjectFlags
  , projectFlagsOptions
  )
import Distribution.Client.ProjectOrchestration
  ( CurrentCommand (..)
  , ProjectBaseContext (..)
  , establishProjectBaseContext
  , establishProjectBaseContextWithRoot
  )
import Distribution.Client.Setup
  ( GlobalFlags (..)
  )
import Distribution.Client.TargetSelector
  ( ComponentKind
  , TargetSelector (..)
  , readTargetSelectors
  , reportTargetSelectorProblems
  )
import Distribution.Client.Types
  ( PackageLocation (..)
  , PackageSpecifier (..)
  , UnresolvedSourcePackage
  )
import Distribution.Solver.Types.SourcePackage
  ( SourcePackage (..)
  )

import Distribution.Client.Errors
import Distribution.Client.SrcDist
  ( packageDirToSdist
  )
import Distribution.Compat.Lens
  ( _1
  , _2
  )
import Distribution.Package
  ( Package (packageId)
  )
import Distribution.PackageDescription.Configuration
  ( flattenPackageDescription
  )
import Distribution.ReadE
  ( succeedReadE
  )
import Distribution.Simple.Command
  ( CommandUI (..)
  , OptionField
  , ShowOrParseArgs
  , liftOptionL
  , option
  , reqArg
  )
import Distribution.Simple.PreProcess
  ( knownSuffixHandlers
  )
import Distribution.Simple.Setup
  ( Flag (..)
  , configDistPref
  , configVerbosity
  , flagToList
  , flagToMaybe
  , fromFlagOrDefault
  , optionDistPref
  , optionVerbosity
  , toFlag
  , trueArg
  )
import Distribution.Simple.SrcDist
  ( listPackageSourcesWithDie
  )
import Distribution.Simple.Utils
  ( dieWithException
  , notice
  , withOutputMarker
  , wrapText
  )
import Distribution.Types.ComponentName
  ( ComponentName
  , showComponentName
  )
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Distribution.Types.PackageName
  ( PackageName
  , unPackageName
  )
import Distribution.Verbosity
  ( normal
  )

import qualified Data.ByteString.Lazy.Char8 as BSL
import System.Directory
  ( createDirectoryIfMissing
  , getCurrentDirectory
  , makeAbsolute
  )
import System.FilePath
  ( makeRelative
  , normalise
  , (<.>)
  , (</>)
  )

-------------------------------------------------------------------------------
-- Command
-------------------------------------------------------------------------------

targetCommand :: CommandUI (ProjectFlags, TargetFlags)
targetCommand =
  CommandUI
    { commandName = "v2-target"
    , commandSynopsis = "List target forms."
    , commandUsage = \pname ->
        "Usage: " ++ pname ++ " v2-target [FLAGS] [PACKAGES]\n"
    , commandDescription = Just $ \_ ->
        wrapText
          "List all target forms, abbreviated and explicit."
    , commandNotes = Nothing
    , commandDefaultFlags = (defaultProjectFlags, defaultTargetFlags)
    , commandOptions = \showOrParseArgs ->
        map (liftOptionL _1) (projectFlagsOptions showOrParseArgs)
          ++ map (liftOptionL _2) (targetOptions showOrParseArgs)
    }

-------------------------------------------------------------------------------
-- Flags
-------------------------------------------------------------------------------

data TargetFlags = TargetFlags
  { targetExplicit :: Flag Bool
  }

defaultTargetFlags :: TargetFlags
defaultTargetFlags =
  TargetFlags
    { targetExplicit = toFlag False
    }

targetOptions :: ShowOrParseArgs -> [OptionField TargetFlags]
targetOptions showOrParseArgs =
  [ option
      []
      ["explicit-only"]
      "No abbreviations, only explicit forms."
      targetExplicit
      (\e flags -> flags{targetExplicit = e})
      trueArg
  ]

-------------------------------------------------------------------------------
-- Action
-------------------------------------------------------------------------------

targetAction :: (ProjectFlags, TargetFlags) -> [String] -> GlobalFlags -> IO ()
targetAction (pf@ProjectFlags{..}, TargetFlags{..}) targetStrings globalFlags = do
  undefined