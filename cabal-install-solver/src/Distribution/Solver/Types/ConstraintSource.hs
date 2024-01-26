{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Distribution.Solver.Types.ConstraintSource
    ( ConstraintSource(..)
    , RootConfig(..)
    , Importee(..)
    , Importer(..)
    , ProjectConfigPath(..)
    , mkProjectConfigPath
    , projectConfigPathSource
    , showProjectConfigPath
    , showConstraintSource
    , nullProjectConfigPath
    ) where

import Distribution.Solver.Compat.Prelude hiding (toList)
import Prelude ()
import Data.Coerce (coerce)
import Data.List.NonEmpty (toList)
import GHC.Stack (HasCallStack)

-- | Path to the project config file root, typically cabal.project.
newtype RootConfig = RootConfig FilePath
    deriving (Eq, Show, Generic)

-- | Path to the project config file with the import.
newtype Importer = Importer FilePath
    deriving (Eq, Show, Generic)

-- | Path to the imported file contributing to the project config.
newtype Importee = Importee FilePath
    deriving (Eq, Show, Generic)

-- | Path to the project config file, either the root or an import.
newtype ProjectConfigPath = ProjectImport (NonEmpty FilePath)
    deriving (Eq, Show, Generic)

-- | Renders the path as a tree node with its ancestors.
showProjectConfigPath :: ProjectConfigPath -> String
showProjectConfigPath (ProjectImport xs) =
    unlines
        [ (nTimes i (showChar ' ') . showString "+-- " . showString x) ""
        | x <- reverse $ toList xs
        | i <- [0..]
        ]

-- | Apply a function @n@ times to a given value.
-- SEE: GHC.Utils.Misc
nTimes :: Int -> (a -> a) -> (a -> a)
nTimes 0 _ = id
nTimes 1 f = f
nTimes n f = f . nTimes (n-1) f

mkProjectConfigPath :: HasCallStack => [Importer] -> Importee -> ProjectConfigPath
mkProjectConfigPath (coerce -> importers) (Importee importee) =
    ProjectImport $ importee :| importers

projectConfigPathSource :: ProjectConfigPath -> FilePath
projectConfigPathSource (ProjectImport xs) = last xs

nullProjectConfigPath :: ProjectConfigPath
nullProjectConfigPath = ProjectImport $ "unused" :| []

instance Binary RootConfig
instance Structured RootConfig
instance Binary Importee
instance Structured Importee
instance Binary Importer
instance Structured Importer
instance Binary ProjectConfigPath
instance Structured ProjectConfigPath

-- | Source of a 'PackageConstraint'.
data ConstraintSource =

  -- | Main config file, which is ~/.cabal/config by default.
  ConstraintSourceMainConfig FilePath

  -- | Local cabal.project file
  | ConstraintSourceProjectConfig ProjectConfigPath

  -- | User config file, which is ./cabal.config by default.
  | ConstraintSourceUserConfig FilePath

  -- | Flag specified on the command line.
  | ConstraintSourceCommandlineFlag

  -- | Target specified by the user, e.g., @cabal install package-0.1.0.0@
  -- implies @package==0.1.0.0@.
  | ConstraintSourceUserTarget

  -- | Internal requirement to use installed versions of packages like ghc-prim.
  | ConstraintSourceNonReinstallablePackage

  -- | Internal constraint used by @cabal freeze@.
  | ConstraintSourceFreeze

  -- | Constraint specified by a config file, a command line flag, or a user
  -- target, when a more specific source is not known.
  | ConstraintSourceConfigFlagOrTarget

  -- | Constraint introduced by --enable-multi-repl, which requires features
  -- from Cabal >= 3.11
  | ConstraintSourceMultiRepl

  -- | The source of the constraint is not specified.
  | ConstraintSourceUnknown

  -- | An internal constraint due to compatibility issues with the Setup.hs
  -- command line interface requires a minimum lower bound on Cabal
  | ConstraintSetupCabalMinVersion

  -- | An internal constraint due to compatibility issues with the Setup.hs
  -- command line interface requires a maximum upper bound on Cabal
  | ConstraintSetupCabalMaxVersion
  deriving (Eq, Show, Generic)

instance Binary ConstraintSource
instance Structured ConstraintSource

-- | Description of a 'ConstraintSource'.
showConstraintSource :: ConstraintSource -> String
showConstraintSource (ConstraintSourceMainConfig path) =
    "main config " ++ path
showConstraintSource (ConstraintSourceProjectConfig projectConfig) =
    "project config " ++ showProjectConfigPath projectConfig
showConstraintSource (ConstraintSourceUserConfig path)= "user config " ++ path
showConstraintSource ConstraintSourceCommandlineFlag = "command line flag"
showConstraintSource ConstraintSourceUserTarget = "user target"
showConstraintSource ConstraintSourceNonReinstallablePackage =
    "non-reinstallable package"
showConstraintSource ConstraintSourceFreeze = "cabal freeze"
showConstraintSource ConstraintSourceConfigFlagOrTarget =
    "config file, command line flag, or user target"
showConstraintSource ConstraintSourceMultiRepl =
    "--enable-multi-repl"
showConstraintSource ConstraintSourceUnknown = "unknown source"
showConstraintSource ConstraintSetupCabalMinVersion =
    "minimum version of Cabal used by Setup.hs"
showConstraintSource ConstraintSetupCabalMaxVersion =
    "maximum version of Cabal used by Setup.hs"
