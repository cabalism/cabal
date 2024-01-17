{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ParallelListComp #-}
module Distribution.Solver.Types.ConstraintSource
    ( ConstraintSource(..)
    , RootConfig(..)
    , Importee(..)
    , Importer(..)
    , ImportedConfig(..)
    , ProjectConfigPath(..)
    , mkProjectConfigPath
    , projectConfigPathSource
    , nullProjectConfigPath
    ) where

import Distribution.Solver.Compat.Prelude
import Prelude ()
import Data.Coerce (coerce)
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

-- | The imported config along with its import chain.
data ImportedConfig =
    ImportedConfig
        { importers :: [Importer]
        -- ^ Path to the project config file with the import. Doesn't include the importee.
        , importee :: Importee
        -- ^ Path to the imported file contributing to the project config.
        }
    deriving (Eq, Show, Generic)

-- | Path to the project config file, either the root or an import.
data ProjectConfigPath = ProjectRoot RootConfig | ProjectImport ImportedConfig
    deriving (Eq, Show, Generic)

mkProjectConfigPath :: HasCallStack => [Importer] -> Importee -> ProjectConfigPath
mkProjectConfigPath [] (Importee path) = ProjectRoot $ RootConfig path
mkProjectConfigPath importers@[_] importee = ProjectImport $ ImportedConfig
    { importers
    , importee
    }
mkProjectConfigPath (i:is) importee = case mkProjectConfigPath is importee of
    ProjectImport importedConfig -> ProjectImport $ importedConfig
        { importers = i : importers importedConfig }
    ProjectRoot _ -> error $ "mkProjectConfigPath: depth == 0 but expected import depth > 1"

projectConfigPathSource :: ProjectConfigPath -> FilePath
projectConfigPathSource = \case
    ProjectRoot path -> coerce path
    ProjectImport importedConfig -> coerce $ importee importedConfig

nullProjectConfigPath :: ProjectConfigPath
nullProjectConfigPath = ProjectRoot $ RootConfig "unused"

instance Binary RootConfig
instance Structured RootConfig
instance Binary Importee
instance Structured Importee
instance Binary Importer
instance Structured Importer
instance Binary ImportedConfig
instance Structured ImportedConfig
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