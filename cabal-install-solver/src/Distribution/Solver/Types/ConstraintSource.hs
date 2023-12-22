{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Distribution.Solver.Types.ConstraintSource
    ( ConstraintSource(..)
    , RootConfig(..)
    , Importee(..)
    , Importer(..)
    , ImportedConfig(..)
    , ProjectConfigPath(..)
    , mkProjectConfigPath
    , projectConfigPathSource
    , showConstraintSource
    , nullProjectConfigPath
    ) where

import Distribution.Solver.Compat.Prelude
import Prelude ()
import Data.Coerce (coerce)
import GHC.Stack (HasCallStack)

newtype RootConfig = RootConfig FilePath
    deriving (Eq, Show, Generic)

newtype Importer = Importer FilePath
    deriving (Eq, Show, Generic)

newtype Importee = Importee FilePath
    deriving (Eq, Show, Generic)

data ImportedConfig =
    ImportedConfig
        { importDepth :: Int
        -- ^ Depth of the import. The main project config file has depth 0, and each
        -- import increases the depth by 1.
        , importers :: [Importer]
        -- ^ Path to the project config file with the import.
        , importee :: Importee
        -- ^ Path to the imported file contributing to the project config.
        }
    deriving (Eq, Show, Generic)

data ProjectConfigPath = ProjectRoot RootConfig | ProjectImport ImportedConfig
    deriving (Eq, Show, Generic)

mkProjectConfigPath :: HasCallStack => Int -> [Importer] -> Importee -> ProjectConfigPath
mkProjectConfigPath 0 [] (Importee path) = ProjectRoot $ RootConfig path
mkProjectConfigPath p [] _ = error $ "mkProjectConfigPath: depth == " ++ show p ++ " but expected depth == 0"
mkProjectConfigPath 0 xs _ = error $ "mkProjectConfigPath: importers == " ++ show xs ++ " but expected []"
mkProjectConfigPath 1 importers@[_] importee = ProjectImport $ ImportedConfig
    { importDepth = 1
    , importers
    , importee
    }
mkProjectConfigPath n (i:is) importee = case mkProjectConfigPath (n - 1) is importee of
    ProjectImport importedConfig -> ProjectImport $ importedConfig
        { importDepth = n
        , importers = i : importers importedConfig
        }
    ProjectRoot _ -> error $ "mkProjectConfigPath: depth == " ++ show n ++ " but expected depth > 1"

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

-- | Description of a 'ConstraintSource'.
showConstraintSource :: ConstraintSource -> String
showConstraintSource (ConstraintSourceMainConfig path) =
    "main config " ++ path
showConstraintSource (ConstraintSourceProjectConfig projectConfig) =
    "project config " ++ show projectConfig
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
