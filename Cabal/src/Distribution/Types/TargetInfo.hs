{-# LANGUAGE TypeFamilies #-}

module Distribution.Types.TargetInfo
  ( TargetInfo (..)
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.ModuleName (ModuleName)
import Distribution.Types.Component
import Distribution.Types.ComponentLocalBuildInfo
import Distribution.Types.UnitId

import Distribution.Compat.Graph (IsNode (..))

-- | The 'TargetInfo' contains all the information necessary to build a
-- specific target (e.g., component/module/file) in a package.  In
-- principle, one can get the 'Component' from a
-- 'ComponentLocalBuildInfo' and 'LocalBuildInfo', but it is much more
-- convenient to have the component in hand.
data TargetInfo = TargetInfo
  { targetCLBI :: ComponentLocalBuildInfo
  , targetComponent :: Component
  , targetSub :: Maybe (Either ModuleName FilePath)
  -- ^ The module or file of the component that was targeted, if any. Only
  -- haddock acts on it so far; other commands process the whole component.
  --
  -- @since 3.20.0.0
  }
  deriving (Generic, Show)

instance Binary TargetInfo
instance Structured TargetInfo

instance IsNode TargetInfo where
  type Key TargetInfo = UnitId
  nodeKey = nodeKey . targetCLBI
  nodeNeighbors = nodeNeighbors . targetCLBI
