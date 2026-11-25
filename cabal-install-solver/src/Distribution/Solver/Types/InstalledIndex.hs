-- | The installed packages, as the solver needs them.
module Distribution.Solver.Types.InstalledIndex
    ( InstalledIndex
    , mkInstalledIndex
    , installedGroups
    , lookupUnitId
    ) where

import Distribution.Solver.Compat.Prelude
import Prelude ()

import qualified Data.Map.Strict as Map

import Distribution.InstalledPackageInfo ( InstalledPackageInfo, installedUnitId )
import Distribution.Types.UnitId ( UnitId )

-- | The installed packages the solver can choose from.
data InstalledIndex = InstalledIndex
    { installedGroups :: [[InstalledPackageInfo]]
      -- ^ The installed packages in groups, one group for each library of
      -- each version of a package. The order within a group matters. When
      -- shadowing is on, the solver shadows every package of a group but the
      -- first.
    , installedByUnitId :: Map UnitId InstalledPackageInfo
    }

-- | Make an index from groups of installed packages, one group for each
-- library of each version of a package. The unit ids must be unique.
mkInstalledIndex :: [[InstalledPackageInfo]] -> InstalledIndex
mkInstalledIndex groups = InstalledIndex
    { installedGroups = groups
    , installedByUnitId =
        Map.fromList [ (installedUnitId ipi, ipi) | ipi <- concat groups ]
    }

-- | Look up an installed package by its unit id.
lookupUnitId :: InstalledIndex -> UnitId -> Maybe InstalledPackageInfo
lookupUnitId index uid = Map.lookup uid (installedByUnitId index)
