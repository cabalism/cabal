module Distribution.Solver.Types.LabeledPackageConstraint
    ( LabeledPackageConstraint(..)
    , unlabelPackageConstraint
    ) where

import Distribution.Solver.Types.ConstraintSource
import Distribution.Solver.Types.PackageConstraint
import Distribution.Types.PackageName

-- | 'PackageConstraint' labeled with its source.
data LabeledPackageConstraint
   = LabeledPackageConstraint PackageConstraint ConstraintSource

instance Show LabeledPackageConstraint where
    show (LabeledPackageConstraint pc@(PackageConstraint scope _) src@ConstraintSourceProjectConfig{}) =
        if unPackageName (scopeToPackageName scope) == "hashable"
            then "SOURCE: " ++ showConstraintSource src ++ ", CONSTRAINT: " ++ show pc ++ "\n"
            else "\n"
    show LabeledPackageConstraint{} = "\n"

unlabelPackageConstraint :: LabeledPackageConstraint -> PackageConstraint
unlabelPackageConstraint (LabeledPackageConstraint pc _) = pc
