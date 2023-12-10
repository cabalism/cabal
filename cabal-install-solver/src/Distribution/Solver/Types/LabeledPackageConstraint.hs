{-# LANGUAGE FlexibleInstances #-}

module Distribution.Solver.Types.LabeledPackageConstraint
    ( LabeledPackageConstraint(..)
    , unlabelPackageConstraint
    , weedLabeledPackageConstraints
    ) where

import Distribution.Compat.Prelude
import Prelude ()
import Distribution.Solver.Types.ConstraintSource
import Distribution.Solver.Types.PackageConstraint
import Distribution.Types.PackageName

-- | 'PackageConstraint' labeled with its source.
data LabeledPackageConstraint
   = LabeledPackageConstraint PackageConstraint ConstraintSource

instance {-# INCOHERENT #-} Show [LabeledPackageConstraint] where
    show [] = ""
    show xs = intercalate "\n" $
        filter (/= "") (showLabeledPackageConstraint <$> xs)

instance Show LabeledPackageConstraint where
    show = showLabeledPackageConstraint

showLabeledPackageConstraint :: LabeledPackageConstraint -> String
showLabeledPackageConstraint (LabeledPackageConstraint pc@(PackageConstraint scope _) src@ConstraintSourceProjectConfig{}) =
    if unPackageName (scopeToPackageName scope) == "hashable"
        then "SOURCE: " ++ showConstraintSource src ++ ", CONSTRAINT: " ++ show pc
        else ""
showLabeledPackageConstraint _ = ""

unlabelPackageConstraint :: LabeledPackageConstraint -> PackageConstraint
unlabelPackageConstraint (LabeledPackageConstraint pc _) = pc

weedLabeledPackageConstraints :: [LabeledPackageConstraint] -> [LabeledPackageConstraint]
weedLabeledPackageConstraints =
    -- Partition into ConstraintSourceProjectConfig and rest, pick one of former.
    (\(xs, ys) -> take 1 (sortBy (comparing (\(LabeledPackageConstraint _ src) -> case src of
        ConstraintSourceProjectConfig pci -> getProjectImportDepth pci
        _ -> maxBound)) xs) ++ ys
    )
    . partition (\(LabeledPackageConstraint _ src) -> case src of
            ConstraintSourceProjectConfig{} -> True
            _ -> False)
