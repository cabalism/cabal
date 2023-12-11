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
import Distribution.Types.VersionRange

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

-- | Weed out potential package version conflicts by picking one version
-- equality constraint with the lowest import depth and discarding the rest.
-- Constraints such as installed, source, flags and stanzas are untouched by
-- weeding.
weedLabeledPackageConstraints :: [LabeledPackageConstraint] -> [LabeledPackageConstraint]
weedLabeledPackageConstraints =
    (\(xs, ys) -> take 1 (sortBy (comparing (\(LabeledPackageConstraint _ src) -> case src of
        ConstraintSourceProjectConfig pci -> importDepth pci
        _ -> maxBound)) xs) ++ ys
    )
    . partition isVersionEqualityConstraint

isVersionEqualityConstraint :: LabeledPackageConstraint -> Bool
isVersionEqualityConstraint (LabeledPackageConstraint constraint source)
    | ConstraintSourceProjectConfig{} <- source
    , PackageConstraint _ (PackagePropertyVersion versionRange) <- constraint
    , ThisVersionF _ <- projectVersionRange versionRange = True
    | otherwise = False
