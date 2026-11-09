{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | LiquidHaskell assumptions about "Distribution.Types.VersionRange.Internal".
module Distribution.Types.VersionRange.Internal_LHAssumptions where

import Distribution.Types.Version
import Distribution.Types.VersionRange.Internal
import Distribution.Types.Version_LHAssumptions ()

{-@

// The versions in a version range are valid.  Nothing enforces this at
// construction, but it is the contract the parser upholds and the one the
// runtime invariant check on version intervals already relies on.
data VersionRangeF a
  = ThisVersionF { thisVersionF :: ValidVersion }
  | LaterVersionF { laterVersionF :: ValidVersion }
  | OrLaterVersionF { orLaterVersionF :: ValidVersion }
  | EarlierVersionF { earlierVersionF :: ValidVersion }
  | OrEarlierVersionF { orEarlierVersionF :: ValidVersion }
  | MajorBoundVersionF { majorBoundVersionF :: ValidVersion }
  | UnionVersionRangesF { unionLeftF :: a, unionRightF :: a }
  | IntersectVersionRangesF { intersectLeftF :: a, intersectRightF :: a }

// The major upper bound of a version is above it: the second component is
// incremented, or appended when there is only one.
assume majorUpperBound :: v:Version -> {u:Version | v < u}

@-}
