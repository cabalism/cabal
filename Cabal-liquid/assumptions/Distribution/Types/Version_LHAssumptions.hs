{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | LiquidHaskell assumptions about "Distribution.Types.Version".
--
-- A 'Version' is modelled as an integer in the logic.  Only its ordering and
-- equality are ever used, and 'Ord' on 'Version' is a total order, so any
-- finite set of versions can be numbered in order.  'version0' is given the
-- number 0, which makes a valid version one that is at least 0.
module Distribution.Types.Version_LHAssumptions where

import Distribution.Types.Version

{-@

embed Version as int

type ValidVersion = {v:Version | v >= 0}

assume version0 :: {v:Version | v == 0}

// A valid version has no negative components, so it is at least version0.
assume validVersion :: v:Version -> {b:Bool | b => v >= 0}

@-}
