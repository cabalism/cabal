{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | LiquidHaskell assumptions for names in scope through
-- "Distribution.Compat.Prelude".
--
-- LiquidHaskell models 'Word64' as an integer but, unlike for the narrower
-- word types, does not state that it is non-negative.  No upper bound is
-- stated: a literal of 2^64 made the invariant unsatisfiable, which makes
-- every specification in a module that mentions a 'Word64' hold vacuously.
module Distribution.Compat.Prelude_LHAssumptions where

import Data.Word
import Distribution.Compat.Prelude

{-@

invariant {v:Word64 | 0 <= v}

@-}
