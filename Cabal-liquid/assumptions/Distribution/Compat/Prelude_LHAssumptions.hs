{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | LiquidHaskell assumptions for names in scope through
-- "Distribution.Compat.Prelude".
--
-- LiquidHaskell models 'Word' and 'Word64' as integers but, unlike for the
-- narrower word types, does not state that they are non-negative.  No upper
-- bound is stated: a literal of 2^64 made the invariant unsatisfiable, which
-- makes every specification in a module that mentions a 'Word64' hold
-- vacuously.
--
-- LiquidHaskell ships no specifications for 'NonEmpty'.  The measure 'neLen'
-- is its length, so that it can be related to the length of a list.
module Distribution.Compat.Prelude_LHAssumptions where

import Data.Maybe_LHAssumptions ()
import Data.Word
import Distribution.Compat.Prelude

{-@

invariant {v:Word | 0 <= v}
invariant {v:Word64 | 0 <= v}

measure neLen :: NonEmpty a -> Int
  neLen ((:|) x xs) = 1 + len xs

@-}
