-- | The exposed Foo module of package A.
module Foo (foo) where

import Bar (bar)

-- | One more than 'bar'.
foo :: Int
foo = bar + 1
