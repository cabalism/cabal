---
synopsis: Fix `NonEmptySet.delete` returning the set unchanged
packages: [Cabal-syntax]
prs: 0
---

`Distribution.Compat.NonEmptySet.delete` returned the original set instead of
the set without the element, after checking that the latter was not empty.
Its one use is in the rewrite of dependencies on internal libraries for
packages with `cabal-version` below 3.4, where a dependency such as
`foo:{foo, bar}` on an internal library `foo` kept its main library
component instead of dropping it.  Found by a LiquidHaskell specification.
