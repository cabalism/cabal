---
synopsis: Freeze only the flags that all instances of a package agree on
packages: [cabal-install]
issues: 5134
prs: 0000
---

When a plan had more than one instance of a package, for example one as a
library dependency and another version as a setup dependency, `cabal freeze`
wrote the flags of one of them as a constraint on the package. That constraint
applies to the top-level instance, so when the instances needed different
values for a flag the freeze file could make the project unsolvable.

`cabal freeze` now leaves out a flag when the instances of a package in the
plan give it different values, and still writes the flags they agree on.
