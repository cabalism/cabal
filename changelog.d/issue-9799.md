---
synopsis: Freeze the versions and flags of setup dependencies separately
packages: [cabal-install]
issues: 9799
prs: 0000
---

A plan can have more than one version of a package, for example one as a
library dependency and another as a setup dependency. `cabal freeze` wrote a
single constraint allowing either version in any scope, such as
`any.pkg ==1.0 || ==2.0`, so building with the freeze file could pick a
different version for each scope than the plan that was frozen.

`cabal freeze` still writes that constraint, and now adds constraints for the
top level and for setup dependencies where they have fewer versions:

```
constraints: any.pkg ==1.0 || ==2.0,
             pkg ==1.0,
             setup.pkg ==2.0
```

When the setup dependencies of different packages need different versions, the
constraint names the package, as in `foo:setup.pkg ==2.0`. Flags of a setup
dependency that is not also a top-level dependency are frozen in the same way.

Freeze files for plans with one version of each package are unchanged. The
dependencies of build tools have no scope that a constraint can name, so two
build tools that need different versions of a package are still only
constrained by the `any.` constraint.
