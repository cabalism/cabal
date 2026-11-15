---
synopsis: "Stop `cabal build` and `cabal repl` reconfiguring each other's packages"
packages: [cabal-install]
prs:
issues: 11623
---

Alternating `cabal build` and `cabal repl` on the same package reconfigured it
on every switch, and rebuilt its dependents: the repl marked the package as
built in memory, which the configure check took for a configuration change,
and then recorded no built components in the package's build cache. The build
style is now left out of the configure check, and a repl leaves the build cache
alone since it builds nothing on disk. A multi-repl of several local packages
still reconfigures the ones whose dependencies it loads in memory, as those are
configured differently.
