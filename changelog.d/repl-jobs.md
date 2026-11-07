---
synopsis: "`cabal repl` loads modules in parallel"
packages: [Cabal, cabal-install]
prs:
---

`cabal repl` has long accepted `-j`, but for a single component the value only
governed how many dependencies cabal built at once. It never reached GHCi, which
loaded the component's modules one at a time. It is now passed on as GHC's `-j`,
as the multi-repl already did, so GHCi compiles modules in parallel while
loading them.
