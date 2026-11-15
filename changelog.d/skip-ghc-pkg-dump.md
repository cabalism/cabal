---
synopsis: "Read the installed package index only when a package will be configured"
packages: [cabal-install]
prs:
issues: 12592
---

Executing a build plan started with a bulk `ghc-pkg dump` of every package db
the plan touches, including the store, before any package was built. Most
runs never used the result: a `cabal repl` of an already configured component,
or a `cabal build` after editing a source file, configure nothing. The index is
now read only when some package in the plan is going to be configured, which
is known before execution starts, so those runs skip the dumps.
