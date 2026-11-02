---
synopsis: Read the package databases only when a package needs configuring
packages: [cabal-install]
prs: 0000
---

Since cabal-install 3.18, every `build`, `repl` and the like started by
reading all the package databases the plan touches, running `ghc-pkg dump`
once per database and parsing every entry, to seed the per-project
`InstalledPackageIndex` that configuring a package consults. On a large store
this costs seconds even when nothing has changed: with 3400 entries in the
store, an up-to-date `cabal repl --with-repl --enable-multi-repl` (the command
hie-bios runs for every HLS and `hdb` session) spent 3.2 s of its 3.9 s there.

The databases are now read the first time a package is configured. A run that
configures nothing no longer reads them.
