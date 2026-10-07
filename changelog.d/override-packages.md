---
synopsis: Package sources at a stronger position replace weaker ones
packages: [cabal-install]
issues: [8463]
---

When several project files list a source for the same package, with `packages`
or `source-repository-package`, cabal now keeps the one listed at the strongest
position and drops the others, reporting what it used and what it ignored. So a
`source-repository-package` or a `packages` directory in `cabal.project.local`
replaces a `source-repository-package` for the same package in `cabal.project`,
and a source in a project file replaces one in a file it imports. Positions are
those of `override-constraints`: `cabal.project.local` and its imports, then
`cabal.project`, its freeze file and their imports, with a file nearer the root
of the imports outranking the files it imports.

Two different sources for one package at the same position are now an error
rather than an undefined choice. The same source listed twice still counts once.
