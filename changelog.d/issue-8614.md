---
synopsis: Install only the executables selected by a `cabal install` target
packages: [cabal-install]
prs: 
issues: [8614, 9817]
---

`cabal install example1`, for a project package with executables `example1`
and `example2`, installed both. The component chosen by the target was dropped
when the package was rebuilt for installation. Only the selected executables
are installed now.

Outside of a project, a repository executable can be named as
`pkg:exe:exename` as well as `pkg:exename`. A target that is neither of these,
nor a package name, nor an `http`, `https` or `file` URI of a source tarball,
such as `pkg:exe:*`, is reported as an unknown target. It was previously taken
for a URI and handed to the download tool, which failed with a message about
the port number.
