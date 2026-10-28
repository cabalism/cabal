---
synopsis: Add `cabal haddock-module`, an external command that documents one module quickly
packages: [cabal-install, Cabal]
issues: 8660
---

cabal-install now installs a second executable, `cabal-haddock-module`, which
cabal runs as the external command `cabal haddock-module [PACKAGE:]MODULE`, or
`cabal haddock-module FILE` with the module's source file. It renders the
Haddock page of one module of a local package's main library in a couple of
seconds, hidden modules included, where `cabal haddock MODULE` documents the
whole component.

The command builds the library first, unless `--no-build` is given, then runs
Haddock on the one source file with the built library as a dependency, so
Haddock has a single home module to process. The run uses the component's saved
configuration, so extensions, CPP, include directories and package databases are
those of the build. The page goes under `<builddir>/haddock-module/<package>/`
or the `--output` given, and its links go to the documentation cabal has
generated for the package and its dependencies, where that exists, or to
Hackage with `--hackage`, or wherever `--html-location` says. `--with-haddock`
and `--haddock-options` choose and configure haddock, as for `cabal
haddock-project`. The few options of `cabal build` it takes, `--builddir`,
`--project-file`, `--project-dir`, `--with-compiler`, `--with-hc-pkg`,
`--keep-temp-files` and the verbosity, go after the command name and are
passed on to the build. Its help text is kept with
the other commands' under `doc/`, in a new `cmd-external-help` group.

The saved configuration can only be read by the Cabal library that wrote it,
which is the one cabal links when it configures a package in process. The
command therefore works for packages with the Simple build type when it was
installed together with the cabal that built them, and says so otherwise.

The `Cabal` library now exports `HaddockArgs`, `Directory`, `Output`,
`getHaddockProg` and `runHaddock` from
`Distribution.Simple.Haddock`, the pieces `Setup haddock` is built from, for
callers like this one that run Haddock on something other than a whole
component.
