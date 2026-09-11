# Building cabal with buck2

This directory, together with the `buck2/` git submodule
([cabalism/haskell-buck2](https://github.com/cabalism/haskell-buck2), a fork
of [simonmar/haskell-buck2](https://github.com/simonmar/haskell-buck2)), lets
you use [Buck2](https://buck2.build/) for the edit-compile-test loop on the
packages in this repository. Cabal is still needed: it solves and builds the
Hackage dependencies, and buck2 only builds the code in this repository
against them.

## Setup

1. Install `buck2` (for example `nix shell nixpkgs#buck2`, or a release from
   <https://github.com/facebook/buck2/releases>) and check out the submodule:

   ```
   git submodule update --init buck2
   sh project-buck2/apply-patches.sh
   ```

   The second command applies `project-buck2/patches/haskell-buck2.patch`,
   three small fixes to the submodule that have not been upstreamed yet:
   the dependency generator crashed on, and did not follow the dependencies
   of, packages that cabal builds in-place; the prelude exposed packages to
   GHC by name (`-package time`), which picked GHC's global `time-1.15`
   over the `time-1.14` in the cabal store that everything else was built
   against, and now uses `-package-id`; and the compile action keeps its
   previous outputs (`no_outputs_cleanup`, a TODO in the prelude for
   hash-based GHC >= 9.4) so that `ghc --make` only recompiles changed
   modules instead of the whole package.

2. Build the dependencies with cabal and generate the buck2 view of them:

   ```
   cabal build cabal-install --only-dependencies --enable-tests
   python3 buck2/gen-haskell-prebuilt.py
   python3 project-buck2/fetch-inplace-deps.py
   ```

   The target is `cabal-install` rather than `all`: with `all`, cabal
   refuses `--only-dependencies` ("the package Cabal-syntax-3.19.0.0 is
   required by a dependency of one of the other targets") because
   hackage-security, a dependency, needs Cabal-syntax, a target. With
   `cabal-install` as the only target, the other local packages are just
   dependencies and get built in-place by cabal once, along with
   hackage-security and every Hackage package the buck2 targets need.

   The first script writes `third-party/haskell/` (a `haskell_prebuilt_library`
   per store/global package, the GHC toolchain version, and the locations of
   `alex` and `happy`). The second copies the source of Hackage packages that
   cabal builds in-place because they depend on a local package (currently
   `hackage-security`, which depends on `Cabal-syntax`) into
   `project-buck2/vendor/<pkg>/src`, where a hand-written `BUCK` file builds
   them. Both directories are git-ignored; re-run the scripts after changing
   the dependencies or the GHC version (`cabal build ... -w ghc-X.Y.Z`).

3. Build and run:

   ```
   buck2 build //...
   buck2 run //cabal-install/main:cabal -- --version
   buck2 test //cabal-install-solver/tests:unit-tests
   buck2 build //... -m opt        # -O and static linking; also -m prof
   ```

## Layout

| File | Purpose |
| --- | --- |
| `.buckconfig`, `PACKAGE` | Cells (root, `prelude`, `toolchains`, `third-party`) and the build-mode modifiers. |
| `project-buck2/cabal.bzl` | `cabal_library`, `cabal_binary`, `cabal_test` and `cabal_paths_module`, thin wrappers over `buck2/haskell.bzl`. |
| `project-buck2/cfg.bzl` | The `dev`/`opt`/`prof`/`asan` aliases for `-m`. |
| `project-buck2/patches/`, `apply-patches.sh` | Local fixes to the submodule, see Setup. |
| `project-buck2/fetch-inplace-deps.py`, `vendor/` | Hackage packages that cabal builds in-place, built from source. |
| `*/src/BUCK`, `cabal-install/main/BUCK`, `*/tests/BUCK` | One target per Cabal component, in its `hs-source-dirs`. |
| `Cabal/BUCK`, `cabal-install/BUCK` | The generated `Paths_Cabal` and `Paths_cabal_install` modules. |

`BUCK` files list `packages` (Hackage packages, by name) and `deps` (other
targets). They duplicate the `build-depends` of the corresponding `.cabal`
stanza, so update both when adding a dependency; module lists come from
`glob()`, so new modules need no change.

## How the Cabal-specific parts are handled

* **CPP macros.** GHC generates `MIN_VERSION_<pkg>` for every package named
  on its command line, so `#if MIN_VERSION_base(4,18,0)` works as under
  Cabal. Two differences: libraries built by buck2 are registered as version
  1.0.0 by the prelude, so `MIN_VERSION_Cabal` and friends are not accurate
  (nothing in the tree uses them); and `CURRENT_PACKAGE_KEY`, which only
  Cabal's `cabal_macros.h` defines, is passed explicitly where needed
  (`Cabal/src/BUCK`, so that `cabalVersion` reads `Paths_Cabal`).
* **Package scope.** Every target is compiled with `-hide-all-packages`, so a
  missing entry in `packages` is a compile error rather than an accidental
  dependency on something in GHC's global package db.
* **`Paths_*` modules.** `cabal_paths_module` generates a module exporting
  only `version`, read from the `.cabal` file, which is all the code uses.
* **Flags.** The cabal.project flags that need extra packages are off:
  `git-rev` (so `cabal --version` reports no git hash) and
  `legacy-comparison`. `native-dns` is on.
* **alex.** `Distribution/Fields/Lexer.x` in Cabal-syntax is run through the
  `alex` that cabal built (see `third-party/haskell/tools.bzl`).

## Incremental builds

Each package is one `ghc --make` action, so buck2's own incrementality is
per package. Three things make edits cheap anyway: the compile action keeps
its `-odir`/`-hidir` between runs so GHC's recompilation checker skips
unchanged modules (see the patch above), GHC gets `-j`, and dev mode passes
`-O0` (the submodule's toolchain would otherwise use `-O` in every mode).
Measured on a 24-core machine after a whitespace-only edit in
`Cabal/src/Distribution/Simple/GHCJS.hs`: `buck2 build //...` went from
87s to 3s, against 23s for `cabal build all --enable-tests`.

## Covered components

Libraries `Cabal-syntax`, `Cabal`, `cabal-install-solver`, `hooks-exe`,
`cabal-install` and the vendored `hackage-security`; the `cabal` executable;
the `cabal-install-solver` unit tests. The other test suites, benchmarks and
packages (`Cabal-tests`, `cabal-testsuite`, ...) have no `BUCK` files yet and
are built with cabal as before.
