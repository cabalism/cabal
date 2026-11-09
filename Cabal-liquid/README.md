# Cabal-liquid

This package is not released.  It exists so that refinement type
specifications written in modules of the Cabal libraries can be checked with
[LiquidHaskell](https://ucsd-progsys.github.io/liquidhaskell/).

The specifications live in the modules they describe, as `{-@ ... @-}`
comments that are inert in an ordinary build.  LiquidHaskell is a GHC plugin
and supports one GHC version at a time, so it cannot be a dependency of the
libraries, which are GHC boot packages.  Instead this package compiles the
modules under verification a second time, from their own source files, with
the plugin enabled.

## Running the check

```
make liquid
```

which is

```
cabal build --project-file=cabal.liquid.project --builddir=dist-newstyle-liquid all
```

This needs the GHC that the pinned liquidhaskell targets, currently GHC 9.14,
and the `z3` SMT solver on the `PATH`.  The build products are kept apart
from the day-to-day `dist-newstyle` because the plugin changes how modules are
compiled.

A module passes when the plugin prints `**** LIQUID: SAFE ****` for it.
Failures are reported as GHC errors pointing at the module's source.

## Adding a module

Each module under verification is a symbolic link, in a sub-library of its
own, to the module of
the same name in the library that owns it, and is listed in `other-modules`
of `Cabal-liquid.cabal`.  Its imports still come from the compiled library.

```
ln -s ../../../../Cabal-syntax/src/Distribution/Types/Foo.hs Cabal-liquid/foo/Distribution/Types/Foo.hs
```

Do not put a whole library source directory on `hs-source-dirs`: GHC would
then compile every module reachable by import with the plugin.

## Assumptions

Specifications for library modules that are not themselves verified go in
companion modules named `<Module>_LHAssumptions`, which LiquidHaskell finds
by name.  These are trusted, not checked.  Keep them in the `assumptions`
sub-library so they are compiled before the modules that need them.

## Modules under verification

- `Distribution.Types.VersionInterval`: the intervals built from a version
  range are non-empty and pairwise separated.
- `Distribution.Types.Version`: the packed representation is well formed and
  only used for components that fit it, and `mkVersion` and `versionNumbers`
  are inverses.  What the bit operations do to the four 16-bit fields is
  assumed.  Not built yet, see the `version` sub-library.

- `Distribution.Compat.NonEmptySet`: the set is never empty, so `panic` is
  unreachable, and each operation is described by the set it returns.  The
  specification of `delete` found that it discarded its own result.  Not
  built yet, see the `nonemptyset` sub-library.

## Known limitations of the plugin

The helper package is built without optimisation because LiquidHaskell cannot
handle `UNPACK` fields.  It pins a fork of liquidhaskell because the Hackage
release depends on the Cabal library.  The `version` and `nonemptyset`
sub-libraries are not buildable; the comments on them in the package
description say what each waits for upstream.

Newtypes are a moving target.  With the pinned release a newtype is its
representation in the logic, with upstream develop it is a type of its own
whose field measure is not yet related to the value it was built from, and
in neither is the constructor refinement checked where a value is built.
A type `invariant` is what makes functions see the property, and it is an
assumption until construction is checked.

Check that a module's verification is not vacuous: a `WARNING: Found false`
line from the plugin means some binder was inferred to be unreachable, and if
that binder is top level every specification in the module passes trivially.
Breaking the code on purpose and expecting UNSAFE is the quickest test.
