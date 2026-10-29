# Components of a non-local package built in place race to unpack its tarball

## Describe the bug

When a non-local package is built in place and has several components, each component unpacks the package's tarball into the same `dist-newstyle/src/<pkg-id>` directory at the same time. With enough components and enough jobs, the build fails in one of two ways: two extractions overlap on one file and one fails with `withFile: resource busy (file is locked)`, or a component that found the directory already there goes on to configure while another is still extracting and fails with `No cabal file found`. Running the same build again succeeds, because by then the directory exists.

It takes four things at once. The package is non-local, from a repository or a tarball. It is built in place rather than into the store, which happens when it depends on a local package. It is built per component, so `cabal-version` 1.8 or later and a build type other than Custom or Hooks, which makes each component its own unit of the install plan. And more than one job is running. That is the situation for every tool in `cabal.external-commands.project` on the `add/test-external-commands` branch, which builds third-party `cabal-<cmd>` executables against the in-tree `Cabal` and `cabal-install`.

How often it fails depends on the tarball. The double extraction itself happens most of the time, but it only errors when two extractions are writing the same file at the same instant, so a tarball of small files usually gets away with it and one with larger files does not.

The same test, whether the directory exists, is also all that decides whether an unpacked directory is reused by later builds, whatever it holds. An unpack that was interrupted, by the race above or by the user, leaves a directory that is never unpacked again, and every later build fails with missing files until it is removed by hand. A revision of the package's cabal file published after the first build never reaches the directory either: the plan is solved with the revision, the configure step runs against the file from the first unpack, and the two can disagree about dependencies. The comment above the check already says `use a proper file monitor rather than this dir exists test`.

## To Reproduce

`cabal-testsuite/PackageTests/UnpackRace` on the `test/unpack-race-condition` branch is a minimal reproducer: a local library, and a repository package `many-exes` with sixteen executables that depend on it and no library, so that the executables are independent units that all start at once. It is named in `extra-packages` and built with `-j`. The package has `cabal-version: 2.0`, so it builds per component, and the test pads it with eight 1 MB files so that each file's write is long enough for the extractions to overlap. With `cabal-version: >= 1.2` instead, the package is one unit and nothing races, and with 1000 empty padding files the double extraction happens but never collides. As it is, it failed eight runs out of eight on a 24-core machine, seven on the file lock and one on the missing cabal file. The test asserts that the build succeeds, so it fails until the race is fixed. `cabal-testsuite/PackageTests/UnpackMonitor` on the same branch covers the reuse of the directory: it builds a repository package in place, then builds again after deleting a source file and the record of the unpack, after removing the directory, and after revising the cabal file to drop a dependency, as a `<pkg-id>.cabal` file beside the tarball in the file+noindex repository. Run either with:

```
cabal run cabal-testsuite:cabal-tests -- --with-cabal="$(cabal list-bin cabal-install:exe:cabal)" cabal-testsuite/PackageTests/UnpackRace/cabal.test.hs
```

The failure was first seen in the wild on the `add/test-external-commands` branch, where `hpack-dhall` is an `extra-packages` entry with a library and eight executables, and it depends on the in-tree `Cabal` through `hpack`. With `jobs: $ncpus` in the user config on a 24-core machine:

```
rm -rf dist-newstyle-external-commands/src/hpack-dhall-0.6.0 dist-newstyle-external-commands/build/x86_64-linux/ghc-9.14.1/hpack-dhall-0.6.0
cabal build --project-file=cabal.external-commands.project --builddir=dist-newstyle-external-commands hpack-dhall
```

Two of five runs failed. The file that is locked differs each time:

```
Error: [Cabal-7125]
Failed to unpack exe:dhall-hpack-yaml from hpack-dhall-0.6.0. The exception was:
  .../dist-newstyle-external-commands/src/hpack-dhall-0.6.0/LICENSE: withFile: resource busy (file is locked)
```

```
Failed to unpack hpack-dhall-0.6.0. The exception was:
  .../dist-newstyle-external-commands/src/hpack-dhall-0.6.0/exe/yaml-hpack-cabal/CabalMain.hs: withFile: resource busy (file is locked)
Failed to build exe:yaml-hpack-cabal from hpack-dhall-0.6.0.
```

```
Failed to unpack exe:cabal-diy-pack from hpack-dhall-0.6.0. The exception was:
  .../dist-newstyle-external-commands/src/hpack-dhall-0.6.0/test-suite-golden/test-files/key/when-dependencies.cabal: withFile: resource busy (file is locked)
```

The other three runs, and every rerun after a failure, succeeded. The testsuite reproducer shows the second symptom too, from a component that found the directory already there and configured while another was still extracting:

```
Failed to build many-exes-0.1-inplace-exe2. The failure occurred during the configure step. The exception was:
  Error: [Cabal-5559]
  [no-cabal-file] No cabal file found.
```

## Expected behavior

The tarball is unpacked once per package, however many components the package has and however many jobs are running, and the build succeeds first time. An unpacked directory is reused only while it is complete and matches the tarball and the revision of the cabal file, and is unpacked again otherwise.

## System information

- Operating system: Linux
- `cabal`: 3.19.0.0 built from master at e326f96be, but the code involved predates it by years
- `ghc`: 9.14.1

## Additional context

The unpack happens in `withTarballLocalDirectory` in `cabal-install/src/Distribution/Client/ProjectBuilding.hs`. The `BuildInplaceOnly` branch, from line 855, checks `doesDirectoryExist srcdir` and calls `unpackPackageTarball` when it does not. Nothing serialises that check-then-unpack across the components of one package: `rebuildTarget` runs per unit of the install plan, and per-component builds make each component of the package its own unit, all of which share one `distUnpackedSrcDirectory pkgid`. Every component that runs the check before the first extraction has created the directory goes on to extract the whole tarball too, and `Tar.extractTarGzFile` then has several writers to the same files. A component that runs the check after the directory appears skips the unpack and configures at once, against a source tree that is still being written. The comment above the check already says `use a proper file monitor rather than this dir exists test`. With verbose output the testsuite reproducer shows two to four `Extracting ...` lines for the one package.

The `BuildAndInstall` branch does not have the problem, since it unpacks into a fresh temporary directory per unit, which is why the same package builds fine into the store.

The unpack should happen once per package before any of its components build, or the components should take a per-package lock around the check and the extraction. Until then, a rerun or `-j1` works around it. The reuse of the directory should be decided by a file monitor, as the comment says, recording the tarball, the revision text and the directory, and written only once an unpack has completed.
