---
synopsis: Time the planning steps under timings
packages: [cabal-install]
prs:
---

The `timings` setting now also times planning and the checks before building. Each planning step a run performs prints a `[timing] plan:` line in the same format as the package phases, with `-` in place of the package and a note of what the step covered: reading the project file and the local `.cabal` files, configuring the compiler and the other programs, reading the installed package databases, the repository indexes and the `pkg-config` database, the solver run, the source hashes, elaboration and the store check. A run that reuses its cached plan performs none of them and prints nothing new. Two `[timing] check:` lines follow: the status check of every package against the previous run, and the read of the installed package index that a run makes before it builds anything. A package fetched from a repository prints `[timing] pkg:download` for the fetch and `[timing] pkg:unpack` for the extraction. A package with a `Custom` or `Hooks` build type prints `[timing] pkg:setup` for the compile of its setup script, which until now was hidden inside its configure time. A package phase with nothing to do, such as the build and install phases of a package that `cabal repl` loads in memory, prints its line with the note `(skipped)` rather than a bare `0.000s`. The lines print at any verbosity, so `cabal build -v0 --timings` prints the timings and nothing else. Where a slow command spends its time before the first package builds is now visible without a debug log, for instance:

```
   0.019s [timing] plan:project-config -
   0.094s [timing] plan:configure-compiler -
   0.063s [timing] plan:local-packages - (15 packages)
   0.132s [timing] plan:installed-packages - (47 packages)
   0.786s [timing] plan:source-packages - (155233 packages)
   0.020s [timing] plan:pkg-config-db - (251 packages)
   0.752s [timing] plan:solve - (159 packages)
   0.029s [timing] plan:source-hashes - (118 packages)
   0.046s [timing] plan:configure-programs - (11 programs)
   0.191s [timing] plan:elaborate - (185 packages)
   0.013s [timing] plan:improve - (185 packages)
   0.021s [timing] check:status - (185 packages)
   0.842s [timing] check:installed-packages - (3417 packages)
```
