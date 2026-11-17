---
synopsis: Time the planning steps under timings
packages: [cabal-install]
prs:
---

The `timings` setting now also times planning and the checks before building. Each planning step a run performs prints a `[timing] plan:` line in the same format as the package phases, with `-` in place of the package and a note of what the step covered: reading the project file and the local `.cabal` files, configuring the compiler and the other programs, reading the installed package databases, the repository indexes and the `pkg-config` database, the solver run, the source hashes, elaboration and the store check. A run that reuses its cached plan performs none of them and prints nothing new. Two `[timing] check:` lines follow: the status check of every package against the previous run, and the read of the installed package index that a run makes before it builds anything. A package with a `Custom` or `Hooks` build type prints `[timing] pkg:setup` for the compile of its setup script, which until now was hidden inside its configure time. A package phase with nothing to do, such as the build and install phases of a package that `cabal repl` loads in memory, prints its line with the note `(skipped)` rather than a bare `0.000s`. Where a slow command spends its time before the first package builds is now visible without a debug log, for instance:

```
[timing] plan:project-config - 0.019s
[timing] plan:configure-compiler - 0.094s
[timing] plan:local-packages - 0.063s (15 packages)
[timing] plan:installed-packages - 0.132s (47 packages)
[timing] plan:source-packages - 0.786s (155233 packages)
[timing] plan:pkg-config-db - 0.020s (251 packages)
[timing] plan:solve - 0.752s (159 packages)
[timing] plan:source-hashes - 0.029s (118 packages)
[timing] plan:configure-programs - 0.046s (11 programs)
[timing] plan:elaborate - 0.191s (185 packages)
[timing] plan:improve - 0.013s (185 packages)
[timing] check:status - 0.021s (185 packages)
[timing] check:installed-packages - 0.842s (3417 packages)
```
