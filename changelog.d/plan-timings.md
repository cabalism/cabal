---
synopsis: Time the planning steps under timings
packages: [cabal-install]
prs:
---

The `timings` setting now also times planning and the checks before building. Each planning step a run performs prints a `[timing] plan:` line in the same format as the package phases, with `-` in place of the package and a note of what the step covered: reading the installed package databases and the repository indexes, the solver run, elaboration and the store check. A run that reuses its cached plan performs none of them and prints nothing new. Two `[timing] check:` lines follow: the status check of every package against the previous run, and the read of the installed package index that a run makes before it builds anything. A package with a `Custom` or `Hooks` build type prints `[timing] pkg:setup` for the compile of its setup script, which until now was hidden inside its configure time. Where a slow command spends its time before the first package builds is now visible without a debug log, for instance:

```
[timing] plan:installed-packages - 0.127s (47 packages)
[timing] plan:source-packages - 0.774s (155233 packages)
[timing] plan:solve - 0.738s (159 packages)
[timing] plan:elaborate - 0.267s (185 packages)
[timing] plan:improve - 0.013s (185 packages)
[timing] check:status - 0.021s (185 packages)
[timing] check:installed-packages - 0.842s (3417 packages)
```
