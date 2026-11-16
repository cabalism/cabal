---
synopsis: Time the planning steps under timings
packages: [cabal-install]
prs:
---

The `timings` setting now also times planning. Each step a run performs prints a `[timing] plan:` line in the same format as the package phases, with `-` in place of the package and a note of what the step covered: reading the installed package databases and the repository indexes, the solver run, elaboration and the store check. A run that reuses its cached plan performs none of them and prints nothing new. Where a slow command spends its time before the first package builds is now visible without a debug log, for instance:

```
[timing] plan:installed-packages - 0.127s (47 packages)
[timing] plan:source-packages - 0.774s (155233 packages)
[timing] plan:solve - 0.738s (159 packages)
[timing] plan:elaborate - 0.267s (185 packages)
[timing] plan:improve - 0.013s (185 packages)
```
