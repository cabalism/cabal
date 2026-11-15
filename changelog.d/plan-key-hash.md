---
synopsis: "Keep a plan cache entry per key so alternating commands do not re-solve"
packages: [cabal-install]
prs:
issues: 11623
---

The solver, elaborated and improved plan caches held one entry each, keyed on
the project configuration. Two commands whose configurations differ evicted
each other's entry, so alternating between them re-solved the plan on every
switch: `cabal build` and `cabal repl --enable-multi-repl` or `--with-repl`,
which add a `setup.Cabal` constraint, or a build with and without a
`--constraint`. The caches now keep a file per distinct key, named by a hash
of the key, with the four most recently written kept, and `plan.json` is
rewritten whenever the plan in use changes.
