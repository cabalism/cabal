---
synopsis: Replace constraints with override-constraints
packages: [cabal-install]
issues: [9511]
---

Project files accept a new `override-constraints` field, and commands a
`--override-constraint` flag, with the syntax of `constraints`. Where plain
constraints are intersected, an override replaces the other constraints of its
kind on the package, so a version pinned by an imported package set, such as a
Stackage snapshot, can be changed without downloading and editing the snapshot:

```
import: https://www.stackage.org/lts-21.25/cabal.config

override-constraints: any.hashable ==1.4.2.0, any.text ==2.0.2
```

An override replaces constraints at weaker positions: from a less authoritative
layer (the command line, then `cabal.project.local`, then `cabal.project` and
its freeze file, then the global config), or deeper in the import tree. It never
replaces a stronger position, and two different overrides at the same position
are an error. Version ranges and `installed` are one kind and each flag is its
own kind, so flags can be overridden individually.
