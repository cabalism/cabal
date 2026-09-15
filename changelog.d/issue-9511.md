---
synopsis: Hide constraints of a project import with hide-constraints
packages: [cabal-install]
issues: [9511]
---

A project import can now hide the constraints on packages coming from the
imported file and from anything that it imports in turn. This makes it possible
to use a different version of a package than the version pinned by an imported
snapshot, without downloading and editing the snapshot:

```
import: https://www.stackage.org/lts-21.25/cabal.config
  hide-constraints: hashable, text

constraints: hashable ==1.4.2.0, text ==2.0.2
```
