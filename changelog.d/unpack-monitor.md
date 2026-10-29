---
synopsis: Unpack a tarball package built in place again when its sources are out of date
packages: [cabal-install]
prs:
---

The sources of a non-local package that is built in place, unpacked under
`dist-newstyle/src`, were reused by every later build so long as the directory
existed, whatever it held. A file monitor beside the directory now records the
tarball, the revision of the cabal file and the directory itself, and the
tarball is unpacked again when any of those change, which covers a revision
published after the first build, the directory being removed by hand and an
unpack that was interrupted. Sources unpacked by an earlier version of cabal
have no monitor and are unpacked and built again once.
