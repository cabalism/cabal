---
synopsis: Unpack a tarball package built in place once, and again when it is out of date
packages: [cabal-install]
prs:
---

A non-local package that depends on a local package is built in place, under
`dist-newstyle/src`, and when it is built per component each of its components
unpacked its tarball there, with nothing stopping them from doing so at the same
time under `--jobs`. A build could then fail with `withFile: resource busy (file
is locked)` from two extractions writing the same file, or with `No cabal file
found` from a component configuring while another was still extracting. The
unpacking is now serialised, so the tarball is extracted once.

The unpacked sources were also reused by every later build so long as the
directory existed, whatever it held. A file monitor beside the directory now
records the tarball, the revision of the cabal file and the directory itself,
and the tarball is unpacked again when any of those change, which covers a
revision published after the first build, the directory being removed by hand
and an unpack that was interrupted. Sources unpacked by an earlier version of
cabal have no monitor and are unpacked and built again once.
