---
synopsis: Serialise unpacking of a tarball package built in place
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
