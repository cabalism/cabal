---
synopsis: Compress the file monitor cache files
packages: [cabal-install]
prs:
---

The cache files that `cabal-install` keeps under `dist-newstyle/cache` and in each package's build directory are now gzip compressed. The plan caches, `solver-plan`, `elaborated-plan` and `improved-plan`, are the bulk of them and shrink several times over: on the `cabal` repository itself the three went from 18 MB to 2.4 MB. A cache file written uncompressed by an earlier version is still read, so upgrading does not replan.
