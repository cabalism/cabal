import Test.Cabal.Prelude

-- The per-project InstalledPackageIndex is seeded from the package databases
-- with `ghc-pkg dump` only when a package gets configured, not on every run.
-- See Note [Per-project InstalledPackageIndex] in
-- Distribution.Client.ProjectBuilding.
main = cabalTest $ recordMode DoNotRecord $ do
  -- A fresh build configures the package, which reads the databases.
  r1 <- cabal' "build" ["-v3", "all"]
  assertOutputContains "dump --global" r1
  -- An up-to-date build configures nothing, so it must not read them.
  r2 <- cabal' "build" ["-v3", "all"]
  assertOutputContains "Up to date" r2
  assertOutputDoesNotContain "dump --global" r2
  -- A build that reconfigures reads them again.
  r3 <- cabal' "build" ["-v3", "--ghc-options=-DRECONFIGURE", "all"]
  assertOutputContains "dump --global" r3
