import Test.Cabal.Prelude

-- Load a library and its test suite into one GHCi session and run the test
-- suite from the prompt. The test suite must be the first target: cabal makes
-- the first target the active unit, and only the active unit's 'Main' can be
-- reached as 'Main.main'. This example is included verbatim in the user guide
-- (doc/how-to-rerun-tests-on-change.rst).
main = do
  cabalTest $ do
    skipUnlessGhcVersion ">= 9.4"
    res <- cabalWithStdin "v2-repl" ["--enable-multi-repl", "test:never-boils", "lib:watched-pot"] "Main.main"
    assertOutputContains "The watched pot never boils early." res
