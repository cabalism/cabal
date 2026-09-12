import Test.Cabal.Prelude

-- An executable is not in the test stanza, so it may not depend on a library
-- that is: disabling test-suites would leave it with a missing dependency.
main = cabalTest $ do
    res <- fails $ cabal' "v2-build" ["pkg"]
    assertOutputContains "cross-stanza-dependency" res
