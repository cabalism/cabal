import Test.Cabal.Prelude

-- Asking for the library explicitly while test-suites are disabled fails, and
-- the message names the stanza that is disabled rather than the kind of the
-- component asked for.
main = cabalTest $ do
    res <- fails $ cabal' "v2-build" ["--disable-tests", "pkg:lib:helper"]
    assertOutputContains "building test suites has been explicitly disabled" res
