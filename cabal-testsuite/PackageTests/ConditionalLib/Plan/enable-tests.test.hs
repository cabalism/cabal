import Test.Cabal.Prelude

-- With test-suites requested, the library in the test stanza is requested too
-- and is built alongside them. Compare with cabal.test.hs.
main = cabalTest $ do
    cabal "v2-build" ["--enable-tests", "all"]
