import Test.Cabal.Prelude

-- A library in the test stanza is not requested when test-suites are not, so
-- neither it nor the test-suite it serves appears in the plan. Compare with
-- enable-tests.test.hs, which differs only in --enable-tests.
main = cabalTest $ do
    cabal "v2-build" ["all"]
