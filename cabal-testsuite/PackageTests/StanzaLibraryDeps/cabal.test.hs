import Test.Cabal.Prelude

-- The dependencies of a library in the test stanza are not solved for when
-- test-suites are not requested. 'helper' names a package that does not exist,
-- so the solve would fail if they were; the main library builds regardless.
--
-- This is the property that keeps such a library out of a plan that sets
-- tests: False, as cabal.bootstrap.project does.
main = cabalTest $ do
    cabal "v2-build" ["--disable-tests", "all"]
