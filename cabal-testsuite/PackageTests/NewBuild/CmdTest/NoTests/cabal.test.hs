import Test.Cabal.Prelude

-- Package p has a test suite, package q does not.
--
-- Every scenario is run as is and, in the mode with the
-- "_+failflag" suffix, with --test-fail-when-no-test-suites.
-- The flag turns a skipped target into an error and is otherwise inert.

flag :: String
flag = "--test-fail-when-no-test-suites"

main = do
  -- Requesting both must skip q with a notice and still run the tests of p.
  cabalTest' "mixed" $ do
    res <- cabal' "v2-test" ["p", "q"]
    assertOutputContains "No tests to run for the package q-0.1" res
    assertOutputDoesNotContain "No tests to run for the package p-0.1" res
    assertOutputContains "Test suite p-tests: PASS" res

  -- With the flag q having no tests triggers the error.
  cabalTest' "mixed_+failflag" $ do
    res <- fails $ cabal' "v2-test" ["p", "q", flag]
    assertOutputContains "Cannot run tests for the target 'q'" res
    assertOutputDoesNotContain "Test suite p-tests: PASS" res

  -- With "all" as target, the tests of p are found.
  cabalTest' "all" $ do
    res <- cabal' "v2-test" ["all"]
    assertOutputDoesNotContain "No tests to run" res
    assertOutputContains "Test suite p-tests: PASS" res

  -- Same thing even with the fail flag.
  cabalTest' "all_+failflag" $ do
    res <- cabal' "v2-test" ["all", flag]
    assertOutputDoesNotContain "No tests to run" res
    assertOutputContains "Test suite p-tests: PASS" res

  -- Same again with the :tests filter.
  cabalTest' "all-tests" $ do
    res <- cabal' "v2-test" ["all:tests"]
    assertOutputDoesNotContain "No tests to run" res
    assertOutputContains "Test suite p-tests: PASS" res

  -- No difference even with the fail flag.
  cabalTest' "all-tests_+failflag" $ do
    res <- cabal' "v2-test" ["all:tests", flag]
    assertOutputDoesNotContain "No tests to run" res
    assertOutputContains "Test suite p-tests: PASS" res

  -- When no target has tests, the command succeeds and reports skipped targets.
  cabalTest' "only-no-tests" $ do
    res <- cabal' "v2-test" ["q"]
    assertOutputContains "No tests to run for the package q-0.1" res
    assertOutputDoesNotContain "Test suite p-tests" res

  -- The fail flag causes the command to fail when no tests are found.
  cabalTest' "only-no-tests_+failflag" $ do
    res <- fails $ cabal' "v2-test" ["q", flag]
    assertOutputContains "Cannot run tests for the target 'q'" res
