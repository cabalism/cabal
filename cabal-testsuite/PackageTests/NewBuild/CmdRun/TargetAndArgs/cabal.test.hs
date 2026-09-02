import Test.Cabal.Prelude

-- Telling a target from an argument. The target may be given on either side of
-- '--', and when there is only one thing to run it need not be given at all.
main = do
  -- Regression test for https://github.com/haskell/cabal/issues/12231:
  -- a target named only after '--' used to be handed to the executable.
  cabalTest' "target-after-sep" $ do
    res <- cabal' "run" ["--", "foo", "--randomize", "--strict"]
    assertOutputContains "foo args: [\"--randomize\",\"--strict\"]" res
    assertOutputDoesNotContain "Unrecognised target" res

  cabalTest' "target-before-sep" $ do
    res <- cabal' "run" ["foo", "--", "--randomize"]
    assertOutputContains "foo args: [\"--randomize\"]" res

  -- A leading flag after '--' is not probed, so it stays an argument and the
  -- sole executable of the package in the current directory is implied.
  cabalTest' "arg-after-sep" $ withDirectory "single" $ do
    res <- cabal' "run" ["--", "--randomize"]
    assertOutputContains "solo args: [\"--randomize\"]" res
    assertOutputDoesNotContain "Unrecognised target" res

  -- 'bar' names another executable but sits on the argument side of '--', so
  -- it is passed through rather than treated as a second target.
  cabalTest' "arg-named-like-target" $ do
    res <- cabal' "run" ["foo", "--", "bar"]
    assertOutputContains "foo args: [\"bar\"]" res

  -- 'bar' names an executable but follows a word that does not name anything,
  -- so it is an argument. Without a '--' to say that was intended, say so.
  cabalTest' "target-in-args" $ do
    res <- cabal' "run" ["foo", "zzz", "bar"]
    assertOutputContains "names a component in this project" res
    assertOutputContains "foo args: [\"zzz\",\"bar\"]" res

  -- A word before '--' that does not name a target used to be an error.
  -- It is now an argument, which is worth saying out loud.
  cabalTest' "demoted-before-sep" $ do
    res <- cabal' "run" ["foo", "zzz", "--", "x"]
    assertOutputContains "does not name a target" res
    assertOutputContains "foo args: [\"zzz\",\"x\"]" res

  -- Without '--' a leading word is a target claim, so an unknown one still
  -- fails rather than quietly becoming an argument.
  cabalTest' "unknown-target" $ withDirectory "single" $ do
    res <- fails $ cabal' "run" ["baz"]
    assertOutputDoesNotContain "solo args" res

  cabalTest' "implied-exe" $ withDirectory "single" $ do
    res <- cabal' "run" []
    assertOutputContains "solo args: []" res

  cabalTest' "implied-test" $ withDirectory "onlytest" $ do
    res <- cabal' "run" ["--enable-tests", "--", "--flag"]
    assertOutputContains "only-test args: [\"--flag\"]" res

  cabalTest' "implied-bench" $ withDirectory "onlybench" $ do
    res <- cabal' "run" ["--enable-benchmarks", "--", "--flag"]
    assertOutputContains "only-bench args: [\"--flag\"]" res

  -- The same string twice: caught from the command line alone, before
  -- anything is resolved.
  cabalTest' "repeated-target" $ withDirectory "single" $ do
    res <- cabal' "run" ["solo", "solo"]
    assertOutputContains "given more than once" res
    assertOutputContains "solo args: []" res

  -- Two spellings of one component. They resolve to the very same selector,
  -- so the repetition is only visible from the command line itself.
  cabalTest' "same-target-twice" $ withDirectory "single" $ do
    res <- cabal' "run" ["solo", "exe:solo"]
    assertOutputContains "name the same target" res
    assertOutputContains "solo args: []" res

  -- A component and a wildcard covering it. These are different selectors that
  -- only collapse once resolved, so this is caught after the plan is built.
  cabalTest' "wildcard-over-one-target" $ withDirectory "single" $ do
    res <- cabal' "run" ["solo", "single:exes"]
    assertOutputContains "all refer to the same component" res
    assertOutputContains "solo args: []" res

  -- Two different components is still an error.
  cabalTest' "multiple-targets" $ do
    res <- fails $ cabal' "run" ["foo", "bar"]
    assertOutputContains "single executable at once" res
