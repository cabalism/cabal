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

  -- Only targets and flags belong before '--'. A word there that names no
  -- target is the unrecognised target it looks like, not a quiet argument.
  cabalTest' "unknown-before-sep" $ do
    res <- fails $ cabal' "run" ["foo", "zzz", "--", "x"]
    assertOutputDoesNotContain "foo args:" res

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

  -- Every shape of repetition, each producing exactly one warning. 'solo' and
  -- 'single:exe:solo' resolve to the very same selector; 'single:exes'
  -- resolves to a different one that only collapses onto them once the plan is
  -- built. One check after that point sees all of them.
  cabalTest' "repeated-target" $ withDirectory "single" $ do
    res <- cabal' "run" ["solo", "solo"]
    assertOutputContains "was given more than once" res
    assertOutputContains "solo args: []" res

    res <- cabal' "run" ["solo", "exe:solo"]
    assertOutputContains "all name the same component" res
    assertOutputContains "solo args: []" res

    res <- cabal' "run" ["solo", "single:exes"]
    assertOutputContains "all name the same component" res
    assertOutputContains "solo args: []" res

    res <- cabal' "run" ["solo", "single:exes", "single:exe:solo"]
    assertOutputContains "all name the same component" res
    assertOutputContains "solo args: []" res

  -- Two different components is still an error.
  cabalTest' "multiple-targets" $ do
    res <- fails $ cabal' "run" ["foo", "bar"]
    assertOutputContains "single executable at once" res
