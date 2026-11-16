import Test.Cabal.Prelude

-- timings prints a [timing] line for each planning step a run performs and
-- for each package phase. The times vary, so this checks the lines rather than
-- recording the output.
main = cabalTest $ recordMode DoNotRecord $ do
  cold <- cabal' "build" ["--timings"]
  mapM_
    (\step -> assertOutputContains ("[timing] plan:" ++ step ++ " -") cold)
    ["installed-packages", "source-packages", "solve", "elaborate", "improve"]
  assertOutputContains "[timing] plan:solve - " cold
  assertOutputContains "[timing] pkg:configure timed-0" cold
  assertOutputContains "[timing] pkg:build timed-0" cold
  -- A run that reuses its cached plan reads nothing and solves nothing, and
  -- has no package phase to time. It may still check the store, which the
  -- first build changed, so the improve line is allowed.
  warm <- cabal' "build" ["--timings"]
  mapM_
    (\step -> assertOutputDoesNotContain ("[timing] " ++ step) warm)
    ["plan:installed-packages", "plan:source-packages", "plan:solve", "plan:elaborate", "pkg:configure", "pkg:build"]
