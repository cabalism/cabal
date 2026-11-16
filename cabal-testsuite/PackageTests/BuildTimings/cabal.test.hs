import Test.Cabal.Prelude

-- build-timings prints a line for each planning step a run performs and for
-- each package phase. The times vary, so this checks the lines rather than
-- recording the output.
main = cabalTest $ recordMode DoNotRecord $ do
  cold <- cabal' "build" ["--build-timings"]
  mapM_
    (\step -> assertOutputContains ("[build-timings] " ++ step ++ " -") cold)
    ["installed-packages", "source-packages", "solve", "elaborate", "improve"]
  assertOutputContains "[build-timings] solve - " cold
  assertOutputContains "[build-timings] configure timed-0" cold
  assertOutputContains "[build-timings] build timed-0" cold
  -- A run that reuses its cached plan reads nothing and solves nothing, and
  -- has no package phase to time. It may still check the store, which the
  -- first build changed, so the improve line is allowed.
  warm <- cabal' "build" ["--build-timings"]
  mapM_
    (\step -> assertOutputDoesNotContain ("[build-timings] " ++ step) warm)
    ["installed-packages", "source-packages", "solve", "elaborate", "configure timed", "build timed"]
