import Test.Cabal.Prelude

-- timings prints a [timing] line for each planning step a run performs and
-- for each package phase. The times vary, so this checks the lines rather than
-- recording the output.
main = cabalTest $ recordMode DoNotRecord $ do
  cold <- cabal' "build" ["all", "--timings"]
  mapM_
    (\step -> assertOutputContains ("[timing] plan:" ++ step ++ " -") cold)
    ["installed-packages", "source-packages", "solve", "elaborate", "improve"]
  assertOutputContains "[timing] plan:solve - " cold
  assertOutputContains "[timing] check:status - " cold
  assertOutputContains "[timing] check:installed-packages - " cold
  assertOutputContains "[timing] pkg:configure timed-0" cold
  assertOutputContains "[timing] pkg:build timed-0" cold
  -- A Custom build type compiles its Setup.hs once, inside its configure.
  assertOutputContains "[timing] pkg:setup custom-0" cold
  -- A run that reuses its cached plan reads nothing and solves nothing, and
  -- has no package phase to time. It still checks what there is to do, and
  -- having nothing, does not read the installed package index. It may also
  -- check the store, which the first build changed, so the improve line is
  -- allowed.
  warm <- cabal' "build" ["all", "--timings"]
  assertOutputContains "[timing] check:status - " warm
  mapM_
    (\step -> assertOutputDoesNotContain ("[timing] " ++ step) warm)
    ["plan:installed-packages", "plan:source-packages", "plan:solve", "plan:elaborate", "check:installed-packages", "pkg:configure", "pkg:build", "pkg:setup"]
  -- A repl checks its status like any run, and its repl phase is not timed,
  -- since that would be the length of the GHCi session. It loads the package
  -- in memory, so its build and install phases have nothing to do and say so.
  repl <- cabalWithStdin "repl" ["timed", "--timings"] ":quit"
  assertOutputContains "[timing] check:status - " repl
  assertOutputMatches "\\[timing\\] pkg:build timed-0 [0-9.]*s \\(skipped\\)" repl
  assertOutputMatches "\\[timing\\] pkg:install timed-0 [0-9.]*s \\(skipped\\)" repl
  assertOutputDoesNotContain "[timing] pkg:repl" repl
