import Data.List (isInfixOf)
import Test.Cabal.Prelude

-- timings prints a [timing] line for each planning step a run performs and
-- for each package phase. The times vary, so this checks the lines rather than
-- recording the output.
main = cabalTest $ recordMode DoNotRecord $ withRepo "repo" $ do
  cold <- cabal' "build" ["all", "--timings"]
  mapM_
    (\step -> assertOutputContains ("[timing] plan:" ++ step ++ " -") cold)
    planSteps
  -- The seconds come first, right-aligned, then the tag.
  assertOutputMatches "^ *[0-9]+\\.[0-9][0-9][0-9]s \\[timing\\] plan:solve - " cold
  assertOutputContains "[timing] check:status - " cold
  assertOutputContains "[timing] check:installed-packages - " cold
  assertOutputContains "[timing] pkg:configure timed-0" cold
  assertOutputContains "[timing] pkg:build timed-0" cold
  -- A Custom build type compiles its Setup.hs once, inside its configure.
  assertOutputContains "[timing] pkg:setup custom-0" cold
  -- A package from a repository is unpacked before its configure. A local
  -- repository has nothing to download; download.test.hs covers that.
  assertOutputContains "[timing] pkg:unpack remote-0.1" cold
  assertOutputDoesNotContain "[timing] pkg:download" cold
  -- A run that reuses its cached plan reads nothing and solves nothing, and
  -- has no package phase to time. It still checks what there is to do, and
  -- having nothing, does not read the installed package index. It may also
  -- check the store, which the first build changed, so the improve line is
  -- allowed.
  warm <- cabal' "build" ["all", "--timings"]
  assertOutputContains "[timing] check:status - " warm
  mapM_
    (\step -> assertOutputDoesNotContain ("[timing] " ++ step) warm)
    (map ("plan:" ++) (filter (/= "improve") planSteps) ++ ["check:installed-packages", "pkg:unpack", "pkg:configure", "pkg:build", "pkg:setup"])
  -- A repl checks its status like any run, and its repl phase is not timed,
  -- since that would be the length of the GHCi session. It loads the package
  -- in memory, so its build and install phases have nothing to do and say so.
  repl <- cabalWithStdin "repl" ["timed", "--timings"] ":quit"
  assertOutputContains "[timing] check:status - " repl
  assertOutputContains "[timing] pkg:build timed-0 (skipped)" repl
  assertOutputContains "[timing] pkg:install timed-0 (skipped)" repl
  assertOutputDoesNotContain "[timing] pkg:repl" repl
  -- The lines print at any verbosity, so a silent run prints them and
  -- nothing else. The harness's own verbosity is overridden, keeping its
  -- output markers, and the marked output is what cabal printed. The
  -- seconds come first, so the check is that each line has the tag.
  quiet <- cabal' "build" ["all", "--timings", "-vsilent +markoutput +nowrap"]
  assertOutputContains "[timing] check:status - " quiet
  assertEqual
    "lines of a silent run that are not timings"
    []
    (filter (not . ("[timing] " `isInfixOf`)) (lines (getMarkedOutput (resultOutput quiet))))
  where
    -- The steps of a cold plan, in order. configure-compiler is left out:
    -- the compiler cache is shared by the whole builddir, so whether a run
    -- configures the compiler depends on what ran before it.
    planSteps =
      [ "project-config"
      , "local-packages"
      , "installed-packages"
      , "source-packages"
      , "pkg-config-db"
      , "solve"
      , "source-hashes"
      , "configure-programs"
      , "elaborate"
      , "improve"
      ]
