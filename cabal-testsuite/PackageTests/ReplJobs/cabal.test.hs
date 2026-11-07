import Test.Cabal.Prelude
import System.FilePath ((</>))

-- `cabal repl` passes the project's jobs setting to GHCi as -j, so that the
-- interpreter compiles the component's modules in parallel when it loads
-- them. A stand-in for GHCi prints the arguments it receives.
main = do
  skipIfWindows "requires a POSIX shell script as the --with-repl program"

  -- An explicit -j reaches GHCi.
  cabalTest' "jobs-4" $ recordMode DoNotRecord $ do
    env <- getTestEnv
    let fakeRepl = testCurrentDir env </> "scripts" </> "print-args.sh"
    res <- cabalWithStdin "v2-repl" ["-j4", "--with-repl=" ++ fakeRepl] ""
    assertOutputContains "repl-arg: --interactive" res
    assertOutputContains "repl-arg: -j4" res

  -- A single job means a serial load, so GHCi gets no -j at all. (The test
  -- harness runs every v2 command with -j1, so this is also the default here.)
  cabalTest' "jobs-1" $ recordMode DoNotRecord $ do
    env <- getTestEnv
    let fakeRepl = testCurrentDir env </> "scripts" </> "print-args.sh"
    res <- cabalWithStdin "v2-repl" ["-j1", "--with-repl=" ++ fakeRepl] ""
    assertOutputContains "repl-arg: --interactive" res
    assertOutputDoesNotContain "repl-arg: -j" res
