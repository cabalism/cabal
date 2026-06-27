import Test.Cabal.Prelude
import System.Exit (ExitCode(ExitFailure))
import System.IO

-- If tests are enabled then we get this output:
-- [1 of 1] Compiling Main
-- test/Main.hs:4:8: error: [GHC-88464]
--     Variable not in scope: puStrLn :: [Char] -> IO ()
--     Suggested fix: Perhaps use ‘putStrLn’ (imported from Prelude)
--   |
-- 4 | main = puStrLn "Test suite not yet implemented."
--   |
main = cabalTest . recordMode RecordMarked $ do
  projEnabledTests <- fails $ cabal' "build" []
  assertExitCode (ExitFailure 1) projEnabledTests
  assertOutputContains "Failed to build cabal-project-repro-0.1.0.0-inplace-cabal-project-repro-test." projEnabledTests

  cmdDisabledTests <- cabal' "build" ["--disable-tests"]
  assertOutputDoesNotContain "Test suite not yet implement" cmdDisabledTests

  -- Change the imported project file with "Tests: False"
  test_dir <- fmap testTmpDir getTestEnv
  liftIO $ writeFile (test_dir </> "test" </> "tests-toggle.config") "package *\n  Tests: False"
  projDisabledTests <- cabal' "build" []
  assertOutputDoesNotContain "Test suite not yet implement" projDisabledTests
  assertOutputDoesNotContain "Failed to build cabal-project-repro-0.1.0.0-inplace-cabal-project-repro-test." projDisabledTests

  cmdEnabledTests <- fails $ cabal' "build" ["--enable-tests"]
  assertOutputContains "Test suite not yet implement" cmdEnabledTests
