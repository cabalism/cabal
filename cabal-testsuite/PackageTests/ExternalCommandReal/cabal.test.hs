import Test.Cabal.Prelude

import Control.Monad (void)
import System.Environment (lookupEnv)
import qualified System.Process as Process

-- Real external commands, the ones cabal.external-commands.project builds,
-- run through the cabal under test. `make external-commands-bin` puts them
-- in one directory; give it as CABAL_TEST_EXTERNAL_BIN, else this skips.
main = cabalTest $ do
  mbin <- liftIO (lookupEnv "CABAL_TEST_EXTERNAL_BIN")
  case mbin of
    Nothing -> skip "CABAL_TEST_EXTERNAL_BIN is not set; see `make external-commands-bin`"
    Just bin -> addToPath bin $ do
      r <- cabal_raw_action ["plan", "--help"]
      assertOutputContains "cabal-plan" r
      r <- cabal_raw_action ["add", "--help"]
      assertOutputContains "cabal-add" r

cabal_raw_action :: [String] -> TestM Result
cabal_raw_action args = do
  configured_prog <- requireProgramM cabalProgram
  env <- getTestEnv
  r <-
    liftIO $
      runAction
        (Just $ testCurrentDir env)
        (testEnvironment env)
        (programPath configured_prog)
        args
        Nothing
        (\h -> void (Process.waitForProcess h))
  recordLog r
  requireSuccess r
