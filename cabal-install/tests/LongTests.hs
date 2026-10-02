module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit (testCaseInfo)

import qualified UnitTests.Distribution.Client.Described
import qualified UnitTests.Distribution.Client.FileMonitor
import qualified UnitTests.Distribution.Client.VCS
import qualified UnitTests.Distribution.Solver.Modular.QuickCheck
import qualified UnitTests.Distribution.Solver.Modular.QuickCheck.Oracle
import qualified UnitTests.Distribution.Solver.Modular.QuickCheck.Oracle.SMT
import UnitTests.Options

main :: IO ()
main = do
  z3 <- UnitTests.Distribution.Solver.Modular.QuickCheck.Oracle.SMT.findZ3
  defaultMainWithIngredients
    (includingOptions extraOptions : defaultIngredients)
    (tests z3)

-- | The tests that need Z3 are skipped when it is not installed.
tests :: Maybe FilePath -> TestTree
tests z3 =
  askOption $ \(OptionMtimeChangeDelay mtimeChange) ->
    testGroup
      "Long-running tests"
      [ testGroup
          "Solver oracle"
          UnitTests.Distribution.Solver.Modular.QuickCheck.Oracle.tests
      , testGroup "Solver SMT oracle" $
          maybe
            [testCaseInfo "skipped" (return "z3 is not on the PATH")]
            UnitTests.Distribution.Solver.Modular.QuickCheck.Oracle.SMT.tests
            z3
      , testGroup "Solver QuickCheck" $
          UnitTests.Distribution.Solver.Modular.QuickCheck.tests
            ++ maybe [] UnitTests.Distribution.Solver.Modular.QuickCheck.smtTests z3
      , testGroup "UnitTests.Distribution.Client.VCS" $
          UnitTests.Distribution.Client.VCS.tests mtimeChange
      , testGroup "UnitTests.Distribution.Client.FileMonitor" $
          UnitTests.Distribution.Client.FileMonitor.tests mtimeChange
      , UnitTests.Distribution.Client.Described.tests
      ]
