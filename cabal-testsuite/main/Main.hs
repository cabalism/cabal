-- | The @cabal-tests@ test-suite. The test driver is in the library, see
-- "Test.Cabal.Driver".
module Main (main) where

import Test.Cabal.Driver (cabalTestsMain)
import Test.Cabal.ScriptEnv0 (scriptPackageDbs, scriptPackageIds)

main :: IO ()
main = cabalTestsMain scriptPackageDbs scriptPackageIds
