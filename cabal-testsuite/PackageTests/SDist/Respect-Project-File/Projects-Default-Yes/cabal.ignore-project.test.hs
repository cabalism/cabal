import Test.Cabal.Prelude
import Data.List (isPrefixOf)

main = cabalTest $ do
    sdistResult <- fails $ cabal' "sdist" ["all", "--ignore-project"]
    let errorPrefix = "Errors encountered when parsing cabal file"
    assertOutputContains errorPrefix sdistResult
    let xs = lines $ resultOutput sdistResult
    let ys = "..." : dropWhile (not . (errorPrefix `isPrefixOf`)) xs
    recordMode RecordAll . recordLog $ sdistResult { resultOutput = unlines ys }