import Test.Cabal.Prelude

-- Unknown extension.
main = cabalTest . recordMode RecordMarked $ do
  fails $ cabal "build" ["--dry-run"]
  --fails $ cabal "check" []
