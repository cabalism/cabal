import Test.Cabal.Prelude

main = cabalTest $ do
    cabal "sdist" ["all", "--ignore-project"]