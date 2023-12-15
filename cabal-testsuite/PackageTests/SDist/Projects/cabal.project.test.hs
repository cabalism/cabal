import Test.Cabal.Prelude

main = cabalTest . withProjectFile "cabal.project" $ do
    cabal "sdist" ["all"]