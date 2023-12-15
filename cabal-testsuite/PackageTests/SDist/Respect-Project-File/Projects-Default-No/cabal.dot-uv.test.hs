import Test.Cabal.Prelude

main = cabalTest . withProjectFile "cabal.dot-uv.project" $ do
    cabal "sdist" ["all"]