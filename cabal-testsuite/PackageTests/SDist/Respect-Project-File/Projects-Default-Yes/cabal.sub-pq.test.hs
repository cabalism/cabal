import Test.Cabal.Prelude

main = cabalTest . withProjectFile "cabal.sub-pq.project" $ do
    cabal "sdist" ["all"]