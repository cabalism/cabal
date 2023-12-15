import Test.Cabal.Prelude

main = cabalTest . withProjectFile "cabal.sub-rs.project" $ do
    cabal "sdist" ["all"]