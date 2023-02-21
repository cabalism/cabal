import Test.Cabal.Prelude

main = cabalTest $ do
  -- Build from here importing the rest of the project from elsewhere.
  cabal "build" []
