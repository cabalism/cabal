import Test.Cabal.Prelude

main = cabalTest $ do
  -- Build from toplevel.
  cabal "build" []
