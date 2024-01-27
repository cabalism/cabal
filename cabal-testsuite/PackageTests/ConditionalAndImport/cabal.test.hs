import Test.Cabal.Prelude
main = cabalTest $
  withRepo "repo" $ do
     cabal "v2-run" [ "some-exe" ]
     fails $ cabal "v2-build" [ "--project-file=cabal-cyclical-loopback.project" ]
     fails $ cabal "v2-build" [ "--project-file=cabal-cyclical-1-hop.project" ]
     fails $ cabal "v2-build" [ "--project-file=cabal-cyclical-2-hop.project" ]
     fails $ cabal "v2-build" [ "--project-file=cabal-bad-conditional.project" ]
