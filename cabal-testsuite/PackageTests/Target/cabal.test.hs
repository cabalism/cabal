import Test.Cabal.Prelude

main = do
  cabalTest' "default-all" $ do
    cabal "clean" []
    cabal "v2-target" []

  cabalTest' "explicit-all" $ do
    cabal "clean" []
    cabal "v2-target" ["all"]

  cabalTest' "all-enable-tests" $ do
    cabal "clean" []
    cabal "v2-target" ["all", "--enable-tests"]

  cabalTest' "all-enable-benches" $ do
    cabal "clean" []
    cabal "v2-target" ["all", "--enable-benchmarks"]

  cabalTest' "everything" $ do
    cabal "clean" []
    cabal "v2-target" ["all", "--enable-tests", "--enable-benchmarks"]

  cabalTest' "all-exes" $ do
    cabal "clean" []
    cabal "v2-target" ["all:exes"]

  cabalTest' "all-tests" $ do
    cabal "clean" []
    cabal "v2-target" ["all:tests"]

  cabalTest' "all-benches" $ do
    cabal "clean" []
    cabal "v2-target" ["all:benches"]
