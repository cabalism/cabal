import Test.Cabal.Prelude

-- Which libraries are requested, as the optional stanzas are turned on and off.
-- 'shared' belongs to both stanzas, so it appears whenever either is on.
main = do
  cabalTest' "neither" $
    cabal "v2-build" ["all"]

  cabalTest' "tests" $
    cabal "v2-build" ["--enable-tests", "all"]

  cabalTest' "benchmarks" $
    cabal "v2-build" ["--enable-benchmarks", "all"]

  cabalTest' "both" $
    cabal "v2-build" ["--enable-tests", "--enable-benchmarks", "all"]
