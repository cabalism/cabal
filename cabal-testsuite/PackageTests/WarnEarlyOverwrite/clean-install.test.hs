import Test.Cabal.Prelude

main = withShorterPathForNewBuildStore $ \storeDir -> cabalTest $ do
    let options = ["--store-dir=" ++ storeDir, "--installdir=" ++ storeDir]
    cabalG options "v2-install" []