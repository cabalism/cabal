import Test.Cabal.Prelude

main = withShorterPathForNewBuildStore $ \storeDir -> cabalTest $ do
    let options = ["--store-dir=" ++ storeDir, "--installdir=" ++ storeDir]
    -- Use install method copy that should surely work on Windows too.
    cabalG options "v2-install" ["--install-method=copy"]