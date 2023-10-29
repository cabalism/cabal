import Test.Cabal.Prelude

import System.FilePath

main = withShorterPathForNewBuildStore $ \storeDir -> cabalTest $ do
    let options = ["--store-dir=" ++ storeDir, "--installdir=" ++ storeDir]
    -- Touch the second exe to trigger a warning that we're overwriting it and
    -- installing the second exe when we only asked to install the first one.
    _ <- runM "touch" [storeDir </> "second-of-many-exes"] Nothing
    fails $ cabalG options "v2-install" []
    -- TODO: Remove the fails when we can install a single exe from a package.
    fails $ cabalG options "v2-install" ["--overwrite-policy=never", "InstallOneOfManyExes:exe:first-of-many-exes"]