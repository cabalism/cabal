import System.Directory (copyFile, removeFile)
import Test.Cabal.Prelude

-- cabal keeps an index of a file+noindex repository in a cache file. When the
-- package files in the directory change, cabal reads the directory again.
main = cabalTest $ withShorterPathForNewBuildStore $ recordMode DoNotRecord $ withRepo "repo" $ do
  env <- getTestEnv
  let tarball = testRepoDir env </> "other-lib-1.0.tar.gz"
      aside = testCurrentDir env </> "other-lib-1.0.tar.gz"

  -- The update that withRepo runs has written a cache that lists both
  -- packages.
  shouldExist (testRepoDir env </> "noindex.cache")

  -- Take one package away. The solver no longer knows of it.
  liftIO $ copyFile tarball aside >> removeFile tarball
  fails (cabal' "v2-build" ["all"]) >>= assertOutputContains "unknown package: other-lib"

  -- Put it back. The build finds it again. The second build leaves a plan
  -- that the next one would reuse as it is.
  liftIO $ copyFile aside tarball
  cabal "v2-build" ["all"]
  cabal "v2-build" ["all"]

  -- Take it away once more, now that there is a cached plan that uses it.
  liftIO $ removeFile tarball
  fails (cabal' "v2-build" ["all"]) >>= assertOutputContains "unknown package: other-lib"
