import Test.Cabal.Prelude
import System.Directory (copyFile, removeFile, removePathForcibly)
import System.FilePath ((</>))

-- The tarball of a non-local package that is built in place, here because it
-- depends on the local package, is unpacked under dist-newstyle/src and the
-- unpacked sources are reused by later builds. They have to be unpacked again
-- when an earlier unpack did not complete, when they were removed, and when
-- the cabal file of the package was revised.
main = cabalTest $ recordMode DoNotRecord $ withRepo "repo" $ do
  env <- getTestEnv
  let srcroot = testDistDir env </> "src"
      srcdir = srcroot </> "remote-0.1"
      monitor = srcroot </> "remote-0.1.unpack"
  cabal "build" ["remote"]
  shouldExist monitor
  -- An unpack that did not complete left no monitor behind it.
  liftIO $ do
    removeFile (srcdir </> "Remote.hs")
    removeFile monitor
  cabal "build" ["remote"]
  shouldExist (srcdir </> "Remote.hs")
  -- The unpacked sources were removed by hand.
  liftIO $ removePathForcibly srcdir
  cabal "build" ["remote"]
  shouldExist (srcdir </> "Remote.hs")
  -- The cabal file was revised, dropping the dependency on dep. A
  -- file+noindex repository takes a revision from a <pkg-id>.cabal file
  -- beside the tarball, and its cache, which is never refreshed, has to go
  -- for it to be read.
  liftIO $ do
    copyFile
      (testCurrentDir env </> "revision" </> "remote-0.1.cabal")
      (testRepoDir env </> "remote-0.1.cabal")
    removeFile (testRepoDir env </> "noindex.cache")
  cabal "build" ["remote"]
  assertFileDoesContain (srcdir </> "remote.cabal") "x-revision"
