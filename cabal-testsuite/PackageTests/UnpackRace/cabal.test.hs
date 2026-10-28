import Test.Cabal.Prelude

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

-- A non-local package that depends on a local one is built in place, and
-- each of its components unpacks the tarball into the same source directory.
-- Nothing serialises them, so the executables of many-exes, independent units
-- that all start at once, race: either two extractions write the same file at
-- once and one fails with "withFile: resource busy (file is locked)", or a
-- unit that found the directory already there configures while another is
-- still extracting and fails with "No cabal file found". The padding files
-- make each file's write long enough for the extractions to overlap.
main = cabalTest $ recordMode DoNotRecord $ do
  env <- getTestEnv
  let pad = testCurrentDir env </> "repo" </> "many-exes-0.1" </> "pad"
  liftIO $ do
    createDirectoryIfMissing True pad
    forM_ [1 .. 8 :: Int] $ \i ->
      BS.writeFile (pad </> show i) (BS.replicate (1024 * 1024) 0)
  withRepo "repo" $
    cabal "build" ["many-exes", "-j"]
