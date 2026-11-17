import Test.Cabal.Prelude

import System.Directory ( createDirectoryIfMissing )
import qualified Data.ByteString.Char8 as BS

-- Making an sdist reads every data file. It must not hold them all open at
-- once, as there can be more data files than the limit on open files.
main = cabalTest $ do
    liftIO $ skipIfWindows "has no limit on open files to lower"
    cwd <- testCurrentDir <$> getTestEnv
    let limit = 256

    liftIO $ createDirectoryIfMissing False (cwd </> "data")
    forM_ [1 .. limit + 1] $ \i ->
        liftIO $ BS.writeFile (cwd </> "data" </> ("data-file-" ++ show i) <.> "txt") (BS.pack "a data file\n")

    withOpenFilesLimit limit $ cabal "v2-sdist" ["many-data-files"]
