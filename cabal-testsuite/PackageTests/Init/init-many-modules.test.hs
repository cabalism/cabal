import Test.Cabal.Prelude

import System.Directory (createDirectoryIfMissing)

-- For a library, cabal init reads every module in the package directory. It
-- must not hold them all open at once, as there can be more modules than the
-- limit on open files.
main = cabalTest $ do
    liftIO $ skipIfWindows "has no limit on open files to lower"
    cwd <- testCurrentDir <$> getTestEnv
    let limit = 256
        moduleName i = "M" ++ show i

    liftIO $ createDirectoryIfMissing True (cwd </> "src")
    forM_ [1 .. limit + 1] $ \i ->
        liftIO $ writeFile (cwd </> "src" </> moduleName i <.> "hs") ("module " ++ moduleName i ++ " where\n")

    withOpenFilesLimit limit $ cabal "init" ["-n", "--lib", "-p", "many-modules"]

    assertFileDoesContain (cwd </> "many-modules.cabal") (moduleName (limit + 1))
