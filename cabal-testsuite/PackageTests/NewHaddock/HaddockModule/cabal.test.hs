import Test.Cabal.Prelude

import Control.Monad (void)
import System.Directory (doesFileExist, exeExtension, findExecutable)
import System.FilePath
import qualified System.Process as Process

-- `cabal haddock-module` runs the cabal-haddock-module executable found on
-- the PATH. The one built next to the cabal under test is used, so that both
-- link the same Cabal library, which is what lets the command read the
-- configuration cabal saved for the package.
main = cabalTest $ do
  env <- getTestEnv
  exeDir <- haddockModuleExeDir
  let distDir = testDistDir env
      outDir = distDir </> "haddock-module"
      args =
        ["haddock-module", "--builddir", distDir]
          ++ ["--project-file=" ++ fp | Just fp <- [testCabalProjectFile env]]
  addToPath exeDir $ do
    -- Nothing has been built yet: the command builds the library first.
    r <- cabalHaddockModule (args ++ ["A:Foo"])
    assertOutputContains "Foo.html" r
    shouldExist (outDir </> "A" </> "Foo.html")
    shouldNotExist (outDir </> "A" </> "Bar.html")
    assertFileDoesContain (outDir </> "A" </> "Foo.html") "The exposed Foo module of package A."

    -- A hidden module can be documented too, here without rebuilding.
    void $ cabalHaddockModule (args ++ ["--no-build", "A:Bar"])
    assertFileDoesContain (outDir </> "A" </> "Bar.html") "The hidden Bar module of package A."

    -- Without a package, a module both packages have is ambiguous.
    r <- fails $ cabalHaddockModule (args ++ ["Foo"])
    assertOutputContains "more than one local package" r

    -- The source file settles the package.
    void $ cabalHaddockModule (args ++ ["B/src/Foo.hs"])
    assertFileDoesContain (outDir </> "B" </> "Foo.html") "The Foo module of package B."

    -- A file that is no module of a library.
    r <- fails $ cabalHaddockModule (args ++ ["cabal.project"])
    assertOutputContains "not a module of a local package" r

    -- A module no local package has.
    r <- fails $ cabalHaddockModule (args ++ ["Nope"])
    assertOutputContains "No library of a local package" r

-- | The directory of the cabal-haddock-module executable built next to the
-- cabal under test, or one on the PATH; skips the test without either.
haddockModuleExeDir :: TestM FilePath
haddockModuleExeDir = do
  cabalProg <- requireProgramM cabalProgram
  -- dist-newstyle/.../x/cabal/build/cabal/cabal
  let siblingDir =
        takeDirectory (takeDirectory (takeDirectory (takeDirectory (programPath cabalProg))))
          </> "cabal-haddock-module" </> "build" </> "cabal-haddock-module"
      sibling = siblingDir </> "cabal-haddock-module" <.> exeExtension
  hasSibling <- liftIO (doesFileExist sibling)
  if hasSibling
    then return siblingDir
    else do
      onPath <- liftIO (findExecutable "cabal-haddock-module")
      case onPath of
        Just exe -> return (takeDirectory exe)
        Nothing -> do
          skip "cabal-haddock-module is not built next to cabal nor on the PATH"
          return siblingDir

cabalHaddockModule :: [String] -> TestM Result
cabalHaddockModule args = do
  configured_prog <- requireProgramM cabalProgram
  env <- getTestEnv
  r <-
    liftIO $
      runAction
        (Just $ testCurrentDir env)
        (testEnvironment env)
        (programPath configured_prog)
        args
        Nothing
        (\h -> void (Process.waitForProcess h))
  recordLog r
  requireSuccess r
