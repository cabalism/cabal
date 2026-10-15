import Control.Monad.IO.Class (MonadIO (..))
import System.Directory (removeFile)
import Distribution.Types.LibraryName (LibraryName (..))
import Distribution.Types.PackageName (mkPackageName)
import Distribution.Types.ComponentName (ComponentName (..))
import Test.Cabal.Plan (DistDirOrBinFile (..), planDistDir)
import Test.Cabal.Prelude

-- A module target narrows `cabal haddock` to that module: only its page is
-- (re)written in the component's documentation directory, while the contents
-- and index pages, the `.haddock` interface file and the pages of the other
-- modules are left as they are.
main = cabalTest . withRepo "repo" $ do
  cabal "haddock" ["A"]
  withPlan $ do
    Just plan <- testPlan <$> getTestEnv
    let DistDir distDir = planDistDir plan (mkPackageName "A") (CLibName LMainLibName)
        docDir = distDir </> "doc" </> "html" </> "A"
        shared = map (docDir </>) ["Bar.html", "index.html", "doc-index.html", "A.haddock"]
        fooPage = docDir </> "Foo.html"
    mapM_ shouldExist (fooPage : shared)
    assertFindInFile "The Bar module, which Foo does not import." (docDir </> "Bar.html")
    liftIO $ mapM_ removeFile (fooPage : shared)

    cabal "haddock" ["Foo"]
    shouldExist fooPage
    assertFindInFile "The Foo module, documented on its own." fooPage
    mapM_ shouldNotExist shared

    liftIO $ removeFile fooPage
    cabal "haddock" ["A/src/Foo.hs"]
    shouldExist fooPage
    mapM_ shouldNotExist shared
