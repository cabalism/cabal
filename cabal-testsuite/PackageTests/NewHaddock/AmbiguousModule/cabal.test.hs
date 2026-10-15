import Distribution.Types.ComponentName (ComponentName (..))
import Distribution.Types.LibraryName (LibraryName (..))
import Distribution.Types.PackageName (mkPackageName)
import Test.Cabal.Plan (DistDirOrBinFile (..), planDistDir)
import Test.Cabal.Prelude

-- A module target to `cabal haddock` resolves like any other module target:
-- a name that several components share is an error until it is qualified
-- enough, and a qualified one documents just that component's module.
main = cabalTest . withRepo "repo" $ do
  r <- fails $ cabal' "haddock" ["Foo"]
  assertOutputContains "Ambiguous target 'Foo'" r
  r' <- fails $ cabal' "haddock" ["A:Foo"]
  assertOutputContains "Ambiguous target 'A:Foo'" r'

  cabal "haddock" ["lib:A:Foo"]
  withPlan $ do
    Just plan <- testPlan <$> getTestEnv
    let fooPage pkg = case planDistDir plan (mkPackageName pkg) (CLibName LMainLibName) of
          DistDir distDir -> distDir </> "doc" </> "html" </> pkg </> "Foo.html"
          BinFile _ -> error "fooPage: expected a dist dir"
    shouldExist (fooPage "A")
    assertFindInFile "The Foo module of the library of A." (fooPage "A")
    shouldNotExist (fooPage "B")

    cabal "haddock" ["B:Foo"]
    shouldExist (fooPage "B")
    assertFindInFile "The Foo module of the library of B." (fooPage "B")
