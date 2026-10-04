import Test.Cabal.Prelude

-- pkg-A and pkg-B both depend on dep-X for their setup scripts, and nothing
-- depends on it as a library. A constraint makes pkg-A use dep-X-1.0, and
-- pkg-B uses the latest, dep-X-2.0.
--
-- The setup scopes differ, so 'cabal v2-freeze' should write a constraint
-- for the setup scope of each package. With those the build after the
-- freeze still has both versions. See #9799.
main = cabalTest $
  withRepo "repo" $ do
    cabal "v2-freeze" ["--constraint=pkg-A:setup.dep-X == 1.0"]

    cwd <- fmap testCurrentDir getTestEnv
    let freezeFile = cwd </> "cabal.project.freeze"
    assertFileDoesContain freezeFile "any.dep-X ==1.0 || ==2.0"
    assertFileDoesNotContain freezeFile " setup.dep-X"
    assertFileDoesContain freezeFile "pkg-A:setup.dep-X ==1.0"
    assertFileDoesContain freezeFile "pkg-B:setup.dep-X ==2.0"

    out <- cabal' "v2-build" ["--dry-run", "all"]
    assertOutputContains "dep-X-1.0 (lib)" out
    assertOutputContains "dep-X-2.0 (lib)" out
