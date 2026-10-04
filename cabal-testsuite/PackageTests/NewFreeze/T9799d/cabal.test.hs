import Test.Cabal.Prelude

-- pkg-A depends on dep-X-2.0 and on dep-Z. The setup script of dep-Z needs an
-- older dep-X, so the plan also has dep-X-1.0. Each version of dep-X depends
-- on the same version of dep-Y, and has a flag.
--
-- This is the shape of a package that depends on a recent Cabal library and
-- on a package with a custom setup that has an upper bound on Cabal, where
-- dep-X stands for Cabal, dep-Y for Cabal-syntax and dep-Z for the package
-- with the custom setup.
--
-- 'cabal v2-freeze' should say which versions of dep-X and dep-Y are at the
-- top level and which are setup dependencies, including dep-Y, which the
-- setup script only depends on through dep-X. See #9799.
main = cabalTest $
  withRepo "repo" $ do
    cabal "v2-freeze" []

    cwd <- fmap testCurrentDir getTestEnv
    let freezeFile = cwd </> "cabal.project.freeze"

    -- A constraint at the top level starts the line, after its indentation.
    assertFileDoesContain freezeFile "any.dep-X ==1.0 || ==2.0"
    assertFileDoesContain freezeFile " dep-X ==2.0"
    assertFileDoesContain freezeFile "setup.dep-X ==1.0"
    assertFileDoesContain freezeFile " dep-X -flag-g"
    assertFileDoesContain freezeFile "setup.dep-X -flag-g"

    assertFileDoesContain freezeFile "any.dep-Y ==1.0 || ==2.0"
    assertFileDoesContain freezeFile " dep-Y ==2.0"
    assertFileDoesContain freezeFile "setup.dep-Y ==1.0"

    -- dep-Z has one version, so it only has the constraint for any scope.
    assertFileDoesContain freezeFile "any.dep-Z ==1.0"
    assertFileDoesNotContain freezeFile " dep-Z =="
    assertFileDoesNotContain freezeFile "setup.dep-Z"

    -- The freeze file should not stop cabal from finding the plan it was
    -- written from.
    out <- cabal' "v2-build" ["--dry-run"]
    assertOutputContains "dep-X-1.0 (lib)" out
    assertOutputContains "dep-X-2.0 (lib)" out
