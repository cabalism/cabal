import Test.Cabal.Prelude

-- pkg-A depends on dep-X-1.0 as a library and on dep-X-2.0 for its setup
-- script, so the plan has both. Each version has two flags. flag-g has to be
-- off for both. dep-X-1.0 can only be built with flag-f on and dep-X-2.0 only
-- with it off.
--
-- 'cabal v2-freeze' writes a flag constraint for a package at the top level,
-- and so for only one of its instances. It should write the flags that the
-- instances agree on and leave out the others. See #5134.
main = cabalTest $
  withRepo "repo" $ do
    cabal "v2-build" ["--dry-run"]

    cabal "v2-freeze" []

    cwd <- fmap testCurrentDir getTestEnv
    let freezeFile = cwd </> "cabal.project.freeze"
    assertFileDoesContain freezeFile "any.dep-X ==1.0 || ==2.0"
    assertFileDoesContain freezeFile "dep-X -flag-g"
    assertFileDoesNotContain freezeFile "flag-f"

    -- The freeze file should not stop cabal from finding the plan it was
    -- written from.
    cabal "v2-build" ["--dry-run"]
