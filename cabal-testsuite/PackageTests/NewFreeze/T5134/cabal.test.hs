import Test.Cabal.Prelude

-- pkg-A depends on dep-X-1.0 as a library and on dep-X-2.0 for its setup
-- script. Both versions have a flag, flag-f. dep-X-1.0 can only be built
-- with it on and dep-X-2.0 only with it off, so the plan has both.
--
-- 'cabal v2-freeze' writes the flags of one instance of each package, as a
-- constraint on the package at the top level. Here it writes those of
-- dep-X-2.0, which then apply to dep-X-1.0.
main = cabalTest $
  withRepo "repo" $ do
    cabal "v2-build" ["--dry-run"]

    cabal "v2-freeze" []

    cwd <- fmap testCurrentDir getTestEnv
    let freezeFile = cwd </> "cabal.project.freeze"
    assertFileDoesContain freezeFile "any.dep-X ==1.0 || ==2.0"

    -- The freeze file should not stop cabal from finding the plan it was
    -- written from.
    expectBroken 5134 $
      cabal "v2-build" ["--dry-run"]
