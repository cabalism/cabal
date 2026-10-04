import Test.Cabal.Prelude

-- The plan has two instances of dep-X and two of dep-Y:
--
-- * pkg-A depends on dep-X-1.0 as a library and on dep-X-2.0 for its setup
--   script, so one instance of dep-X is at the top level.
-- * pkg-A depends on dep-Y-2.0 for its setup script and its build tool
--   depends on dep-Y-1.0, so no instance of dep-Y is at the top level.
--
-- Each version of both has two flags. flag-g has to be off for all of them.
-- Version 1.0 can only be built with flag-f on and version 2.0 only with it
-- off.
--
-- 'cabal v2-freeze' writes a flag constraint for a package at the top level,
-- and so for only one of its instances. It should write the flags of the
-- top-level instance if there is one, and otherwise only the flags that the
-- instances agree on. It should also write the flags of the instances that
-- are only setup dependencies, as constraints on the setup scope. See #5134.
main = cabalTest $
  withRepo "repo" $ do
    cabal "v2-build" ["--dry-run"]

    cabal "v2-freeze" []

    cwd <- fmap testCurrentDir getTestEnv
    let freezeFile = cwd </> "cabal.project.freeze"

    -- A constraint at the top level starts the line, after its indentation.
    assertFileDoesContain freezeFile "any.dep-X ==1.0 || ==2.0"
    assertFileDoesContain freezeFile " dep-X ==1.0"
    assertFileDoesContain freezeFile "setup.dep-X ==2.0"
    assertFileDoesContain freezeFile " dep-X +flag-f -flag-g"
    assertFileDoesContain freezeFile "setup.dep-X -flag-f -flag-g"

    assertFileDoesContain freezeFile "any.dep-Y ==1.0 || ==2.0"
    assertFileDoesNotContain freezeFile " dep-Y =="
    assertFileDoesContain freezeFile "setup.dep-Y ==2.0"
    assertFileDoesContain freezeFile " dep-Y -flag-g"
    assertFileDoesNotContain freezeFile " dep-Y +flag-f"
    assertFileDoesNotContain freezeFile " dep-Y -flag-f"
    assertFileDoesContain freezeFile "setup.dep-Y -flag-f -flag-g"

    -- The freeze file should not stop cabal from finding the plan it was
    -- written from.
    cabal "v2-build" ["--dry-run"]
