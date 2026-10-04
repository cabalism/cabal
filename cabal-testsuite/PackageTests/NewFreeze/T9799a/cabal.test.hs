import Test.Cabal.Prelude

-- T9799 depends on libA-0.1.0.0 as a library and on libA-0.2.0.0 for its
-- setup script.
main = cabalTest $ do
  withRepo "repo" $ do
    cabal "v2-freeze" []
    cwd <- fmap testCurrentDir getTestEnv
    let freezeFile = cwd </> "cabal.project.freeze"

    -- Freeze allows both versions in any scope, and then says which version
    -- is at the top level and which is a setup dependency. A constraint at
    -- the top level starts the line, after its indentation.
    assertFileDoesContain freezeFile "any.libA ==0.1.0.0 || ==0.2.0.0"
    assertFileDoesContain freezeFile " libA ==0.1.0.0"
    assertFileDoesContain freezeFile "setup.libA ==0.2.0.0"

    -- Guarantee that freeze writes scope-qualified constraints only, and no
    -- 'any' qualified constraints.
    expectBroken 9799 $
      assertFileDoesNotContain freezeFile "any.libA"
