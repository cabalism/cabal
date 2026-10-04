import Test.Cabal.Prelude

-- The setup script can only be built with libA-0.1.0.0 and the library only
-- with libA-0.2.0.0. Each prints the version it was built with.
--
-- Check that
--    cabal freeze --constraint=... && cabal build
-- gives the same plan as
--    cabal build --constraint=...
-- which needs the freeze file to say which version of libA goes with which
-- scope. Without that the build after the freeze picks libA-0.2.0.0 for the
-- setup script too, and fails.
main = do
  cabalTest' "constraint" . recordMode DoNotRecord $
    withRepo "repo" $ do
      out <- cabal' "v2-build" ["--constraint=setup.libA == 0.1.0.0"]
      assertOutputContains "Setup: libA-0.1.0.0" out
      assertOutputContains "Building: libA-0.2.0.0" out

  cabalTest' "freeze" . recordMode DoNotRecord $
    withRepo "repo" $ do
      cabal "v2-freeze" ["--constraint=setup.libA == 0.1.0.0"]

      cwd <- fmap testCurrentDir getTestEnv
      let freezeFile = cwd </> "cabal.project.freeze"
      assertFileDoesContain freezeFile "any.libA ==0.1.0.0 || ==0.2.0.0"
      assertFileDoesContain freezeFile " libA ==0.2.0.0"
      assertFileDoesContain freezeFile "setup.libA ==0.1.0.0"

      out <- cabal' "v2-build" []
      assertOutputContains "Setup: libA-0.1.0.0" out
      assertOutputContains "Building: libA-0.2.0.0" out
