import Test.Cabal.Prelude

-- Two directories hold the same package, listed in the same project file, so
-- neither outranks the other and cabal refuses to pick one. A source in
-- cabal.project.local would outrank them both; see
-- PackageTests/ProjectConfig/OverridePackages.
--
-- output contains filepaths into /tmp, so we only match parts of the output
main = cabalTest . recordMode DoNotRecord $ do
      liftIO $ skipIfWindows "\\r\\n confused with \\n"

      let msg = unlines
            [ "cabal project has different sources for pkg-one at the same position:"
            , "  .*/pkg-one from cabal.project"
            , "  .*/pkg-two from cabal.project"
            ]

      r <- fails $ cabal' "configure" ["-v1", "pkg-one"]
      assertOutputMatches msg r

      return ()
