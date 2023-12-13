import Test.Cabal.Prelude

main = cabalTest . withRepo "repo" . withProjectFile "3-web.project" $ do
    skipUnlessGhcVersion "== 9.6.3"
    cabal "v2-build" ["--dry-run"]