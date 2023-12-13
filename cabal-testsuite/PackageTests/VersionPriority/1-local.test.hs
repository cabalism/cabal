import Test.Cabal.Prelude

main = cabalTest . withRepo "repo" . withProjectFile "1-local.project" $ do
    skipUnlessGhcVersion ">= 9.4"
    cabal "v2-build" ["--dry-run"]