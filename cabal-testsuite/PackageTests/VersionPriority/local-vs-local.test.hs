import Test.Cabal.Prelude
import System.Directory

main = cabalTest $ do
    cabal "update" []
    cabal "v2-build" ["--project-file=cabal.local-vs-stackage.project",  "--dry-run"]
    -- cabal' "v2-freeze" ["--project-file=cabal.local-vs-web.project",  "--dry-run"]

    -- cwd <- fmap testCurrentDir getTestEnv
    -- let freezeFile = cwd </> "cabal.local-vs-local.project.freeze"

    -- assertFileDoesContain freezeFile "any.hashable ==1.4.2.0"
    -- assertFileDoesNotContain freezeFile "any.hashable ==1.4.3.0"