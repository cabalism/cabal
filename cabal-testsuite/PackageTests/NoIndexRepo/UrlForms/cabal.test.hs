import System.Directory (createDirectoryIfMissing)
import Test.Cabal.Prelude

-- The forms of URL that a file+noindex repository accepts for an absolute
-- path. Each is tried in a project of its own repository, the only active
-- one, so the dependency can only come from the URL being tested.
main = cabalTest $ do
  env <- getTestEnv
  let repoDir = testRepoDir env
      -- A path as written in a URL, with forward slashes on every system.
      path = map (\c -> if c == '\\' then '/' else c) repoDir

  liftIO $ createDirectoryIfMissing True repoDir
  (testCurrentDir env </> "repo" </> "dep-1.0") `archiveTo` (repoDir </> "dep-1.0.tar.gz")

  -- The path directly after the scheme. This is the one form that works on
  -- every system.
  dependencyFoundWith "path-after-scheme" ("file+noindex:" ++ path)

  -- These forms need a path that starts with a slash, which a Windows path
  -- with a drive letter does not.
  unless isWindows $ do
    -- An empty host, giving three slashes in a row.
    dependencyFoundWith "empty-host" ("file+noindex://" ++ path)
    dependencyFoundWith "localhost" ("file+noindex://localhost" ++ path)

-- | Check that a project whose only active repository has the given URL finds
-- its dependency there.
dependencyFoundWith :: String -> String -> TestM ()
dependencyFoundWith name url = do
  writeSourceFile "cabal.project" . unlines $
    [ "packages: ."
    , "repository " ++ name
    , "  url: " ++ url
    , "active-repositories: " ++ name
    ]
  cabal' "v2-build" ["--dry-run", "all"] >>= assertOutputContains "dep-1.0 (lib)"
