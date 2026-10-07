import Distribution.System (OS (Windows), buildOS)
import System.FilePath ((</>))
import qualified System.FilePath.Posix as Posix
import qualified System.FilePath.Windows as Windows
import Test.Cabal.Prelude

import Control.Monad (forM_)

-- | Several sources for one package: the source listed at the strongest
-- position wins. @cabal.project.local@ outranks @cabal.project@, and a project
-- file outranks the files it imports. Two different sources at the same
-- position are an error.
--
-- The git repository @upstream@ holds @foo-0.1@ at its first commit and
-- @foo-0.4@ at its second. The directories @foo-0.2@, @foo-0.2-copy@ and
-- @foo-0.3@ hold the versions their names say.
main :: IO ()
main = cabalTest . recordMode DoNotRecord $ do
  env <- getTestEnv

  (commit01, commit04) <- withDirectory "upstream" $ do
    git "init" []
    git "config" ["user.email", "testsuite@example.invalid"]
    git "config" ["user.name", "Cabal Testsuite"]
    git "config" ["commit.gpgsign", "false"]
    git "add" ["foo"]
    git "commit" ["-m", "foo-0.1"]
    commit01 <- headCommit
    writeSourceFile ("foo" </> "foo.cabal") (fooCabal "0.4")
    git "commit" ["-am", "foo-0.4"]
    commit04 <- headCommit
    pure (commit01, commit04)

  let upstreamUri = fileUri (testCurrentDir env </> "upstream")
      repo commit =
        [ "source-repository-package"
        , "  type: git"
        , "  location: " ++ upstreamUri
        , "  tag: " ++ commit
        , "  subdir: foo"
        ]
      project name ls = writeSourceFile name (unlines ls)

  project "a.project" (["packages: uses-foo"] ++ repo commit01)
  project "a.project.local" ["packages: foo-0.2"]

  project "b.project" (["packages: uses-foo"] ++ repo commit01)
  project "b.project.local" (repo commit04)

  project "c.project" ["packages: uses-foo foo-0.2", "import: c-import.config"]
  project "c-import.config" (repo commit01)

  project "c2.project" (["packages: uses-foo", "import: c2-import.config"] ++ repo commit01)
  project "c2-import.config" ["packages: foo-0.2"]

  project "d.project" ["packages: uses-foo foo-0.2 foo-0.2-copy"]

  project "e.project" ["packages: uses-foo foo-0.2 foo-0.3"]

  project "f.project" (["packages: uses-foo"] ++ repo commit01)
  project "f.project.local" (repo commit01)

  -- The compare parser checks that both parsers tag entries identically.
  forM_ ["legacy", "parsec", "compare"] $ \parser -> do

    let log = recordHeader . pure . (("--project-file-parser=" <> parser <> " ") <>)
        dryRun name = cabal' "v2-build" ["all", "--dry-run", "--project-file=" <> name, "--project-file-parser=" <> parser]

    log "checking that packages in cabal.project.local replaces a source-repository-package in cabal.project"
    a <- dryRun "a.project"
    assertOutputContains "foo-0.2" a
    assertOutputContains "cabal project has multiple sources for foo" a
    assertOutputContains "using" a
    assertOutputContains "ignoring" a

    log "checking that a source-repository-package in cabal.project.local replaces one in cabal.project"
    b <- dryRun "b.project"
    assertOutputContains "foo-0.4" b

    log "checking that a source in the root replaces one in an import"
    c <- dryRun "c.project"
    assertOutputContains "foo-0.2" c

    log "checking that a source in an import does not replace one in the root"
    c2 <- dryRun "c2.project"
    assertOutputContains "foo-0.1" c2

    log "checking that two directories for the same package at the same position conflict"
    d <- fails $ dryRun "d.project"
    assertOutputContains "cabal project has different sources for foo at the same position" d

    log "checking that two versions of a package at the same position conflict"
    e <- fails $ dryRun "e.project"
    assertOutputContains "cabal project has different sources for foo at the same position" e

    log "checking that the same source-repository-package in both files is one source"
    f <- dryRun "f.project"
    assertOutputContains "foo-0.1" f
    assertOutputDoesNotContain "cabal project has multiple sources for foo" f
  where
    headCommit = do
      result <- git' "rev-parse" ["HEAD"]
      case lines (resultOutput result) of
        (commit : _) -> pure commit
        [] -> error "git rev-parse HEAD produced no output"

    fooCabal version =
      unlines
        [ "cabal-version: 2.2"
        , "name:          foo"
        , "version:       " ++ version
        , "build-type:    Simple"
        , ""
        , "library"
        , "  build-depends:    base"
        , "  default-language: Haskell2010"
        ]

    -- Git ignores --depth when cloning from a plain local path, so the location
    -- has to be a URI for the checkouts to be shallow in the first place.
    fileUri path = "file://" ++ root ++ map toPosixSeparator path
      where
        root = case buildOS of
          Windows -> "/"
          _ -> ""

    toPosixSeparator c
      | c == Windows.pathSeparator = Posix.pathSeparator
      | otherwise = c
