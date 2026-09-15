import Test.Cabal.Plan
import Test.Cabal.Prelude

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import Data.Maybe (mapMaybe)
import Distribution.Types.PackageName (PackageName, mkPackageName)
import System.Directory (copyFile)

-- Revision pins against a secure repository with two revisions of foo-1.0.
main = do
  skipIfWindows "Mysteriously hangs in CI"
  cabalTest $ recordMode DoNotRecord $ flakyIfCI 9530 $ withRemoteRepo "repo" $ do
    env <- getTestEnv
    let repoDir = testRepoDir env
        cwd = testCurrentDir env

    cabal "v2-update" []
    assertRevisionOfFoo 0

    -- Publish revision 1 of foo-1.0: 'hackage-repo-tool update' appends to
    -- the index every file in the index directory that is newer than the
    -- index tarball.
    liftIO $ threadDelay 1000000
    liftIO $ copyFile (cwd </> "rev1" </> "foo.cabal") (repoDir </> "index" </> "foo" </> "1.0" </> "foo.cabal")
    hackageRepoTool "update" ["--keys", repoDir </> "keys", "--repo", repoDir]
    cabal "v2-update" []

    -- The latest revision is used by default; pins select a revision by
    -- number or by hash.
    assertRevisionOfFoo 1
    withProjectFile "rev0.project" $ assertRevisionOfFoo 0
    withProjectFile "sha0.project" $ assertRevisionOfFoo 0
    withProjectFile "rev1.project" $ assertRevisionOfFoo 1

    -- A pin for a revision the repository does not have is an error.
    r <- withProjectFile "rev2.project" $ fails $ cabal' "v2-build" ["--dry-run"]
    assertOutputContains "has no revision matching the pin 'rev:2'" r

    -- Freezing records the revision of foo-1.0 in its version constraint
    -- when it is a revision.
    cabal "v2-freeze" []
    assertFileDoesContain (cwd </> "cabal.project.freeze") "any.foo ==1.0@rev:1"
    assertFileDoesNotContain (cwd </> "cabal.project.freeze") "revisions"
    withProjectFile "rev0.project" $ do
      cabal "v2-freeze" []
      assertFileDoesContain (cwd </> "rev0.project.freeze") "any.foo ==1.0,"
      assertFileDoesNotContain (cwd </> "rev0.project.freeze") "@rev"
      assertFileDoesNotContain (cwd </> "rev0.project.freeze") "revisions"
  where
    assertRevisionOfFoo :: Int -> TestM ()
    assertRevisionOfFoo expected = do
      cabal "v2-build" ["--dry-run"]
      withPlan $ do
        Just plan <- testPlan `fmap` getTestEnv
        let [rev] = mapMaybe (revisionOf $ mkPackageName "foo") (planInstallPlan plan)
        assertEqual "revision of package foo" rev (Revision expected)

    revisionOf :: PackageName -> InstallItem -> Maybe Revision
    revisionOf pkgName (AConfiguredGlobal configuredGlobal)
      | configuredGlobalPackageName configuredGlobal == pkgName =
          Just $ configuredGlobalRevision configuredGlobal
    revisionOf _ _ = Nothing
