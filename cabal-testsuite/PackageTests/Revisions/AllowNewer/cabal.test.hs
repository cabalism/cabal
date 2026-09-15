import Test.Cabal.Plan
import Test.Cabal.Prelude

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import Data.Maybe (mapMaybe)
import Distribution.Types.PackageName (PackageName, mkPackageName)
import System.Directory (copyFile)

-- Revision pins and allow-newer: revision 1 of foo-1.0 restricts its
-- dependency on base to '<4', which no plan can satisfy. Either the original
-- upload is pinned, or the restriction is relaxed with allow-newer; the
-- solver sees the bounds of the revision in use.
main = do
  skipIfWindows "Mysteriously hangs in CI"
  cabalTest $ recordMode DoNotRecord $ flakyIfCI 9530 $ withRemoteRepo "repo" $ do
    env <- getTestEnv
    let repoDir = testRepoDir env
        cwd = testCurrentDir env

    cabal "v2-update" []
    assertRevisionOfFoo 0

    -- Publish revision 1 of foo-1.0.
    liftIO $ threadDelay 1000000
    liftIO $ copyFile (cwd </> "rev1" </> "foo.cabal") (repoDir </> "index" </> "foo" </> "1.0" </> "foo.cabal")
    hackageRepoTool "update" ["--keys", repoDir </> "keys", "--repo", repoDir]
    cabal "v2-update" []

    -- The latest revision is used by default and its bounds cannot be met.
    r <- fails $ cabal' "v2-build" ["--dry-run"]
    assertOutputContains "Could not resolve dependencies" r

    -- Pinning the original upload restores its bounds.
    withProjectFile "rev0.project" $ assertRevisionOfFoo 0

    -- allow-newer relaxes the bounds of the revision in use.
    withProjectFile "allow-newer.project" $ assertRevisionOfFoo 1
    withProjectFile "both.project" $ assertRevisionOfFoo 0
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
