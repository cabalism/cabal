import Test.Cabal.Plan
import Test.Cabal.Prelude

import Data.Maybe (mapMaybe)
import Distribution.Types.PackageName (PackageName, mkPackageName)

-- Revision pins and deprecated versions: foo-1.0 (a revision) is deprecated
-- by the repository's preferred-versions in favour of foo-2.0.
main = cabalTest $ withRepo "repo" $ do
  -- The deprecated version is not chosen.
  cabal' "v2-build" ["--dry-run"] >>= assertChoosesFoo "2.0"

  -- A 'revisions' pin does not affect which version is chosen, so a pin on
  -- the deprecated version has no effect, which is pointed out.
  withProjectFile "revisions.project" $ do
    r <- cabal' "v2-build" ["--dry-run"]
    assertChoosesFoo "2.0" r
    assertOutputContains "The revision pin foo-1.0@rev:1 from the 'revisions' field (project config revisions.project) has no effect: the plan uses foo-2.0 instead." r

  -- A constraint with a pin forces the deprecated version, at that revision.
  withProjectFile "constraint.project" $ do
    cabal' "v2-build" ["--dry-run"] >>= assertChoosesFoo "1.0"
    withPlan $ do
      Just plan <- testPlan `fmap` getTestEnv
      let [rev] = mapMaybe (revisionOf $ mkPackageName "foo") (planInstallPlan plan)
      assertEqual "revision of package foo" rev (Revision 1)

  -- A pin for a revision the repository does not have is an error even when
  -- that version would not be chosen: the pin is checked against the
  -- repository, not against the plan.
  r <- withProjectFile "missing.project" $ fails $ cabal' "v2-build" ["--dry-run"]
  assertOutputContains "has no revision matching the pin 'rev:2'" r
  where
    assertChoosesFoo :: String -> Result -> TestM ()
    assertChoosesFoo ver out = do
      assertOutputContains ("foo-" ++ ver ++ " (lib:foo)") out
      assertOutputDoesNotContain ("foo-" ++ other ver ++ " (lib:foo)") out
    other "1.0" = "2.0"
    other _ = "1.0"

    revisionOf :: PackageName -> InstallItem -> Maybe Revision
    revisionOf pkgName (AConfiguredGlobal configuredGlobal)
      | configuredGlobalPackageName configuredGlobal == pkgName =
          Just $ configuredGlobalRevision configuredGlobal
    revisionOf _ _ = Nothing
