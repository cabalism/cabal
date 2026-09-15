import Test.Cabal.Prelude

-- Revision pins against a file+noindex repository, which has a single
-- .cabal file per package version: foo-1.0 is revision 1, bar-1.0 is the
-- original upload.
main = cabalTest $ withRepo "repo" $ do
  -- Without pins the .cabal file the repository has is used.
  cabal "v2-build" ["--dry-run"]

  -- Pins matching the revision the repository has, by number and by hash.
  withProjectFile "rev1.project" $ cabal "v2-build" ["--dry-run"]
  withProjectFile "sha.project" $ cabal "v2-build" ["--dry-run"]

  -- A pin for a package version that is not chosen has no effect.
  withProjectFile "other.project" $ cabal "v2-build" ["--dry-run"]

  -- A pin for a revision the repository does not have is an error.
  r <- withProjectFile "rev2.project" $ fails $ cabal' "v2-build" ["--dry-run"]
  assertOutputContains "has no revision matching the pin 'rev:2'" r
  assertOutputContains "rev:1" r

  -- Pinning a package version to two different revisions is an error.
  r' <- withProjectFile "conflict.project" $ fails $ cabal' "v2-build" ["--dry-run"]
  assertOutputContains "conflicting revisions" r'
