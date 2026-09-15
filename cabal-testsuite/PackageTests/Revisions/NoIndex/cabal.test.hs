import Test.Cabal.Prelude

-- Revision pins against a file+noindex repository, which has a single
-- .cabal file per package version: foo-1.0 is revision 1, bar-1.0 is the
-- original upload.
main = cabalTest $ withRepo "repo" $ do
  -- Without pins the .cabal file the repository has is used.
  cabal "v2-build" ["--dry-run"]

  -- Pins matching the revision the repository has, by number and by hash,
  -- in the 'revisions' field, in a version constraint and on the command line.
  withProjectFile "rev1.project" $ cabal "v2-build" ["--dry-run"]
  withProjectFile "sha.project" $ cabal "v2-build" ["--dry-run"]
  withProjectFile "constraint.project" $ cabal "v2-build" ["--dry-run"]
  cabal "v2-build" ["--dry-run", "--constraint=any.foo ==1.0@rev:1"]

  -- A pin for a package version that is not chosen has no effect.
  withProjectFile "other.project" $ cabal "v2-build" ["--dry-run"]

  -- A pin for a revision the repository does not have is an error.
  r <- withProjectFile "rev2.project" $ fails $ cabal' "v2-build" ["--dry-run"]
  assertOutputContains "has no revision matching the pin 'rev:2'" r
  assertOutputContains "rev:1" r
  r2 <- fails $ cabal' "v2-build" ["--dry-run", "--constraint=foo ==1.0@rev:2"]
  assertOutputContains "has no revision matching the pin 'rev:2'" r2

  -- Pinning a package version to two different revisions is an error,
  -- whether in the 'revisions' field or between it and a constraint.
  r' <- withProjectFile "conflict.project" $ fails $ cabal' "v2-build" ["--dry-run"]
  assertOutputContains "is pinned to revision 'rev:1' by the 'revisions' field (project config conflict.project) and to revision 'rev:0' by the 'revisions' field" r'
  r'' <- withProjectFile "mixed.project" $ fails $ cabal' "v2-build" ["--dry-run"]
  assertOutputContains "by the 'revisions' field (project config mixed.project) and to revision 'rev:0' by the constraint 'any.foo ==1.0@rev:0' (project config mixed.project)" r''

  -- A revision pin needs an exact version and an unqualified or any. scope.
  r3 <- fails $ cabal' "v2-build" ["--dry-run", "--constraint=foo >=1.0@rev:1"]
  assertOutputContains "only be pinned together with an exact version" r3
  r4 <- fails $ cabal' "v2-build" ["--dry-run", "--constraint=setup.foo ==1.0@rev:1"]
  assertOutputContains "cannot be scoped" r4
