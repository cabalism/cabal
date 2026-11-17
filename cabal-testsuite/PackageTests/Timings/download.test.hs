import Test.Cabal.Prelude

-- A package downloaded from a remote repository prints the time of its
-- download and of its unpack before its configure. The local repository of
-- cabal.test.hs has nothing to download, so this one is served over http.
main = do
  skipIfWindows "Mysteriously hangs in CI"
  cabalTest $ recordMode DoNotRecord $ withRemoteRepo "repo" $ do
    cabal "update" []
    cold <- cabal' "build" ["timed", "--timings"]
    assertOutputContains "[timing] pkg:download remote-0.1" cold
    assertOutputContains "[timing] pkg:unpack remote-0.1" cold
    assertOutputContains "[timing] pkg:configure remote-0.1" cold
