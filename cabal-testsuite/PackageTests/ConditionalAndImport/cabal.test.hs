import Test.Cabal.Prelude

main = cabalTest . withRepo "repo" . recordMode RecordMarked $ do
  let log = recordHeader . pure

  cabal "v2-run" [ "some-exe" ]

  log "checking cyclical loopback"
  cyclical0 <- fails $ cabal' "v2-build" [ "--project-file=cabal-cyclical-loopback.project" ]
  assertOutputContains "cyclical import of" cyclical0

  log "checking cyclical with 1 hop"
  cyclical1 <- fails $ cabal' "v2-build" [ "--project-file=cabal-cyclical-1-hop.project" ]
  assertOutputContains "cyclical import of" cyclical1

  log "checking cyclical with 2 hops"
  cyclical2 <- fails $ cabal' "v2-build" [ "--project-file=cabal-cyclical-2-hop.project" ]
  assertOutputContains "cyclical import of" cyclical2

  log "checking bad conditional"
  badIf <- fails $ cabal' "v2-build" [ "--project-file=cabal-bad-conditional.project" ]
  assertOutputContains "Cannot set compiler in a conditional clause of a cabal project file" badIf

  return ()
