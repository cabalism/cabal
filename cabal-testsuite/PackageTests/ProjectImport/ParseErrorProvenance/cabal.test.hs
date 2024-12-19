import Test.Cabal.Prelude

main = cabalTest . recordMode RecordMarked $ do
  let log = recordHeader . pure

  let expectMulti =
        "When using configuration from:\n\
        \  - else.project\n\
        \  - dir-else/else.config\n\
        \The following errors occurred:\n\
        \  - The package location 'no-pkg-here' does not exist."

  outElse <- fails $ cabal' "v2-build" [ "all", "--dry-run", "--project-file=else.project" ]

  let expectSingle = filter (/= '\n') expectMulti

  log "Multiline string marking:"
  mapM_ log (lines . decodeLfMarkLines $ encodeLf expectMulti)

  log "Pseudo multiline string marking:"
  mapM_ log (lines . decodeLfMarkLines $ encodeLf expectSingle)

  assertOn multilineNeedleHaystack expectMulti outElse
  assertOn multilineNeedleHaystack{expectNeedleInHaystack = False} expectSingle outElse

  assertOutputDoesNotContain expectMulti outElse
  assertOutputDoesNotContain expectSingle outElse

  return ()
