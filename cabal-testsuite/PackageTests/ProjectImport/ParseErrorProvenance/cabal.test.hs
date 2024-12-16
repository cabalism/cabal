import Test.Cabal.Prelude
import System.Directory

main = cabalTest . recordMode RecordMarked $ do
  let log = recordHeader . pure

  outElse <- fails $ cabal' "v2-build" [ "all", "--dry-run", "--project-file=else.project" ]

  msg <- readFileVerbatim "msg.expect.txt"
  let msgSingle = lineBreaksToSpaces msg

  log "Multiline string marking:"
  mapM_ log (lines . delimitLines $ encodeLf msg)

  log "Pseudo multiline string marking:"
  mapM_ log (lines . delimitLines $ encodeLf msgSingle)

  assertOn multilineNeedleHaystack msg outElse
  assertOn multilineNeedleHaystack{expectNeedleInHaystack = False} msgSingle outElse

  assertOutputContains msg outElse
  assertOutputDoesNotContain msgSingle outElse

  outIf <- fails $ cabal' "v2-build" [ "all", "--dry-run", "--project-file=if.project" ]
  assertOutputContains "Error parsing project file dir-if/if.config:3" outIf

  outElif <- fails $ cabal' "v2-build" [ "all", "--dry-run", "--project-file=elif.project" ]
  assertOutputContains "Error parsing project file dir-elif/elif.config:4" outElif

  outElse <- fails $ cabal' "v2-build" [ "all", "--dry-run", "--project-file=else.project" ]
  assertOutputContains "Warnings found while parsing the project file, else.project:" outElse
  assertOutputContains "- dir-else/else.config: Unrecognized section '_' on line 3" outElse

  return ()
