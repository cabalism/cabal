import Test.Cabal.Prelude

import Control.Monad (forM_)

-- +-- stackage.config (hashable ==1.4.3.0, hashable +random-initial-seed)
-- +-- hop.config
--  +-- stackage.config
-- +-- constraints.config (hashable ==1.4.2.0)
main = cabalTest . withRepo "repo" . recordMode RecordMarked $ do
  forM_ ["legacy", "parsec"] $ \parser -> do
    let log = recordHeader . pure . (("--project-file-parser=" <> parser <> " ") <>)
        dryRun project = ["--dry-run", "--project-file=" <> project, "--project-file-parser=" <> parser]

    log "checking that imported constraints conflict without hiding"
    noHide <- fails $ cabal' "v2-build" (dryRun "0-no-hide.project")
    assertOutputContains "Could not resolve dependencies" noHide

    log "checking that hide-constraints hides constraints of an import"
    hide <- cabal' "v2-build" (dryRun "1-hide.project")
    assertOutputContains "hashable-1.4.2.0" hide

    log "checking that hide-constraints hides constraints of imports of an import"
    hideHop <- cabal' "v2-build" (dryRun "2-hide-hop.project")
    assertOutputContains "hashable-1.4.2.0" hideHop

    log "checking that hide-constraints keeps constraints of a sibling import"
    hideSibling <- cabal' "v2-build" (dryRun "3-hide-sibling.project")
    assertOutputContains "hashable-1.4.2.0" hideSibling

    log "checking that hide-constraints doesn't hide constraints of a sibling import"
    hideWrongSibling <- cabal' "v2-build" (dryRun "3-hide-wrong-sibling.project")
    assertOutputContains "hashable-1.4.3.0" hideWrongSibling

    log "checking that hide-constraints warns about packages without constraints to hide"
    hideUnused <- cabal' "v2-build" (dryRun "4-hide-unused.project")
    assertOutputContains "hide-constraints found no constraints to hide for text" hideUnused
    assertOutputContains "hashable-1.4.2.0" hideUnused

    log "checking that an unknown import modifier is a parse error"
    badModifier <- fails $ cabal' "v2-build" (dryRun "5-bad-modifier.project")
    assertOutputContains "unknown import modifier \"hide-constraint\"" badModifier
