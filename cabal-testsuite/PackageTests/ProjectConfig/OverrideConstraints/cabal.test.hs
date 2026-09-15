import Test.Cabal.Prelude

import Control.Monad (forM_)

-- stackage.config pins any.hashable ==1.4.3.0 and +random-initial-seed.
-- override-1.4.2.0.config and override-1.4.3.0.config each hold one override.
main = cabalTest . withRepo "repo" . recordMode RecordMarked $ do
  forM_ ["legacy", "parsec"] $ \parser -> do
    let log = recordHeader . pure . (("--project-file-parser=" <> parser <> " ") <>)
        dryRun project = ["--dry-run", "--project-file=" <> project, "--project-file-parser=" <> parser]

    log "checking that a plain constraint conflicts with an imported pin"
    noOverride <- fails $ cabal' "v2-build" (dryRun "0-no-override.project")
    assertOutputContains "Could not resolve dependencies" noOverride

    log "checking that an override in the root replaces an imported pin"
    override <- cabal' "v2-build" (dryRun "1-override.project")
    assertOutputContains "hashable-1.4.2.0" override

    log "checking that an override in a sibling import replaces the pin"
    sibling <- cabal' "v2-build" (dryRun "2-override-sibling.project")
    assertOutputContains "hashable-1.4.2.0" sibling

    log "checking that an override in an import cannot replace a root constraint"
    root <- fails $ cabal' "v2-build" (dryRun "3-import-cannot-override-root.project")
    assertOutputContains "Could not resolve dependencies" root

    log "checking that two different overrides at the same position conflict"
    conflict <- fails $ cabal' "v2-build" (dryRun "4-conflict.project")
    assertOutputContains "conflicting override-constraints at the same position" conflict

    log "checking that a root override silences a conflict between imports"
    silenced <- cabal' "v2-build" (dryRun "5-root-silences-conflict.project")
    assertOutputContains "hashable-1.4.2.0" silenced

    log "checking that an override narrower than the pin is an error"
    narrow <- fails $ cabal' "v2-build" (dryRun "6-narrow.project")
    assertOutputContains "narrower than a constraint it would replace" narrow

    log "checking that cabal.project.local overrides an import of cabal.project"
    local <- cabal' "v2-build" (dryRun "7-local.project")
    assertOutputContains "hashable-1.4.2.0" local

    log "checking that the command line overrides an imported pin"
    cli <- cabal' "v2-build" (dryRun "7-local.project" ++ ["--override-constraint=any.hashable ==1.4.3.0"])
    assertOutputContains "hashable-1.4.3.0" cli

    log "checking that an override that replaces nothing is a warning"
    unused <- cabal' "v2-build" (dryRun "8-unused.project")
    assertOutputContains "override-constraints replaced nothing for" unused
    assertOutputContains "hashable-1.4.2.0" unused
