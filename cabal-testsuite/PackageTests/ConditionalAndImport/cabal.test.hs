import Test.Cabal.Prelude

main = cabalTest . withRepo "repo" . recordMode RecordMarked $ do
  let log = recordHeader . pure

  cabal "v2-run" [ "some-exe" ]

  -- +-- cyclical-0-self.project (imports cyclical-0-self.project)
  -- +-- cyclical-0-self.project (already processed)
  -- +-- etc
  log "checking cyclical loopback of a project importing itself"
  cyclical0 <- fails $ cabal' "v2-build" [ "--project-file=cyclical-0-self.project" ]
  assertOutputContains "cyclical import of cyclical-0-self.project" cyclical0

  -- +-- cyclical-1-out-back.project
  --  +-- cyclical-1-out-back.config (imports cyclical-1-out-back.project)
  -- +-- cyclical-1-out-back.project (already processed)
  --  +-- etc
  log "checking cyclical with hops; out and back"
  cyclical1a <- fails $ cabal' "v2-build" [ "--project-file=cyclical-1-out-back.project" ]
  assertOutputContains "cyclical import of cyclical-1-out-back.project" cyclical1a

  -- +-- cyclical-1-out-self.project
  --  +-- cyclical-1-out-self.config (imports cyclical-1-out-self.config)
  --  +-- cyclical-1-out-self.config (already processed)
  --  +-- etc
  log "checking cyclical with hops; out to a config that imports itself"
  cyclical1b <- fails $ cabal' "v2-build" [ "--project-file=cyclical-1-out-self.project" ]
  assertOutputContains "cyclical import of cyclical-1-out-self.config" cyclical1b

  -- +-- cyclical-2-out-out-backback.project
  --  +-- cyclical-2-out-out-backback-a.config
  --   +-- cyclical-2-out-out-backback-b.config (imports cyclical-2-out-out-backback.project)
  -- +-- cyclical-2-out-out-backback.project (already processed)
  --  +-- etc
  log "checking cyclical with hops; out, out, twice back"
  cyclical2a <- fails $ cabal' "v2-build" [ "--project-file=cyclical-2-out-out-backback.project" ]
  assertOutputContains "cyclical import of cyclical-2-out-out-backback.project" cyclical2a

  -- +-- cyclical-2-out-out-back.project
  --  +-- cyclical-2-out-out-back-a.config
  --   +-- cyclical-2-out-out-back-b.config (imports cyclical-2-out-out-back-a.config)
  --  +-- cyclical-2-out-out-back-a.config (already processed)
  --   +-- etc
  log "checking cyclical with hops; out, out, once back"
  cyclical2b <- fails $ cabal' "v2-build" [ "--project-file=cyclical-2-out-out-back.project" ]
  assertOutputContains "cyclical import of cyclical-2-out-out-back-a.config" cyclical2b

  -- +-- cyclical-2-out-out-self.project
  --  +-- cyclical-2-out-out-self-a.config
  --   +-- cyclical-2-out-out-self-b.config (imports cyclical-2-out-out-self-b.config)
  --   +-- cyclical-2-out-out-self-b.config (already processed)
  --   +-- etc
  log "checking cyclical with hops; out, out to a config that imports itself"
  cyclical2c <- fails $ cabal' "v2-build" [ "--project-file=cyclical-2-out-out-self.project" ]
  assertOutputContains "cyclical import of cyclical-2-out-out-self-b.config" cyclical2c

  -- +-- noncyclical-same-filename-a.project
  --  +-- noncyclical-same-filename-a.config
  --    +-- same-filename/noncyclical-same-filename-a.config (no further imports so not cyclical)
  log "checking that cyclical check doesn't false-positive on same file names in different folders; hoping within a folder and then into a subfolder"
  cyclical3a <- cabal' "v2-build" [ "--project-file=noncyclical-same-filename-a.project" ]
  assertOutputDoesNotContain "cyclical import of" cyclical3a

  -- +-- noncyclical-same-filename-b.project
  --  +-- same-filename/noncyclical-same-filename-b.config
  --    +-- noncyclical-same-filename-b.config (no further imports so not cyclical)
  log "checking that cyclical check doesn't false-positive on same file names in different folders; hoping into a subfolder and then back out again"
  cyclical3b <- fails $ cabal' "v2-build" [ "--project-file=noncyclical-same-filename-b.project" ]
  assertOutputDoesNotContain "cyclical import of" cyclical3b

  -- +-- cyclical-same-filename-out-out-self.project
  --  +-- cyclical-same-filename-out-out-self.config
  --    +-- same-filename/cyclical-same-filename-out-out-self.config
  --    +-- same-filename/cyclical-same-filename-out-out-self.config (already processed)
  --    +-- etc
  log "checking that cyclical check catches a same file name that imports itself"
  cyclical4a <- fails $ cabal' "v2-build" [ "--project-file=cyclical-same-filename-out-out-self.project" ]
  assertOutputContains "cyclical import of cyclical-same-filename-out-out-self.config" cyclical4a

  -- +-- cyclical-same-filename-out-out-backback.project
  --  +-- cyclical-same-filename-out-out-backback.config
  --    +-- same-filename/cyclical-same-filename-out-out-backback.config
  -- +-- cyclical-same-filename-out-out-backback.project (already processed)
  -- +-- etc
  log "checking that cyclical check catches importing its importer (with the same file name)"
  cyclical4b <- fails $ cabal' "v2-build" [ "--project-file=cyclical-same-filename-out-out-backback.project" ]
  -- It should fail with "cyclical import of
  -- cyclical-same-filename-out-out-backback.project" but instead there's a
  -- problem with importing so it fails with:
  assertOutputContains "./same-filename/cyclical-same-filename-out-out-backback.project: withBinaryFile: does not exist (No such file or directory)" cyclical4b

  -- +-- cyclical-same-filename-out-out-back.project
  --  +-- cyclical-same-filename-out-out-back.config
  --    +-- same-filename/cyclical-same-filename-out-out-back.config
  --  +-- cyclical-same-filename-out-out-back.config (already processed)
  --  +-- etc
  log "checking that cyclical check catches importing its importer's importer (hopping over same file names)"
  cyclical4c <- fails $ cabal' "v2-build" [ "--project-file=cyclical-same-filename-out-out-back.project" ]
  -- It should fail with "cyclical import of
  -- cyclical-same-filename-out-out-backback.config" but instead there's a
  -- problem with importing so it fails with:
  assertOutputContains "./../cyclical-same-filename-out-out-back.config: withBinaryFile: does not exist (No such file or directory)" cyclical4c

  log "checking bad conditional"
  badIf <- fails $ cabal' "v2-build" [ "--project-file=bad-conditional.project" ]
  assertOutputContains "Cannot set compiler in a conditional clause of a cabal project file" badIf

  return ()
