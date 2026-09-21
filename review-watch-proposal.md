Thanks for writing this up. I think there is a real gap here, but as written the
proposal is asking for two different features under one flag, and they have very
different costs. Splitting them would make this much easier to evaluate.

- 1. Rerun on change (`cabal build|test|run --watch`): poll the project,
rebuild what changed, rerun. cabal-install already has the change detection this
needs. The plan cache and the per-package build monitors
(`Distribution.Client.FileMonitor`, `ProjectBuilding.PackageFileMonitor`) do
mtime-then-hash checks and the pre-build phase short-circuits when nothing
relevant changed. A loop around the existing pre-build/build/post-build phases
in `ProjectOrchestration` would give a correct `--watch` for every command with
no new dependencies. It pays link time and process start-up per iteration, but
the semantics are exactly those of the command being watched. This part I think
is implementable now and would be a reasonable first proposal on its own.

2. Reload in place (`cabal repl --watch`, "fast like `:r`"): keep a GHCi session
loaded with the library and the test suite, send `:reload` on change,
re-evaluate the test entry point. This is the part the motivation is really
about, and none of it exists in cabal today. `cabal repl` launches GHCi as a
blocking one-shot process and never talks to it again. There is no code for
driving a GHCi child over stdin, detecting a failed load, re-evaluating an
expression, or killing a still-running test thread. That is the entire substance
of ghcid and ghciwatch, including the fragile bits (see ghcid#191 on orphaned
`--test` processes). The GHC MR linked in the proposal (!14440) is a draft proof
of concept from June 2025 with no GHC proposal behind it, and it only
re-evaluates bindings after a reload; something still has to trigger the reload.
I would not make a cabal proposal depend on it.

Two things the proposal does not mention that matter for (2): running a test
suite in GHCi is interpreted code, so for larger suites the per-run cost can go
up even though the reload cost goes down (hence people reaching for
`-fobject-code`); and adding a file-watching library to cabal-install is a real
cost, because the bootstrap plans and the platform backends
(inotify/kqueue/Win32) all have to come along.

## On external commands

I think the "Alternatives" section dismisses this too quickly. The objection
that "it is the test suite that is being run using cabal" does not really apply:
an external `cabal-watch` runs cabal. The genuine cost is discoverability and
installation, and that is a documentation problem.

More importantly, cabal already has the hook that makes an external watcher
cheap to write: `cabal repl --with-repl=PROG` makes cabal do all of its planning
(multi-repl, unit files, package dbs, flags) and then hand the complete GHCi
command line to `PROG` instead of `ghc`. A watcher does not need to reimplement
any of that. It asks cabal for the invocation, spawns GHCi itself, watches the
sources, and drives `:reload` and `:main` (or a named expression) over stdin.
That is ghciwatch's architecture with cabal supplying the arguments.
`--repl-multi-file DIR` writes the same arguments to files without launching
anything, if a wrapper program is not wanted. An external tool can also iterate
quickly on the fragile process-driving logic, which cabal-install's release
cadence cannot.

## What I would suggest

- Narrow this proposal to (1), the generic `--watch` loop using the existing
file monitors.

- Prototype (2) as an external `cabal-watch` on top of `--with-repl`, and feed
what is learned into GHC#26159 (re-evaluation on reload belongs in GHCi, since
GHCi is the thing that knows what was recompiled) and into a later proposal for
upstreaming whatever ends up belonging in cabal.

- Independently of either, add a page to the cabal docs showing the multi-repl
recipe with ghcid and ghciwatch. That addresses the "badly documented" complaint
immediately and costs nothing.

One small correction for the "Implementation Notes": the `--reload` flag on
Cabal's `repl` command is unimplemented plumbing (cabal-install hard-wires it to
empty), not a hook to build on.
