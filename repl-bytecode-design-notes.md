# Design notes: `cabal repl` and `--enable-library-bytecode` (#11981)

These notes explain the design chosen for the fix and the alternatives that were considered, so that the trade-offs are on the table for review. The branch is `fix/repl-library-bytecode`.

## The problem

With `--enable-library-bytecode`, `cabal build` asks GHC for `-fbyte-code-and-object-code`, which writes a `.gbc` file per module next to the object files, and links them into a `.bytecodelib`. `cabal repl` did not benefit: every start reinterpreted the component from source, and `.gbc` files written by `build` were ignored.

Three GHC facts, verified in GHC's source and by experiment with GHC 10.0.0.20260917, decide what Cabal has to do:

1. GHCi's internal interpreter compiles with the ways the compiler itself was built with (`makeDynFlagsConsistent` sets the target ways to the host ways), whatever `-static` or `-dynamic` says on the command line. It refuses an interface file whose way tag (`""`, `dyn`, `p`, `p_dyn`) differs from those ways, and a rejected interface means "must compile" before the `.gbc` file is ever consulted. Cabal passed the static way's options to GHCi, so on a dynamically linked GHC (ghcup on Linux, macOS) it named `Mod.hi` with tag `""` and GHCi threw it away.
2. A `.gbc` file is reused only when the interface next to it is up to date and the `.gbc` is no older than it, and only with `-fwrite-byte-code` on. Writing one needs `-fwrite-byte-code`; a repl-only session also needs `-fwrite-interface`, or there is no interface to validate against next time. `-fprefer-byte-code` plays no part in this: it only makes GHC pick installed `.bytecodelib`s over native libraries for dependencies.
3. GHC's recompilation fingerprint records whether `__PIC__` is defined for the preprocessor, for every module, not only those using CPP. A dynamically linked GHCi always has PIC on (it comes with the dynamic way it forces), while a `-dynamic-too` build does not pass `-fPIC`, so even with the right way tag the interface is rejected as "flags changed". Diagnosed with `--ghc-options=-fwrite-if-self-recomp-flags` on the build and `--repl-options=-ddump-hi-diffs` on the repl.

## The chosen design

All in the `Cabal` library; `cabal-install` needs no change because `withBytecodeLib` already reaches the repl's configure step and is part of the package hash. The change has two parts of different standing, and they are best reviewed as such.

### Part 1, the fix proper: choose the repl's base way from the compiler

`GHC/Build/Link.hs`, repl branch: when `withBytecodeLib` is set, base the GHCi options on `compilerBuildWay` (read from `ghc --info`'s "GHC Dynamic" and "GHC Profiled", which are exactly the ways GHCi forces) instead of always the static way; pass `-fbyte-code -fwrite-interface -fwrite-byte-code` (`GhcByteCode`); pass no `-fPIC`. Without the flag the invocation is byte-for-byte what it was.

This is not a workaround. GHCi compiles with the compiler's own ways whatever Cabal passes, so any reuse of files written by a repl has to name files with that way's tag. The static base has only ever appeared to work because GHCi discarded the interfaces and started from source. Keying the new base on `--enable-library-bytecode` is deliberate conservatism so that nobody's repl changes unless they asked for bytecode; the price is two repl behaviours in the code. Making it unconditional is alternative A below and is proposed as a follow-up.

Alongside it, `GHC/Build/Modules.hs` makes build-time bytecode objects library-only, which fixes a spurious "standalone bytecode" error for profiled-only executables (alternative I).

### Part 2, the bridge: `-fPIC` in the build's `-dynamic-too` pass

`GHC/Build/Modules.hs`: when bytecode is on, the `-dynamic-too` pass passes `-fPIC`, so the interface fingerprint written by `build` matches what a dynamically linked GHCi computes (fact 3). This is a workaround for a GHC behaviour and should be understood as temporary. The rule belongs in GHC: GHCi already ignores optimisation-level changes in the fingerprint (`Opt_IgnoreOptimChanges`) and could ignore the PIC preprocessor defines the same way. A GHC issue should be filed with the PR, and this line can be removed once GHC changes. If reviewers do not want a repl concern to touch native code generation, alternative B (a separate repl output directory) is the clean retreat and costs only the build-to-repl case.

Result on a dynamic GHC: build then repl, repl then repl, and build after repl all reuse the files; only a source change recompiles, on whichever side sees it first.

Measured on this repository with GHC 10 (second run of each): `lib:cabal-install` single repl 8.9 s with the 3.18.2.0 release against 2.3 s with the fix; multi-repl of the four libraries 15 s against 2.5 to 6 s.

## Risks

None of these showed up in testing, but they are where a regression would come from.

- With the flag on, the static objects of libraries are now PIC. On x86_64 Linux that is a small code-size and speed cost on the native library, which only matters if binaries are shipped from a bytecode-enabled build. On macOS and aarch64 PIC is already the default, and on Windows the `-dynamic-too` pass does not run.
- The repl now writes interface files into the build directory. Editing a module in the repl and then building recompiles that module and its dependents once. Not new in kind, since `--repl-options=-fwrite-interface` already did this, but new by default under the flag.
- With the flag on, the repl picks up `ghc-shared-options` rather than `ghc-static-options`. Few packages set either.
- HLS drives `cabal repl` through the flag-output path. With bytecode on it now receives `-dynamic`, `-fwrite-interface` and `-fwrite-byte-code`. Not tested against HLS.
- A profiled GHC host (`GHC Profiled: YES`) is unverified; see the hedge under known limits.
- CI cannot run the new test, because validate stops at GHC 9.14, so a regression here is invisible until a newer GHC joins the matrix. This is the strongest argument for adding a GHC 10 job soon, and it is an argument about testing rather than about the design.

## Alternatives considered

### A. Always base the repl on the compiler's way, flag or no flag

Simpler and arguably more correct: the static base has only ever "worked" because GHCi ignored it, and today `--repl-options=-fwrite-interface` or `-fobject-code` write wrongly tagged files into the static slots. Rejected for this PR because it is a visible behaviour change for everyone on a dynamic GHC: after a `-dynamic-too` build, GHCi would read `Mod.dyn_hi`, find `Mod.dyn_o` up to date, and load the package's own modules as object code, so no breakpoints and no `*Mod` scope, which is what plain `ghci` and static-GHC platforms already do but Linux and macOS users have never seen through Cabal. It would also swap `ghc-static-options` for `ghc-shared-options` in the repl. With bytecode on, `-fwrite-byte-code` forces the bytecode path, so none of this arises. Worth proposing separately.

### B. A dedicated output directory for repl artifacts

Write the repl's `.gbc` and interface files under their own directory, say `build/repl/`, instead of sharing the build's. Repl-to-repl reuse works, nothing the repl writes can disturb `build`, and the native build keeps its exact flags (no `-fPIC` change). Lost: build-to-repl reuse, so the first repl after a build always recompiles, and artifacts are duplicated. Rejected because build-to-repl reuse was the request in #11981 and the proposal, but it is the fallback if reviewers object to touching the native build's flags.

### C. Share the directory but do not add `-fPIC` to the build

The minimal change, only `Link.hs`. Fact 3 above makes it worse than it looks: the first repl after a build recompiles with "[Flags changed]" and rewrites `Mod.dyn_hi` at `-O0`; the next `build` then sees "Mismatched dynamic interface file" and recompiles; the next repl sees flags changed again. Every alternation recompiles. Rejected.

### D. Fix the fingerprint mismatch in GHC instead

GHCi already ignores optimisation-level changes (`Opt_IgnoreOptimChanges`); it could ignore the PIC preprocessor defines the same way, or `-dynamic-too` builds could record PIC in the fingerprint. Then Cabal would not need `-fPIC` in the build. This is the better long-term home for the rule, and a GHC issue is worth filing, but a GHC change does not help users on GHC 10.0, so the Cabal-side `-fPIC` is kept. It can be dropped once GHC changes.

### E. Use `-fbyte-code-and-object-code` in the repl, as the build does

Keeps one object mode everywhere. Rejected: in GHCi that flag implies `-fwrite-byte-code` but not `-fwrite-interface`, so a repl-only session would leave no interface to validate the `.gbc` against; and it makes GHCi generate object code too, slowing the first run for nothing. The experiments showed the object-mode flags do not perturb the fingerprint, so `-fbyte-code -fwrite-interface -fwrite-byte-code` is both sufficient and cheaper.

### F. Pass `-fprefer-byte-code` from the repl automatically

It looks related but is not: it does not affect home-module reuse at all, and it changes which artifacts are loaded for dependencies (installed `.bytecodelib` before native). Several open GHC bugs sit on that path (#26749 mixing object code and bytecode, #27715 foreign exports in TH dependencies). Left to the user via `--repl-options`, as the existing `LibraryBytecodeInstall` test and docs already do.

### G. A separate repl option, on by default, as the proposal sketches

The accepted proposal says "Add a new option to enable the repl command to write bytecode files. This will be enabled by default." That would give every GHC 9.15+ user cached repl starts without asking. Not done here because default-on means every repl writes into the build directory and changes its base way, which deserves its own discussion, and because it needs the same mechanics as this fix underneath. Keying on `--enable-library-bytecode` delivers the mechanics now; an option on top is a small follow-up.

### H. `-fexternal-interpreter`

With the external interpreter GHCi keeps the command-line ways, so the static base would match the static interfaces with no way change in Cabal. Rejected: it changes semantics and performance of the repl for everyone and is not what users run.

### I. What the build does for executables, tests and benchmarks

Before: every component got `-fbyte-code-and-object-code`, leaving `.gbc` files nothing links, and a profiled-only executable died with the "standalone bytecode" error, since its only way is `ProfWay`. Options were to keep that, to link executable bytecode somewhere (no consumer exists; GHC has no bytecode-executable mode), or to make build-time bytecode library-only. Library-only was chosen: it matches the flag's name and the `.bytecodelib` link step, and it fixes the profiled executable error. The repl still writes bytecode for whatever component it loads, which is what makes `cabal repl exe:foo` fast and is the practical answer to #12515 today.

## Known limits and follow-ups

- Multi-repl: repl-to-repl reuse works. Build-to-multi-repl recompiles once, because under `-working-dir` the include paths in the fingerprint are spelled absolute while the build spelled them relative. Either cabal-install passes a relative working directory for multi-repl or GHC normalises include paths in the fingerprint. Separate issue.
- In-place but non-local packages, such as a Hackage package that depends on a local one, are units of the multi-repl but do not receive command-line per-package flags, so they are interpreted from source each time. A `package <name>` stanza with `library-bytecode: True` reaches them. General per-package scoping, not specific to this change.
- A profiled GHC host (`GHC Profiled: YES`) would give `compilerBuildWay = ProfWay`; GHC does not support profiled bytecode, so nothing would be reused there, and the repl base way would be `ProfWay` rather than static. Not verified; profiled GHC installations are rare. Restricting the new base to the static and dynamic ways is a one-line hedge if reviewers want it.
- On a dynamic GHC with `--disable-shared`, the build's companion way is static (`Mod.hi`, tag `""`), so the first repl cannot reuse the build's `.gbc`; the second repl reuses its own.
- CI's validate matrix tops out at GHC 9.14, so the new `LibraryBytecodeRepl` test only runs locally with a GHC that has `.gbc` support.
- Follow-up PRs in order of value: (1) a GHC issue to make GHCi's fingerprint ignore PIC defines, after which Part 2 can be dropped; (2) make the compiler-way base unconditional (alternative A) with its own changelog entry; (3) a repl option on by default (alternative G) once (2) has settled; (4) relative working directories for multi-repl, or a GHC fix, for the include-path spelling.
