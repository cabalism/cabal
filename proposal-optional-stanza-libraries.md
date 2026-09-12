# Libraries in optional stanzas

## Problem

A package whose test-suites share helper modules has nowhere good to put them.

`cabal-install` has four test-suites sharing `hs-source-dirs: tests`. Four modules
are listed in more than one stanza, so each is compiled once per suite that uses
it: `UnitTests.Options` and `UnitTests.Distribution.Solver.Modular.DSL` three
times each, `...DSL.TestCaseUtils` and `...Client.ArbitraryInstances` twice.

The two obvious remedies both fail.

### A sublibrary drags test-only dependencies into every solve

Moving the shared modules into a sublibrary of `cabal-install` is the natural fit:
the component graph stays acyclic, since the sublibrary depends on
`lib:cabal-install` and the test-suites depend on the sublibrary.

But `tests: False` disables *test-suite components*, and a sublibrary is a library
component. Its `build-depends` are therefore resolved unconditionally. In
`cabal-install`'s case the helpers need `Cabal-QuickCheck`, which is an
unpublished in-repo package that `cabal.bootstrap.project` deliberately excludes,
so every bootstrap solve fails:

```
Error: [Cabal-7107]
Could not resolve dependencies:
[__0] trying: cabal-install-3.19.0.0 (user goal)
[__1] unknown package: Cabal-QuickCheck (dependency of cabal-install)
```

Even where the dependencies do resolve, they are wrong to require: `tasty` and
friends enter a plan for a build that asked for no tests.

### A separate package is a package-level cycle

Moving the helpers into their own package removes them from the `tests: False`
solve, because only test-suites would depend on that package. But the helpers
need `lib:cabal-install`, so the package graph becomes
`cabal-install:test -> cabal-install-testlib -> cabal-install:lib`. The component
graph is still acyclic, but cabal resolves cycles at package granularity:

```
[_41] rejecting: cabal-install:*test (cyclic dependencies; conflict set: cabal-install, cabal-install-testlib)
```

### The workaround, and why it is not good enough

A manual flag guarding `buildable` does work:

```cabal
flag test-helpers
  default: False
  manual: True

library testlib
  if !flag(test-helpers)
    buildable: False
```

`buildable: False` removes the component's dependencies from the solve, so
bootstrap resolves again. But the flag is a hand-mirror of `--enable-tests` that
nothing keeps in sync: every project that builds the test-suites must set
`+test-helpers`, and a plain `cabal build` outside those projects fails with
`Dependency on unbuildable package cabal-install`, which does not hint at the
cause.

## Proposal

Let a sublibrary declare that it belongs to an optional stanza:

```cabal
library testlib
  visibility: private
  stanza: test
  hs-source-dirs: testlib
  build-depends: cabal-install, Cabal-QuickCheck, tasty, tasty-hunit
  exposed-modules: UnitTests.Options, ...
```

A library with `stanza: test` is *requested* exactly when test-suites are, and one
with `stanza: bench` exactly when benchmarks are. `stanza: always` is the default
and the existing behaviour. Only sublibraries may carry the field; the main
library is always requested.

This needs no new concept in the solver, which already models the optional
stanzas as decision variables rather than inputs.

## Why this shape

`cabal-install`'s solver does not receive a `ComponentRequestedSpec`.
`Distribution.Solver.Modular.IndexConversion.convGPD` converts a package's
components with `os`, `arch` and the compiler only, and expresses the optional
stanzas as a dependency constructor:

```haskell
data FlaggedDep qpn =
    Flagged (FN qpn) FInfo (TrueFlaggedDeps qpn) (FalseFlaggedDeps qpn)
  | Stanza  (SN qpn)       (TrueFlaggedDeps qpn)
  | Simple (LDep qpn) Component
```

Test-suite dependencies are already emitted under `prefix (Stanza (SN pn TestStanzas))`.
Placing a stanza-scoped sublibrary's dependencies in the same group is all the
solver change amounts to.

The alternative of adding a condition to the `.cabal` conditional language --
`if tests()`, parallel to `if impl(ghc)` -- does not work as well:

- `ConfVar` has four constructors (`OS`, `Arch`, `PackageFlag`, `Impl`) and is
  shared with `cabal.project` parsing via `Distribution.Fields.ConfVar.parseConditionConfVar`.
  A new constructor becomes syntactically valid in project files, where it is
  meaningless and where unhandled `ConfVar`s currently reach a raw `error` call in
  `Distribution.Client.ProjectConfig.Legacy`.
- The natural spelling is negative (`if !tests() buildable: False`), but `Stanza`
  carries only true-branch dependencies, unlike `Flagged`. Supporting it would mean
  extending the solver's stanza representation with a false branch.

Component scope is also the more honest model: "this component exists for the
tests" is a property of the component, not a condition on its contents.

## Naming

The field is spelled `stanza:` throughout this document and in the prototype, but
that spelling is provisional and probably wrong. It is recorded here so the
question is settled deliberately rather than by inheritance from the prototype.

**`stanza:` collides with an established meaning.** In user-facing documentation
"stanza" already means *a group of fields*, and the project file documentation
defines it that way outright: fields "live inside stanzas (groups of fields that
apply to only part of a project)". The docs speak of the `library` stanza, the
`source-repository-package` stanza, `common` stanzas, "a typical stanza for a
foreign library". Under that reading, `stanza: test` written inside a
`library testlib` stanza says "this group of fields is test", which is not the
intended meaning at all.

The term is also internal. `OptionalStanza` is a solver and `cabal-install` type;
no user-facing document uses the phrase "optional stanza". The corresponding
user-facing vocabulary is the project fields `tests:` and `benchmarks:` and the
flags `--enable-tests` and `--disable-tests`.

That suggests naming the field after what the user already types. Candidates:

| spelling | reads as | notes |
| --- | --- | --- |
| `enabled-by: tests` | "this library is enabled by `tests`" | values match the `tests:`/`benchmarks:` project fields and `--enable-tests` exactly; the word "enabled" is, however, already load-bearing in Cabal, where *enabled* means buildable **and** requested |
| `requested-by: tests` | "requested when `tests` are" | matches Cabal's own terminology precisely: `--enable-tests` is what *requests* a component, per `Distribution.Types.ComponentRequestedSpec` |
| `optional: tests` | "optional, along with `tests`" | short, but says nothing about which stanza without reading the value |
| `test-only: True` | "only for tests" | clearest at a glance, but needs a second field for benchmarks and does not generalise |
| `scope: test` | "scoped to tests" | "scope" is overloaded in Cabal already (dependency scope, visibility) |
| `stanza: test` | -- | collides as described above |

`requested-by: tests` is the recommendation. It reuses the word Cabal's own
documentation uses for exactly this state, it keeps *enabled* free for its
existing meaning, and its values are the ones users already write in
`cabal.project`.

Two sub-questions go with it:

- **Plural values.** `tests` and `benchmarks` match `--enable-tests` and the
  project fields; `test` and `bench` match the internal constructors
  (`TestStanzas`, `BenchStanzas`). The user-facing plural is preferable.
- **The default.** The prototype spells it `always`, which reads oddly against a
  `requested-by:` field. Omitting the field is the default in any case, so the
  explicit form could simply be dropped, or spelled `requested-by: none`.

Renaming is mechanical: the field name appears once in the field grammar, and the
constructor names are internal to `Distribution.Types.LibraryStanza`.

## Specification

A new field on library components, spelled here as `stanza:` but see
[Naming](#naming) -- `requested-by:` is the recommended spelling:

```
stanza: always | test | bench
```

- Default `always`, which is the current behaviour.
- Accepted only on sublibraries. On the main library it is an unrecognised
  field, warned about and ignored -- the same treatment `visibility:` already
  gets there, so this is consistent rather than special.
- Requires `cabal-version: 3.20` or later.
- A library with `stanza: test` is requested iff test-suites are requested, and
  likewise for `bench`. Being *requested* is necessary but not sufficient for being
  *enabled*; `buildable: False` still applies, per the existing distinction in
  `Distribution.Types.ComponentRequestedSpec`.

### Validation

A component in an optional stanza may only be depended upon by components in the
same stanza, or by other components of the same package that are themselves in
that stanza. In particular the main library may not depend on a `stanza: test`
sublibrary.

Without this rule a package could reintroduce the original problem by having
`lib` depend on a test-scoped sublibrary, and the solver would be left with a
dependency on a component that is not requested. With it, the dependencies of a
stanza-scoped library are needed only when something in that stanza is, which is
what makes gating the dependencies alone sufficient.

This rule is worth stating on its own merits: it is what the flag workaround
cannot express.

It is implemented as a package check reporting `PackageBuildImpossible`, which
`Distribution.Simple.Configure.checkPackageProblems` turns into an error, so the
package cannot be configured. The check walks every branch of each component's
condition tree, since a violation hidden behind a flag is still a violation, and
reads each sublibrary's stanza from its tree root. Reported as
`cross-stanza-dependency`:

```
The package will not build sanely due to these errors:
Error: [cross-stanza-dependency] The executable 'cabal' depends on the library
'testlib', which is in the test stanza. A library in an optional stanza is only
requested when that stanza is, so anything outside the stanza that depends on it
would be left with a missing dependency whenever the stanza is disabled. Either
move the dependency into the test stanza too, or take the library out of it.
```

Three further rules accompany it.

**A stanza-scoped library may not be `public`** (`public-stanza-library`, also
`PackageBuildImpossible`). Whether a stanza is requested is part of *this*
package's configuration, so a package depending on this one has no way to ask for
it; such a library could never be satisfied from outside.

**The `stanza` field may not be set inside a conditional**
(`conditional-stanza`, `PackageBuildImpossible`). A component's stanza has to be
known before conditions are resolved -- the solver reads it from the condition
tree's root -- so a conditional setting would be quietly ignored. Rejecting it is
better than silently taking the root value.

**A stanza-scoped library that nothing in its stanza depends on is warned about**
(`unused-stanza-library`, `PackageBuildWarning`, so not fatal). It would never be
requested and so never built. A library in the same stanza counts as a user, not
just a test-suite or benchmark.

Reachability is transitive. A stanza-scoped library is live only if a test-suite
or benchmark reaches it, directly or through other libraries in the same stanza,
so a dead chain is reported in full rather than one link per run. Verified on a
two-link chain: with the test-suite depending on neither, both libraries are
reported; with the chain live, neither is; and when only the tail is orphaned,
only the tail is reported. The closure keeps a visited set, so a dependency cycle
among libraries terminates rather than looping -- such a package is rejected
elsewhere as a component cycle, but the check must not hang before it gets there.

Two notes on the cross-stanza rule as implemented:

- A component's own stanza is what it may depend on: a test-suite may depend on a
  `stanza: test` library, a benchmark on a `stanza: bench` one, and any component
  on an ordinary library. Anything else is rejected.
- **A stanza-scoped library may depend on another in the same stanza.** A
  library's own stanza is what it is judged by, so `stanza: test` depending on
  `stanza: test` is fine, as is any stanza-scoped library depending on an
  ordinary one. Verified end to end: a chain of two `stanza: test` libraries
  under a test-suite builds and runs with tests enabled, and with
  `--disable-tests` the whole chain is reported as unavailable together:

  ```
  Cannot build the package two-0.1 because none of the components are available
  to build: the test suite 't', the library 'helper-b' and the library 'helper-a'
  are not available because building test suites has been disabled in the
  configuration
  ```
- Making the *main library* depend on a `stanza: test` sublibrary is already
  rejected for a different reason -- the sublibrary depends on the main library,
  so it is a component cycle -- and `cabal` reports it as such. The check is what
  catches the cases that are not cycles, such as an executable depending on it.

## Interactions

**Explicit targets.** `cabal build pkg:lib:testlib` with tests disabled should
behave as `cabal build pkg:test:foo` does today, which is to fail with a specific
explanation rather than silently enabling the stanza:

```
Error: [Cabal-7127]
Cannot build the test suite 'unit-tests' because building test suites has been
explicitly disabled in the configuration. ...
```

Reaching that required a change beyond the solver, because
`componentAvailableTargetStatus` in `Distribution.Client.ProjectPlanning` asks
`componentOptionalStanza` -- which answers from the component's *name* -- whether
a component belongs to an optional stanza. For a library it answered "no", so the
component was treated as always available, and asking for it with tests disabled
produced an internal error rather than a diagnosis:

```
Error: [Cabal-7127]
Internal error when trying to build the library 'testlib' from the package
cabal-install-3.19.0.0. The package,component pair is not in the set of
available targets for the project plan, which would suggest an inconsistency
between readTargetSelectors and resolveTargets.
```

The fix is that a library must be asked, not its name. That function already
receives the whole component, so it is a local change; the shared conversion
`libraryStanzaToOptionalStanza` now lives beside `OptionalStanza`.

That produced the right error class but the wrong noun -- "because building
*libraries* has been explicitly disabled" -- because `renderTargetProblem`
derived it with `renderComponentKind Plural (componentKind cname)`, which was
only ever right while an optional component was always a test-suite or benchmark.
Naming the stanza instead means carrying it, and the stanza is *not* known where
the problem is constructed: `selectComponentTargetBasic` sees only the
`AvailableTargetStatus`. So the two disabled statuses now carry it:

```haskell
    TargetDisabledByUser (Maybe OptionalStanza)
  | TargetDisabledBySolver (Maybe OptionalStanza)
```

and `TargetOptionalStanzaDisabledByUser` / `...BySolver` carry it onward to the
renderer. The result reads correctly for both kinds of component:

```
Cannot build the library 'testlib' because building test suites has been
explicitly disabled in the configuration. ...

Cannot build the test suite 'unit-tests' because building test suites has been
explicitly disabled in the configuration. ...
```

The field is `Maybe` because `Distribution.Client.CmdHaddock` repurposes
`TargetDisabledByUser` to mean "not requested by this target filter", where no
stanza is involved; there the renderer falls back to the component kind, which is
the existing wording. That repurposing is arguably worth its own status
constructor, but that is out of scope here.

The distinction is observable, and `IntegrationTests2` pins it. In the haddock
target-problem fixture a benchmark that *was* buildable is rewritten by haddock's
filter and so carries no stanza, while a test-suite already disabled by the solver
keeps its `Just TestStanzas`:

```haskell
  [ AvailableTarget "p-0.1" (CBenchName "user-disabled")
      -- haddock's own target filter, not a disabled stanza
      (TargetDisabledByUser Nothing) True
  , AvailableTarget "p-0.1" (CTestName "solver-disabled")
      (TargetDisabledBySolver (Just TestStanzas)) True
```

Getting this wrong is caught by the suite, which is a useful property: it means
the two meanings cannot quietly merge again.

The general lesson is that the assumption "optionality is a property of the
component's name" is encoded in several places, and each has to be revisited. The
prototype does so for the solver, for planning, and for the two renderers -- the
last of which also fixes the stanza named in `TargetProblemNoneEnabled`
messages, which had the same defect.

**`cabal check`.** The validation rule above is a new check. A stanza-scoped
sublibrary that no test-suite depends on is dead weight and could warrant a
warning.

**Backwards compatibility.** The field is gated on `cabal-version: 3.20`, so
older `cabal` reports an unsupported spec version rather than misreading the
package. A package using it cannot be built by older toolchains, which is the
normal cost of new `.cabal` syntax.

**sdist and `flattenPackageDescription`.** Flattening takes all components, so
source distributions are unaffected.

## Prototype

A working prototype accompanies this proposal, implemented against
`cabal-install` 3.19 / GHC 9.14.1.20260728.

### Result

`cabal-install`'s four test-suites share four helper modules. With them moved
into a `stanza: test` sublibrary:

| | bootstrap plan | `testlib` / `Cabal-QuickCheck` / `tasty` in plan |
| --- | --- | --- |
| before, no sublibrary | 18 packages | no |
| sublibrary, no `stanza:` field | **fails to resolve** | -- |
| sublibrary, `stanza: test` | **18 packages, identical** | **no** |

and with tests requested, `cabal build cabal-install:tests` pulls in
`cabal-install-3.19.0.0 (lib:testlib)` and `Cabal-QuickCheck` as expected. No
project-file configuration is involved in either direction: `--enable-tests`
alone decides.

### Changes

| file | change |
| --- | --- |
| `Cabal-syntax/src/Distribution/Types/LibraryStanza.hs` | new `LibraryStanza` type, modelled on `LibraryVisibility` |
| `Cabal-syntax/src/Distribution/Types/Library.hs` | `libStanza` field, `emptyLibrary`, `Semigroup` |
| `Cabal-syntax/src/Distribution/Types/Library/Lens.hs` | `libStanza` lens |
| `Cabal-syntax/src/Distribution/PackageDescription/FieldGrammar.hs` | `stanza` field, sublibraries only |
| `Cabal-syntax/src/Distribution/Types/ComponentRequestedSpec.hs` | `libraryStanzaNotRequestedReason`, consulted by `componentDisabledReason` |
| `Cabal-syntax/src/Distribution/PackageDescription/Configuration.hs` | `overallDependencies` consults the stanza as well as the name |
| `cabal-install-solver/.../Modular/IndexConversion.hs` | stanza-scoped sublibraries emitted under `prefix (Stanza ...)` |
| `Cabal/src/Distribution/PackageDescription/Check/Target.hs`, `Cabal/src/Distribution/Simple/Build.hs` | field added to positional match / record literal |
| `Cabal-tree-diff/src/Data/TreeDiff/Instances/Cabal.hs` | `ToExpr LibraryStanza` |
| `cabal-install-solver/.../Types/OptionalStanza.hs` | `libraryStanzaToOptionalStanza` |
| `cabal-install/.../ProjectPlanning.hs` | `componentAvailableTargetStatus` asks the component; the two disabled statuses carry the stanza |
| `cabal-install/.../TargetProblem.hs`, `ProjectOrchestration.hs`, `CmdErrorMessages.hs`, `CmdHaddock.hs` | stanza threaded to the renderers; messages name the stanza |
| `cabal-install/tests/IntegrationTests2.hs` | fixtures updated with the stanza |
| `Cabal/.../Check.hs`, `Check/Warning.hs` | four checks: `cross-stanza-dependency`, `public-stanza-library`, `conditional-stanza`, `unused-stanza-library` |
| `Cabal-tests/.../Utils/Structured.hs` | golden structure hashes for `GenericPackageDescription` and `LocalBuildInfo` |
| `Cabal-tests/tests/CheckTests.hs`, `ParserTests/regressions/stanza-*.cabal`, `*.check` | golden cases for the four findings |
| `Cabal-tests/tests/ParserTests.hs`, `stanza-roundtrip.cabal`, `.format`, `.expr` | pretty-printer and round-trip coverage |

The solver change is the whole mechanism, and it is small:

```haskell
++ concatMap (convSubLib initDR) [sl | sl <- sub_libs, isNothing (subLibStanza sl)]
++ prefix (Stanza (SN pn TestStanzas))
     (L.map (convSubLib (addStanza TestStanzas initDR))
            [sl | sl <- sub_libs, subLibStanza sl == Just TestStanzas])
```

### Tests

Each of the four checks has a golden case under
`Cabal-tests/tests/ParserTests/regressions/`, registered in `CheckTests.hs`. The
fixtures carry version bounds and a long enough description so that the golden
output is only the finding under test, rather than incidental advice that would
break the case whenever unrelated checks change.

`stanza-cross-dep.cabal` is the discriminating one: an executable and a
test-suite both depend on the same `stanza: test` library, and only the
executable is reported. `stanza-unused.cabal` holds a two-link dead chain and its
golden names both libraries, pinning the transitive behaviour.

Parsing and printing are covered separately by `stanza-roundtrip.cabal`,
registered in `ParserTests`, which carries a library for each of the three values
plus one that omits the field. That fixture gets the `format` golden, the `expr`
golden, and the round-trip assertion -- the last being the one that matters, since
it pretty-prints and re-parses and compares `condSubLibraries`, so a field that
printed but did not survive a re-parse would fail rather than quietly round-trip
to the default. The golden output confirms the intended asymmetry: `stanza: test`
and `stanza: bench` are printed, while `stanza: always` is omitted as the default
and still reads back as `LibraryStanzaAlways`.

These are real regression tests, not just recordings: disabling the cross-stanza
rule fails exactly `stanza-cross-dep.cabal`, with a readable diff, and leaves the
other three passing.

Note that `tasty-golden` writes a missing golden file and reports the test as
passing, so a first green run proves nothing. The files were read and checked
against what each case is meant to produce.

### Note on the golden data that a new field disturbs

Adding a field to `Library` moves two sets of golden data, and both are part of
the change rather than churn to be papered over.

The `Structured` hashes of `GenericPackageDescription` and `LocalBuildInfo`
change, because that hash is precisely how `cabal` invalidates its caches when
the description format changes.

The 38 `.expr` fixtures under `Cabal-tests/tests/ParserTests/regressions/` also
move, since they pretty-print a parsed `GenericPackageDescription`. Regenerating
them with `--accept` is safe here only because the result was checked: across all
38 files the diff is 88 added lines, every one of them
`libStanza = LibraryStanzaAlways,`, and nothing removed. A regeneration that
showed anything else would mean the field had changed behaviour rather than
merely appeared.

### Deliberate deviations

- **The field is spelled `stanza:`.** See [Naming](#naming); `requested-by:` is
  the recommended spelling and the rename is mechanical. The prototype predates
  that discussion.
- **The `cabal-version` gate is relaxed.** The shipped field should carry
  `availableSince CabalSpecV3_20`. The prototype omits it because the in-tree
  `Cabal` is 3.19, so no available `Cabal` can satisfy a `cabal-version: 3.20`
  package and `cabal-install` could not build itself. The one-line gate is marked
  in `FieldGrammar.hs`.

### Not implemented

Nothing outstanding from the original list. What remains is judgement the
proposal process should settle rather than the prototype: the field's spelling
(see [Naming](#naming)), whether `CmdHaddock`'s reuse of `TargetDisabledByUser`
deserves its own status constructor, and documentation for the users' guide.
- Explicit-target behaviour (`cabal build pkg:lib:testlib` with tests disabled) is
  untested.
- `optionalStanza` (`CmdErrorMessages.hs`) still answers from the component's
  name alone. It is now only reached for statuses that carry no stanza, so it is
  no longer wrong, but it remains a name-keyed answer to a question that is not
  about names.
- `CmdHaddock`'s reuse of `TargetDisabledByUser` for "not requested by this
  target filter" would be better as its own status constructor.
- Parser round-trip tests, pretty-printer tests, documentation.
