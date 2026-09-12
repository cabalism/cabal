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
- Accepted only on sublibraries. On the main library it is a parse error.
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

## Interactions

**Explicit targets.** `cabal build pkg:lib:testlib` with tests disabled should
behave as `cabal build pkg:test:foo` does today. `OneComponentRequestedSpec`
already exists for the explicit-target case.

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

The solver change is the whole mechanism, and it is small:

```haskell
++ concatMap (convSubLib initDR) [sl | sl <- sub_libs, isNothing (subLibStanza sl)]
++ prefix (Stanza (SN pn TestStanzas))
     (L.map (convSubLib (addStanza TestStanzas initDR))
            [sl | sl <- sub_libs, subLibStanza sl == Just TestStanzas])
```

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

- The validation rule. Nothing currently stops `lib` depending on a
  `stanza: test` sublibrary; the prototype relies on the package being
  well-formed. This is the main piece of work remaining, and it is what makes
  gating only the dependencies sound.
- `cabal check` rules.
- Explicit-target behaviour (`cabal build pkg:lib:testlib` with tests disabled) is
  untested.
- `componentOptionalStanza` (`ProjectPlanning/Types.hs`) and `optionalStanza`
  (`CmdErrorMessages.hs`) still answer from the component's name alone, so a
  stanza-scoped sublibrary is not reported as belonging to a stanza in planning
  and error messages. This did not block the cases tested, but should be
  addressed.
- Parser round-trip tests, pretty-printer tests, documentation.
