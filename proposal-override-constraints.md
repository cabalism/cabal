# Override constraints

## Summary

Add an `override-constraints` field to project files and a `--override-constraint` command-line flag. Plain
`constraints` stay additive: they are intersected, as today. An override *replaces* other constraints of the same kind
on the same package, but only those at a weaker position. A position is weaker if it comes from a less authoritative
configuration layer, or sits deeper in the import tree. Two different overrides at the same position are an error.
Overrides are resolved before the solver runs, so the result doesn't depend on the order of lines or imports.

## Motivation

Importing a package set, such as a Stackage snapshot's `cabal.config`, pins hundreds of packages with `==` and
`installed`. Cabal constraints only intersect, so a project cannot use a different version of even one pinned package.
The user guide documents a workaround: download the `cabal.config`, then repeatedly comment out conflicting lines.
Updo automates exactly that edit.

The same need shows up beyond snapshots:
- company-wide constraint files;
- CI-only configuration;
- a local `cabal.project.local` that bumps one dependency without editing shared files.

Earlier attempts and discussion:

- #9511 is the issue.
- #9510 tried implicit "shallowest import wins" for `==`, applied inside the solver behind `--version-win`. It later
  experimented with "last wins".
- The review of #9510 raised these points:
  - Keep project semantics out of the solver, and pick one semantics rather than a mode flag.
  - "Imported" should not implicitly mean "overridable"; express intent explicitly.
  - Constraints are more than versions. Flags matter, and snapshots need their flags overridden too.
  - Some prefer order, where later overrides earlier. Others want conflicts at the same level to stay errors.
- Masking with `hide-constraints` on an import removes that import's constraints wholesale. It's coarse: it drops
  flags along with versions, and must be attached to each import.

This proposal aims for:
- explicit intent;
- deterministic results that don't depend on order;
- same-level conflicts reported as errors;
- no solver changes;
- per-property granularity.

## Prior art

| Tool | Mechanism | Who may override | Conflicts |
|---|---|---|---|
| npm (`overrides`), Yarn (`resolutions`), pnpm (`pnpm.overrides`) | Replace a dependency's version anywhere in the tree, optionally scoped to a parent (`foo>bar`, `**/bar`) | Root `package.json` only | npm rejects an override that contradicts a direct dependency unless it references it |
| Dart pub (`dependency_overrides`) | Ignore every constraint on a package and use the given one | Root package only | pub warns that overrides are in effect |
| Elixir Mix (`override: true`) | A dependency declared in the root wins over requirements from other dependencies | Root project | Without `override`, diverging requirements are an error |
| Go modules (`replace`, `exclude`) | Replace or exclude module versions | Main module only; ignored in dependencies | n/a |
| Bazel bzlmod (`single_version_override`, …) | Pin or patch a module version | Root module only; ignored elsewhere | n/a |
| Cargo (`[patch]`) | Replace a crate's source, still within semver | Root workspace only | Must satisfy requirements |
| uv (`constraint-dependencies` vs `override-dependencies`) | *Additive* constraints and *replacing* overrides are two separate fields | Workspace root | Overrides replace all requirements on that package |
| Maven (`dependencyManagement`, BOM import) | "Nearest wins" mediation; local declarations beat imported BOMs | Nearer in the tree | Between BOMs, first import wins (order) |
| Gradle (rich versions `require`/`strictly`/`reject`, `force`) | Explicit strength per declaration | Anyone | Conflicting `strictly` fails the build |
| Stack (snapshot layering, `extra-deps`, `flags`, `drop-packages`) | `stack.yaml` and child snapshots override parent snapshot versions and flags | Closer layer wins | n/a |
| Nix module system (`mkDefault`, `mkForce`, `mkOverride n`) | Numeric priority; the highest-priority definition wins | Anyone | Same priority with different values is an error |
| nixpkgs overlays | A later overlay wins over `prev` | Anyone | Order-based |
| Docker Compose (multiple `-f` files; `!override`, `!reset`) | Later files merge; explicit tags replace or remove | Later files | Order-based |
| Spack configuration scopes (`::` suffix) | Scopes ranked defaults < … < user < command line; `::` replaces instead of merging | Higher scope | Scope rank |

Four themes run through these tools:

1. **Replacing is marked explicitly, separately from merging.** Examples are uv's two fields, Compose `!override`,
   Spack `::`, Gradle `strictly` and Nix `mkForce`.
2. **Authority sits with the root, or with whatever is nearer to it.** npm, Yarn, pnpm, Dart, Mix, Go, Bazel and Cargo
   honour overrides only in the root. Maven's nearest-wins is the graded version of the same idea.
3. **Conflicts at equal strength are errors**, as in Nix, Gradle and Mix.
4. **Order-based systems exist**, such as overlays, Compose and Maven BOMs, but order is a known source of surprise.

This proposal takes themes 1–3, and keeps project configuration order-independent, as it is today.

## Proposed Change

### Syntax

```cabal
-- cabal.project
import: https://www.stackage.org/lts-23.0/cabal.config

override-constraints: hashable ==1.4.2.0, text ==2.0.2, any.foo -bar
```

```console
$ cabal build --override-constraint="hashable ==1.4.2.0"
```

`override-constraints` uses exactly the syntax of `constraints`. An override is itself a constraint: it goes to the
solver like any other.

### Position

Every constraint has a **position**, made of a layer and a depth.

The **layers**, strongest first, are:
1. the command line;
2. `cabal.project.local` and its imports;
3. `cabal.project`, `cabal.project.freeze` and their imports;
4. the user config file.

The **depth** is the number of imports between the layer's root file (depth 0) and the file that contains the
constraint.

Position A is **stronger** than position B when either:
- A's layer is stronger than B's; or
- the layers are the same and A has the smaller depth.

### Kinds

An override replaces constraints of the same *kind* on the same package:

- **instance:** version ranges, `installed` and `source`. An override `Cabal ==3.12.1.0` replaces a snapshot's
  `any.Cabal installed`.
- **flag *f*:** one kind per flag name. An override `foo -bar` replaces only the `bar` part of `any.foo +bar -baz`,
  leaving `any.foo -baz`.
- **stanzas:** stanza constraints such as `foo test` only ever enable a stanza, so they are not overridable.

The scopes must also overlap:
- `any.p` overlaps every scope for `p`;
- `setup.p` overlaps setup-qualified scopes;
- any other scopes overlap only if they are equal.

So a top-level override `hashable ==x` does replace a snapshot's `any.hashable ==y`.

### Rules

- **R1:** Plain constraints never remove anything. They intersect, as today.
- **R2:** An override replaces:
  - every constraint of the same kind, with an overlapping scope, at a *weaker* position;
  - *plain* constraints of the same kind, with an overlapping scope, at the *same* position.
- **R3:** Two different overrides of the same kind, with overlapping scopes, at the same position are an error that
  names both files. Identical overrides are merged.
- **R4:** Constraints that cabal adds itself are never replaced. These include non-reinstallable packages, `setup`
  Cabal version bounds and profiled-dynamic.
- **R5:** Resolution is a pure pass over the collected constraints, before solving. The solver is unchanged.

### Examples

1. **Sibling imports, as updo lays out a project.** `cabal.project` imports `stackage.config` and
   `constraints.config`, both at depth 1. `constraints.config` has `override-constraints: hashable ==1.4.2.0`, which
   replaces the snapshot's plain `hashable ==1.4.3.0` at the same position.
2. **A root override beats a deep import.** An override in `cabal.project` replaces a snapshot pin that is imported
   three levels down.
3. **An import cannot overrule the root.** Suppose `cabal.project` has a plain `constraints: hashable ==1.4.3.0` and a
   remote import has `override-constraints: hashable ==1.4.2.0`.
   - The import is at a weaker position, so nothing is replaced.
   - The solver reports the conflict with both sources, as it does today.
   - The root can resolve it by making its own constraint an override, or by using `hide-constraints` on that import.
4. **Conflict.** Two sibling imports both override `hashable`, with different versions:

   ```
   Error: conflicting override-constraints at the same position:
     hashable ==1.4.2.0 from a.config (imported by cabal.project)
     hashable ==1.4.3.0 from b.config (imported by cabal.project)
   ```

5. **Local tweak.** `cabal.project.local` overrides a version pinned in `cabal.project` or its freeze file, without
   editing either file.

### Reporting

- With `-v2`, every replacement is listed with the override, what it replaced, and both sources.
- An override that replaces a plain constraint in the *same file* gets a warning, since that is likely a mistake.
- Solver failure messages keep showing constraint sources, now including overrides.

### Relation to `hide-constraints`

The two features are complementary:
- `hide-constraints` is a per-import veto. It removes both plain and override constraints from that import's subtree.
- `override-constraints` is a precise replacement for one property of one package, and isn't tied to a particular
  import.

## Alternatives Considered

- **Last wins, based on order.**
  - Simple to state, but project fields are otherwise order-independent.
  - Moving an `import:` would silently change semantics.
  - It cannot report conflicts at the same level.
  - Maven's BOM ordering and nixpkgs overlays show how order confuses people.
- **Implicit shallowest-wins for `==` (#9510).** It ties overriding to the import structure without any stated intent.
  It ignores flags and `installed`, and it was implemented in the solver.
- **Root-only overrides, as npm, Dart, Go and Bazel do.** This is the simplest model. It could be a first phase of
  this proposal, allowing `override-constraints` only at depth 0 and on the command line. However, it cannot express
  updo's layout, where overrides live in an imported, generated `constraints.config`.
- **Numeric priorities, like Nix's `mkOverride`.** Maximally flexible, but heavyweight for cabal users, and priorities
  in remote files become an arms race.
- **Importing as preferences**, where a modifier on `import: X` turns its `==` constraints into `preferences`. The
  solver silently deviates when it can't satisfy them, which hides mistakes and can't be reported deterministically.
- **Retrying the solver after dropping conflicting constraints.** The #9510 review rejected this, on the grounds that
  the solver's inputs should be fixed.
- **An `override` marker inside `constraints:`**, for example `constraints: hashable ==1.4.2.0 !`. A separate field is
  easier to grep for, easier for both project parsers to handle, and mirrors naturally as a command-line flag.
- **Masking only, with `hide-constraints`.** It drops flags along with versions, and must be repeated on every import
  that pins the package.

## Backwards Compatibility / Migration

- The change is purely additive: without `override-constraints`, behaviour is unchanged.
- Older cabal-install versions warn about an unknown field, then report the same solver conflict as today.
- Existing workarounds, such as edited downloaded snapshots or `hide-constraints`, keep working and can be replaced
  gradually.
- `cabal freeze` output keeps its form. It records the plan that was solved with overrides applied, as plain
  constraints.

## Interested parties

- People and tools that consume Stackage `cabal.config` files: updo, haskell.nix users and Stackage curators.
- Participants in the #9511 and #9510 discussions: gbaz, michaelpj, int-index, andreabedini, phadej and Mikolaj.

## Implementation Notes

- **Field.** Add `projectConfigOverrideConstraints :: [(UserConstraint, ConstraintSource)]` to `ProjectConfigShared`,
  next to `projectConfigConstraints` in `ProjectConfig/Types.hs`, with:
  - a lens in `ProjectConfig/Lens.hs`;
  - a legacy field description in `ProjectConfig/Legacy.hs`;
  - a grammar entry in `ProjectConfig/FieldGrammar.hs`.
- **Command line.** Add `--override-constraint` to `configureExOptions` in `Setup.hs`, tagged
  `ConstraintSourceCommandlineFlag`.
- **Position.**
  - The depth comes from `ProjectConfigPath`, which already records the import chain.
  - The layer comes from the source constructor and the root file; `ProjectFileKey` distinguishes `.local` and
    `.freeze`.
  - This probably means tagging the layer when each root skeleton is read in `readProjectConfig`, rather than inferring
    it from file names.
- **Resolution.**
  - A new pure module, `Distribution.Client.ProjectConfig.Override`, called from `resolveSolverSettings` where
    `solverSettingConstraints` is built.
  - Conflicts raise a `CabalInstallException`.
  - `cabal-install-solver` is unchanged.
- **Tests.**
  - QuickCheck properties:
    - with no overrides, the result is the identity;
    - the result doesn't depend on input order;
    - command-line plain constraints are never removed by overrides from files;
    - resolution is idempotent.
  - cabal-testsuite package tests against a local repo, as the `hide-constraints` tests do. Cases:
    - sibling imports;
    - a deep import;
    - the root beating an import;
    - a conflict;
    - flag granularity.
- **Prototypes.** #9510 (depth tracking) and the `hide-constraints` branch (tree filtering in both parsers) show that
  the plumbing is small.

## Open Questions

1. Should `cabal.project.local` outrank `cabal.project`, as proposed, or tie with it?
2. Is it acceptable that a top-level override for `p` replaces a broader `any.p` constraint? Setup and exe instances of
   `p` would lose the snapshot pin.
3. Flags set in `package p` stanzas (`flags:`) lose their file provenance before solving. Should they get positions so
   that overrides can replace them, or stay out of scope?
4. Should overrides be allowed in remote (URI) imports? The position rule already stops them from beating the root.
5. Should v1 commands such as `v1-install` and `v1-freeze` honour overrides or ignore them?
6. Should `installed` and `source` share a kind with version ranges, as proposed?
7. What should the field be called: `override-constraints`, `constraint-overrides` or `force-constraints`?
8. Should a first phase ship root-only overrides (depth 0 and the command line) before depth-graded ones?

## References

- #9511 Override version equality constraints
- #9510 Override imported package version equalities (prototype and review)
- #7556 Conditionals and imports in project files
- The `hide-constraints` branch (masking on imports)
- Tool documentation:
  - npm `overrides`, Yarn `resolutions` and pnpm `overrides`
  - Dart `dependency_overrides`
  - Mix `override`
  - Go `replace` and `exclude`
  - Bazel `single_version_override`
  - Cargo `[patch]`
  - uv `override-dependencies` and `constraint-dependencies`
  - Maven dependency mediation
  - Gradle rich versions
  - Stack snapshots
  - Nix module priorities
  - Docker Compose merging (`!override`, `!reset`)
  - Spack configuration scopes
