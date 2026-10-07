# Override package sources

## Summary

When several project files list a source for the same package, with `packages` or
`source-repository-package`, keep the source listed at the strongest *position* and drop the others.
Position is the layer and import depth defined in the `override-constraints` proposal:
`cabal.project.local` and its imports outrank `cabal.project`, its freeze file and their imports, and
within a layer a file nearer the root of the imports outranks the files it imports. Every source is
still fetched and read first, because cabal only learns which package a source provides by reading
it. Then the choice is made per package *name*, before the solver runs. Two different sources at the
same position are an error. The same source listed twice counts once. Cabal reports what it used and
what it ignored.

## Motivation

[#8463](https://github.com/haskell/cabal/issues/8463) asks for a `source-repository-package` in an
importing file to override one for the same package in an imported file. Its first comment by
sellout (2026-10-07) asks for the `cabal.project.local` version of the same thing, plus:

- a `packages: ../foo` entry in `cabal.project.local` replacing a `source-repository-package` for
  `foo` in `cabal.project`, so several repositories can be worked on side by side without committing
  machine-specific paths;
- a notice saying which source was chosen when several exist.

Today the lists of package locations are concatenated across files. Two sources for the same
package id produce the notice "cabal project has multiple sources for foo-1.0: ... the choice of
source that will be used is undefined", and the solver's package index keeps whichever was inserted
last, which depends on the kind of location rather than on which file named it. Two sources for the
same name at *different* versions produce two conflicting `==` pins and the solver fails.
[#7222](https://github.com/haskell/cabal/issues/7222) notes that a local package and a
`source-repository-package` for the same package "confuses internals".

gbaz's objection in #8463 (2022-09-19) is the design constraint:

> Cabal actually has no notion that these are repositories that are used to satisfy particular
> dependencies. ... there's no real way for cabal to see that the two provide the same package --
> they're different urls, which could point to *anything*. ... it would be ad-hoc and not always
> correct for any future import of a src-repository-package in one repo with one hash to override an
> existing one in the same repo with a different hash.

Mikolaj (2022-09-19): "if somebody proposes a good method to implement it, that would move things
forward."

The proposal answers the objection by not matching stanzas at all. Every source is fetched and read,
as today. Only then, with the package names in hand, does cabal choose between sources of the same
name. The choice uses the position order that `override-constraints` already defines, so the two
features share one notion of which project file outranks which.

## Prior art

| Tool | Mechanism | Who may override |
|---|---|---|
| Stack (`extra-deps`) | A package in `stack.yaml` replaces the snapshot's version of the same name, whether from Hackage, a git repository or a local directory | Closer layer wins |
| Cargo (`[patch]`) | Replaces a crate's source by crate name, after reading the replacement | Root workspace only |
| Go modules (`replace`) | Replaces a module path with another path or a local directory | Main module only |
| npm (`overrides`), Yarn (`resolutions`), pnpm (`overrides`) | Replace a dependency's resolution by package name | Root `package.json` only |
| Nix flakes (`inputs.foo.follows`) | Redirects an input by name from the outer flake | Outer flake wins |

All of them key on the *name* of the package being replaced, and all let the nearer or more
local declaration win. None matches on the URL of the source being replaced, which is the matching
gbaz called ad hoc.

## Proposed Change

### Position

Every `packages`, `optional-packages` and `source-repository-package` entry has the position of the
project file that contains it. Positions are those of the `override-constraints` proposal:

1. `cabal.project.local` and its imports;
2. `cabal.project`, `cabal.project.freeze` and their imports;

with depth counted from the layer's root file. The implicit project used when there is no
`cabal.project` counts as a root project file. The command line and the global config file never
contribute package sources, so the command-line and global layers do not arise here.

### Resolution

After every source has been located, fetched and read:

- **S1:** Group the resulting source packages by package name. Packages named by `extra-packages`
  are not sources and take no part.
- **S2:** Within a group, a source listed more than once is one source with the provenances of all
  its listings. Only its first listing is kept, silently. This is what makes `packages: .` in both
  `cabal.project` and `cabal.project.local` keep working, and what makes the same
  `source-repository-package` stanza in two files one checkout.
- **S3:** Among the remaining sources, those at the strongest position win. If exactly one wins, the
  others are dropped before the solver runs, and reported. If several win, it is an error that
  names the package and every winning source with its file.
- **S4:** A source with no known provenance is kept and takes no part in the choice. None arises
  today; the rule is for completeness.

The solver is unchanged. It receives one source per package name, or fails earlier with a clearer
message than its own conflicting-pins error.

### Reporting

At normal verbosity, for each package whose other sources were dropped:

```
cabal project has multiple sources for foo:
  using ../foo from cabal.project.local
  ignoring https://github.com/example/foo.git from cabal.project
```

For a conflict:

```
cabal project has different sources for foo at the same position:
  /home/me/proj/foo from cabal.project
  /home/me/proj/foo-copy from cabal.project
Remove all but one, or list the one to use in a project file that outranks these, such as cabal.project.local.
```

Files are shown with their import chain, as constraint sources are.

### Examples

1. **Local directory over a repository.** `cabal.project` has `packages: app` and a
   `source-repository-package` for `foo`. `cabal.project.local` has `packages: ../foo`. The
   directory wins; the repository is still cloned, then ignored with a notice.
2. **Repository over repository.** `cabal.project.local` has a `source-repository-package` for `foo`
   at a newer commit. It wins over the stanza in `cabal.project`.
3. **Root over import.** `cabal.project` has `packages: foo-0.2` and imports a file whose
   `source-repository-package` provides `foo`. The root wins. With the stanza in the root and the
   directory in the import, the root still wins: an import never beats the file that imports it.
4. **Conflict.** `packages: app foo foo-copy` in one file, where both directories hold `foo`. Error.
   Previously the notice said the choice was undefined. Listing one of them in `cabal.project.local`
   resolves it, since that outranks both.
5. **Different versions, same position.** `packages: app foo-0.2 foo-0.3`. Error, with the message
   above. Previously the solver failed with two conflicting `==` pins.
6. **Same stanza twice.** The same `source-repository-package` in `cabal.project` and
   `cabal.project.local`. One checkout, one source, no notice.

## Alternatives Considered

- **Match stanzas by repository URL or hash.** This is the matching gbaz rejected as ad hoc: two
  URLs may provide the same package, and one URL may provide several.
- **A grammar for removing stanzas**, as gbaz sketched. It would need a way to name a stanza from
  another file, and users would have to repeat the removal for every stanza that provides the
  package. Choosing by name after reading needs no new syntax.
- **Last wins, by order of files or lines.** The position order is already how the rest of the
  project configuration layers, and it does not change when an `import:` line moves.
- **Keep the undefined choice for same-position duplicates.** Rejected in favour of an error, so
  that a project never builds against a source chosen by accident. This is the one behaviour change
  that can turn a previously passing project into a failing one; the fix is to delete one listing or
  to name the intended source in `cabal.project.local`.
- **Skip fetching the losers.** Impossible in general: identity is only known after reading. A later
  optimisation could skip a repository when a local directory at a stronger position already
  provides the name, but only after reading the directory, and only if it is acceptable for the
  result to depend on the order of reading.
- **Let `extra-packages` take part.** A named package is a solver target, not a source; a local
  source for the same name already shadows the index entry.

## Backwards Compatibility / Migration

- Projects with one source per package name are unaffected.
- Projects with the same source listed in several files are unaffected: the listings merge.
- Projects with different sources for one name at different positions used to fail in the solver,
  or to build against an undefined choice. They now build against the stronger source, with a
  notice.
- Projects with different sources for one name at the same position used to get the "undefined"
  notice (same version) or a solver failure (different versions). They now fail before solving with
  a message naming both files. This is the only case where a project that built before stops
  building, and it only affects projects that were already building against an arbitrary choice.
- `cabal configure` writes `cabal.project.local` through the legacy printer, which drops the
  provenance; it is restored on reading, so nothing changes in the written file.

## Implementation Notes

The implementation accompanies this proposal on the branch `add/override-packages`, stacked on the
`override-constraints` branch.

- **Provenance.** `projectPackages`, `projectPackagesOptional`, `projectPackagesRepo` and
  `projectPackagesNamed` in `ProjectConfig` become lists of pairs with a `ProjectConfigProvenance`,
  as `projectConfigConstraints` pairs each constraint with its `ConstraintSource`. Both parsers tag
  entries with the file being parsed. The legacy printer drops the tag. This has to happen at parse
  time: both parsers merge an importing file's fields with the imported file's result into one
  skeleton node, and `readProjectConfig` merges the root files the same way, so the file of origin
  cannot be recovered later.
- **Positions.** `pathPosition :: ProjectConfigPath -> Position` is factored out of
  `constraintPosition` in `Distribution.Client.ProjectConfig.Override`.
- **Reading.** `findProjectPackages` returns each location with its provenance.
  `fetchAndReadSourcePackagesTagged` carries a tag through fetching and returns each package with the
  tags of the locations it came from. Repository packages are matched back to their stanzas through
  the fanned-out repository recorded in their location, so the repository cache and its monitors
  are unchanged.
- **Resolution.** A new pure module, `Distribution.Client.ProjectConfig.Sources`, holds
  `resolveDuplicateSourcePackages`, the notice and the error. It runs in `phaseReadLocalPackages`,
  replacing the "multiple sources ... undefined" notice, before the solver.
- **Tests.** Unit tests for the resolver, and a cabal-testsuite package test covering the six
  examples above with both project file parsers. The existing `ProjectDupPkg` test now expects the
  error.

## Open Questions

1. Should same-position duplicates be an error, as proposed, or keep today's notice and undefined
   choice?
2. Should `cabal install URL` apply the same rule? Its sources all come from the command line, so
   it would only ever report a tie.
3. Should the provenance of each entry also be used in `BadPackageLocations`, so that a bad glob is
   reported with the file that listed it? The data is now there.
4. Should a package named by `extra-packages` take part in the choice?

## References

- [#8463 Importing a `source-repository-package` cannot be overridden](https://github.com/haskell/cabal/issues/8463)
- [#7222 When there is local package, and source-repository-package for the same package](https://github.com/haskell/cabal/issues/7222)
- [#5444 source-repository-package not optional](https://github.com/haskell/cabal/issues/5444)
- The `override-constraints` proposal, for positions
- [Stack `extra-deps`](https://docs.haskellstack.org/en/stable/topics/package_location/)
- [Cargo `[patch]`](https://doc.rust-lang.org/cargo/reference/overriding-dependencies.html)
- [Go `replace`](https://go.dev/ref/mod#go-mod-file-replace)
- [npm `overrides`](https://docs.npmjs.com/cli/v10/configuring-npm/package-json#overrides)
- [Nix flake inputs `follows`](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-flake.html#flake-inputs)
