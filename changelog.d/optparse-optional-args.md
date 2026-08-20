---
synopsis: Fix optional-argument options (e.g. `-j`/`--jobs`) in nix-style commands
packages: [cabal-install]
prs: []
issues: []
---

The optparse-applicative parser used by the nix-style (v2) commands mishandled
optional-argument options such as `-j[NUM]` / `--jobs[=NUM]`,
`-v[n]` / `--verbose[=n]` and `-O[n]` / `--enable-optimization[=n]`.

Because optparse-applicative has no native support for optional option
arguments, these were modelled with an argument-taking parser that
unconditionally consumed the following token. As a result:

* a bare `-j` at the end of the command line failed to parse, and
* `cabal build -j all` treated the target `all` as the value of `-j` and
  errored with `option -j: The jobs value should be a number or '$ncpus'`.

The parser now models each optional-argument option as its documented forms
only — the bare flag (`-j`, `--jobs`) yielding the default, and the attached
value form (`--jobs=NUM`; the short `-jNUM` is normalised to it). A token that
merely follows a bare option is treated as a positional argument, matching the
GetOpt-based parser and what `cabal COMMAND --help` advertises. The
space-separated form (`-j 4`) is not supported, consistent with the `-j[NUM]`
help syntax.
