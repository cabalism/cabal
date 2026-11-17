synopsis: Add a timings setting
packages: cabal-install
prs: #11769 #NNNNN
description:{
Introduce the `timings` setting, `--timings` on the command line, which makes `cabal-install`
print the elapsed time of each unit of work it does, one line each, in the format
`<seconds>s [timing] <scope>:<step> <subject> [<note>]`, the seconds right-aligned so the
column lines up and `sort -n` ranks the lines:

```
   0.042s [timing] pkg:configure aeson-2.2.3.0
   3.284s [timing] pkg:build aeson-2.2.3.0
   0.123s [timing] pkg:install aeson-2.2.3.0
```

The scope is `pkg` for a phase of one package and `plan` for a planning step, so a grep
narrows from `[timing]` to `[timing] pkg:` to `[timing] pkg:build` to one package.
}
