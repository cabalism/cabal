# Review scripts for PR #11252 (exact-pp-leana)

Reproductions for review findings on https://github.com/haskell/cabal/pull/11252.

## Against this branch

Each `fN-*.ghci` file is a GHCi script for the in-tree Cabal-syntax:

    cabal repl Cabal-syntax -v0 < review/11252/f1-comment-before-brace.ghci

## Against the released Cabal-syntax

`ReleasedCabalSyntax.hs` runs the same inputs against the Cabal-syntax bundled with GHC, to show which failures are regressions:

    ghc -package Cabal-syntax -o /tmp/released review/11252/ReleasedCabalSyntax.hs && /tmp/released

With GHC 9.14.1 (Cabal-syntax 3.16.0.0) every input prints OK.
