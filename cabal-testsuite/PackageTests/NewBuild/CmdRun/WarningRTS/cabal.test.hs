import Test.Cabal.Prelude

opts1 = ["+RTS"]
opts2 = ["+RTS", "--"]
opts3 = ["--", "+RTS"]

main = do
  cabalTest $ do
    res <- cabal' "run" ("foo" : opts1)
    assertOutputContains "Warning: Your RTS options" res

    res <- cabal' "run" ("foo" : opts2)
    assertOutputContains "Warning: Your RTS options" res

    res <- cabal' "run" ("foo" : opts3)
    assertOutputDoesNotContain "Warning: Your RTS options" res

  -- Regression tests for https://github.com/haskell/cabal/issues/10487:
  -- 'cabal run -- +RTS' should not fail with "Unrecognised target '+RTS'"
  cabalTest' "no-target" $ do
    res <- cabal' "run" opts1
    assertOutputContains "Warning: Your RTS options" res

    res <- cabal' "run" opts2
    assertOutputContains "Warning: Your RTS options" res

    res <- cabal' "run" opts3
    assertOutputDoesNotContain "Warning: Your RTS options" res
    assertOutputDoesNotContain "Unrecognised target" res
