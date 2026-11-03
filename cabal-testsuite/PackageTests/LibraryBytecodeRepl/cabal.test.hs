import Test.Cabal.Prelude

-- Test that `repl` honours --enable-library-bytecode: GHCi writes bytecode
-- (.gbc) and interface files into the build directory and reuses them on the
-- next run, and reuses those written by `build`. See #11981.
main = do
    -- A first repl writes the bytecode files, a second one reuses them.
    cabalTest' "repl-then-repl" $ defaultRecordMode RecordMarked $ do
        -- Bytecode artifacts are only supported in GHC >= 9.15.0
        skipUnlessGhcVersion ">= 9.15"
        env <- getTestEnv
        r1 <- cabalWithStdin "v2-repl" ["--enable-library-bytecode", "lib:library-bytecode-repl"] ":show modules"
        assertOutputContains "Compiling Lib" r1
        gbcs <- liftIO $ matchGlob (testDistDir env) "**/Lib.gbc"
        assertBool "expected a Lib.gbc file after cabal repl" (not (null gbcs))
        r2 <- cabalWithStdin "v2-repl" ["--enable-library-bytecode", "lib:library-bytecode-repl"] ":show modules"
        assertOutputDoesNotContain "Compiling Lib" r2
        assertOutputContains "Ok, one module loaded." r2

    -- A build writes the bytecode files, a repl reuses them.
    cabalTest' "build-then-repl" $ defaultRecordMode RecordMarked $ do
        skipUnlessGhcVersion ">= 9.15"
        cabal "v2-build" ["--enable-library-bytecode", "lib:library-bytecode-repl"]
        r <- cabalWithStdin "v2-repl" ["--enable-library-bytecode", "lib:library-bytecode-repl"] ":show modules"
        assertOutputDoesNotContain "Compiling Lib" r
        assertOutputContains "Ok, one module loaded." r

    -- The repl writes and reuses bytecode for executables too.
    cabalTest' "exe-repl" $ defaultRecordMode RecordMarked $ do
        skipUnlessGhcVersion ">= 9.15"
        r1 <- cabalWithStdin "v2-repl" ["--enable-library-bytecode", "exe:exe"] ":show modules"
        assertOutputContains "Compiling Main" r1
        r2 <- cabalWithStdin "v2-repl" ["--enable-library-bytecode", "exe:exe"] ":show modules"
        assertOutputDoesNotContain "Compiling Main" r2
        assertOutputContains "Ok, one module loaded." r2
