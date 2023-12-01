module UnitTests.Distribution.Simple.Program.GHC (tests) where

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Algorithm.Diff (PolyDiff (..), getDiff)
import Test.Tasty          (TestTree, testGroup)
import Test.Tasty.HUnit

import Distribution.System (Platform(..), Arch(X86_64), OS(Linux))
import Distribution.Types.ParStrat
import Distribution.Simple.Flag
import Distribution.Simple.Compiler (Compiler(..), CompilerId(..), CompilerFlavor(..), AbiTag(NoAbiTag))
import Distribution.PackageDescription (emptyPackageDescription)
import Distribution.Simple.Program.GHC (normaliseGhcArgs, renderGhcOptions, ghcOptNumJobs)
import Distribution.Version            (mkVersion)

tests :: TestTree
tests = testGroup "Distribution.Simple.Program.GHC"
    [ testGroup "normaliseGhcArgs"
        [ testCase "options added in GHC-8.8" $ do
            let flags :: [String]
                flags = normaliseGhcArgs
                    (Just $ mkVersion [8,8,1])
                    emptyPackageDescription
                    options_8_8_all

            assertListEquals flags options_8_8_affects

        , testCase "options added in GHC-8.10" $ do
            let flags :: [String]
                flags = normaliseGhcArgs
                    (Just $ mkVersion [8,10,1])
                    emptyPackageDescription
                    options_8_10_all

            assertListEquals flags options_8_10_affects

        , testCase "options added in GHC-9.0" $ do
            let flags :: [String]
                flags = normaliseGhcArgs
                    (Just $ mkVersion [9,0,1])
                    emptyPackageDescription
                    options_9_0_all

            assertListEquals flags options_9_0_affects

        , testCase "options added in GHC-9.2" $ do
            let flags :: [String]
                flags = normaliseGhcArgs
                    (Just $ mkVersion [9,2,1])
                    emptyPackageDescription
                    options_9_2_all

            assertListEquals flags options_9_2_affects

        , testCase "options added in GHC-9.4" $ do
            let flags :: [String]
                flags = normaliseGhcArgs
                    (Just $ mkVersion [9,4,1])
                    emptyPackageDescription
                    options_9_4_all

            assertListEquals flags options_9_4_affects

        , testCase "options added in GHC-9.6" $ do
            let flags :: [String]
                flags = normaliseGhcArgs
                    (Just $ mkVersion [9,6,1])
                    emptyPackageDescription
                    options_9_6_all

            assertListEquals flags options_9_6_affects

        , testCase "options added in GHC-9.8" $ do
            let flags :: [String]
                flags = normaliseGhcArgs
                    (Just $ mkVersion [9,8,1])
                    emptyPackageDescription
                    options_9_8_all

            assertListEquals flags options_9_8_affects
        ]
    , testGroup "renderGhcOptions"
      [ testCase "options" $ do
            let flags :: [String]
                flags = renderGhcOptions
                  (Compiler
                      { compilerId = CompilerId GHC (mkVersion [9,8,1])
                      , compilerAbiTag = NoAbiTag
                      , compilerCompat = []
                      , compilerLanguages = []
                      , compilerExtensions = []
                      , compilerProperties = Map.singleton "Support parallel --make" "YES" 
                      })
                  (Platform X86_64 Linux)
                  (mempty { ghcOptNumJobs = Flag (NumJobs (Just 4)) })
            assertListEquals flags ["-j4", "-clear-package-db"]
        ]        
    ]

assertListEquals :: (Eq a, Show a) => [a] -> [a] -> Assertion
assertListEquals xs ys
    | xs == ys = return ()
    | otherwise = assertFailure $ unlines $
        "Lists are not equal" :
        [ case d of
            First x  -> "- " ++ show x
            Second y -> "+ " ++ show y
            Both x _ -> "  " ++ show x
        | d <- getDiff xs ys
        ]

-- | Not -W(no-)error= or -W(no-)warn flags.
optionFilter :: String -> Bool
optionFilter x = not $
        "-Werror=" `List.isPrefixOf` x
    || "-Wno-error=" `List.isPrefixOf` x
    || "-Wwarn=" `List.isPrefixOf` x
    || "-Wno-warn=" `List.isPrefixOf` x

-- | Options added in GHC-8.8.
--
-- Generated with the following commands:
-- @
-- $ ghc-8.6.5 --show-options | sort > 8.6.5.txt
-- $ ghc-8.8.4 --show-options | sort > 8.8.4.txt
-- $ diff -u 8.6.5.txt 8.8.4.txt | grep -E "^\+" | sed -E 's/\+(.*)$/, "\1"/'
-- @
options_8_8_all :: [String]
options_8_8_all = filter optionFilter
    [ "-ddump-cfg-weights"
    , "-dno-suppress-stg-exts"
    , "-dsuppress-stg-exts"
    , "-fblock-layout-cfg"
    , "-fblock-layout-weightless"
    , "-fblock-layout-weights"
    , "-fclear-plugins"
    , "-fkeep-cafs"
    , "-fno-block-layout-cfg"
    , "-fno-block-layout-weightless"
    , "-fno-keep-cafs"
    , "-fno-safe-haskell"
    , "-fno-show-docs-of-hole-fits"
    , "-fno-stg-lift-lams"
    , "-fno-stg-lift-lams-known"
    , "-fno-validate-ide-info"
    , "-fno-write-ide-info"
    , "-fshow-docs-of-hole-fits"
    , "-fstg-lift-lams"
    , "-fstg-lift-lams-known"
    , "-fstg-lift-lams-non-rec-args"
    , "-fstg-lift-lams-non-rec-args-any"
    , "-fstg-lift-lams-rec-args"
    , "-fstg-lift-lams-rec-args-any"
    , "-fvalidate-ide-info"
    , "-fwrite-ide-info"
    , "-hiedir"
    , "-hiesuf"
    , "-keep-hscpp-file"
    , "-keep-hscpp-files"
    , "-Werror=missed-extra-shared-lib"
    , "-Werror=missing-deriving-strategies"
    , "-Werror=missing-space-after-bang"
    , "-Wmissed-extra-shared-lib"
    , "-Wmissing-deriving-strategies"
    , "-Wmissing-space-after-bang"
    , "-Wno-error=missed-extra-shared-lib"
    , "-Wno-error=missing-deriving-strategies"
    , "-Wno-error=missing-space-after-bang"
    , "-Wno-missed-extra-shared-lib"
    , "-Wno-missing-deriving-strategies"
    , "-Wno-missing-space-after-bang"
    , "-Wwarn=missed-extra-shared-lib"
    , "-Wwarn=missing-deriving-strategies"
    , "-Wwarn=missing-space-after-bang"
    ]

-- NOTE: Use test difference to find those that may affect artifacts.
options_8_8_affects :: [String]
options_8_8_affects =
    [ "-fblock-layout-cfg"
    , "-fblock-layout-weightless"
    , "-fblock-layout-weights"
    , "-fclear-plugins"
    , "-fkeep-cafs"
    , "-fno-block-layout-cfg"
    , "-fno-block-layout-weightless"
    , "-fno-keep-cafs"
    , "-fno-safe-haskell"
    , "-fno-stg-lift-lams"
    , "-fno-stg-lift-lams-known"
    , "-fno-validate-ide-info"
    , "-fno-write-ide-info"
    , "-fstg-lift-lams"
    , "-fstg-lift-lams-known"
    , "-fstg-lift-lams-non-rec-args"
    , "-fstg-lift-lams-non-rec-args-any"
    , "-fstg-lift-lams-rec-args"
    , "-fstg-lift-lams-rec-args-any"
    , "-fvalidate-ide-info"
    , "-fwrite-ide-info"
    , "-hiedir"
    , "-hiesuf"
    , "-keep-hscpp-file"
    , "-keep-hscpp-files"
    ]

-- | Options added in GHC-8.8.
--
-- Generated with the following commands:
-- @
-- $ ghc-8.8.4 --show-options | sort > 8.8.4.txt
-- $ ghc-8.10.7 --show-options | sort > 8.10.7.txt
-- $ diff -u 8.8.4.txt 8.10.7txt | grep -E "^\+" | sed -E 's/\+(.*)$/, "\1"/'
-- @
options_8_10_all :: [String]
options_8_10_all = filter optionFilter
    [ "-ddump-cmm-verbose-by-proc"
    , "-ddump-stg-final"
    , "-ddump-stg-unarised"
    , "-dno-typeable-binds"
    , "-fbinary-blob-threshold"
    , "-fdefer-diagnostics"
    , "-fenable-th-splice-warnings"
    , "-fkeep-going"
    , "-fmax-pmcheck-models"
    , "-fno-defer-diagnostics"
    , "-fno-enable-th-splice-warnings"
    , "-fno-keep-going"
    , "-fno-print-axiom-incomps"
    , "-fplugin-trustworthy"
    , "-fprint-axiom-incomps"
    , "-include-cpp-deps"
    , "-optcxx"
    , "-optlm"
    , "-pgmc-supports-no-pie"
    , "-pgminstall_name_tool"
    , "-pgmlm"
    , "-pgmotool"
    , "-Wcompat-unqualified-imports"
    , "-Wderiving-defaults"
    , "-Werror=compat-unqualified-imports"
    , "-Werror=deriving-defaults"
    , "-Werror=inferred-safe-imports"
    , "-Werror=missing-safe-haskell-mode"
    , "-Werror=prepositive-qualified-module"
    , "-Werror=redundant-record-wildcards"
    , "-Werror=unused-packages"
    , "-Werror=unused-record-wildcards"
    , "-Winferred-safe-imports"
    , "-Wmissing-safe-haskell-mode"
    , "-Wno-compat-unqualified-imports"
    , "-Wno-deriving-defaults"
    , "-Wno-error=compat-unqualified-imports"
    , "-Wno-error=deriving-defaults"
    , "-Wno-error=inferred-safe-imports"
    , "-Wno-error=missing-safe-haskell-mode"
    , "-Wno-error=prepositive-qualified-module"
    , "-Wno-error=redundant-record-wildcards"
    , "-Wno-error=unused-packages"
    , "-Wno-error=unused-record-wildcards"
    , "-Wno-inferred-safe-imports"
    , "-Wno-missing-safe-haskell-mode"
    , "-Wno-prepositive-qualified-module"
    , "-Wno-redundant-record-wildcards"
    , "-Wno-unused-packages"
    , "-Wno-unused-record-wildcards"
    , "-Wprepositive-qualified-module"
    , "-Wredundant-record-wildcards"
    , "-Wunused-packages"
    , "-Wunused-record-wildcards"
    , "-Wwarn=compat-unqualified-imports"
    , "-Wwarn=deriving-defaults"
    , "-Wwarn=inferred-safe-imports"
    , "-Wwarn=missing-safe-haskell-mode"
    , "-Wwarn=prepositive-qualified-module"
    , "-Wwarn=redundant-record-wildcards"
    , "-Wwarn=unused-packages"
    , "-Wwarn=unused-record-wildcards"
    , "-XCUSKs"
    , "-XImportQualifiedPost"
    , "-XNoCUSKs"
    , "-XNoImportQualifiedPost"
    , "-XNoStandaloneKindSignatures"
    , "-XNoUnliftedNewtypes"
    , "-XStandaloneKindSignatures"
    , "-XUnliftedNewtypes"
    ]

options_8_10_affects :: [String]
options_8_10_affects =
    [ "-dno-typeable-binds"
    , "-fbinary-blob-threshold"
    , "-fmax-pmcheck-models"
    , "-fplugin-trustworthy"
    , "-include-cpp-deps"
    , "-optcxx"
    ]

options_9_0_all :: [String]
options_9_0_all =
    [ "-ddump-cmm-opt"
    , "-ddump-cpranal"
    , "-ddump-cpr-signatures"
    , "-ddump-hie"
    -- NOTE: we filter out -dlinear-core-lint
    -- we filter, -dcore-lint, -dstg-lint etc.
    , "-dlinear-core-lint"
    ] ++ options_9_0_affects

options_9_0_affects :: [String]
options_9_0_affects =
    [ "-fcmm-static-pred"
    ]

options_9_2_all :: [String]
options_9_2_all =
    [
    ] ++ options_9_2_affects

options_9_2_affects :: [String]
options_9_2_affects =
    [
    ]

options_9_4_all :: [String]
options_9_4_all =
    [
    ] ++ options_9_4_affects

options_9_4_affects :: [String]
options_9_4_affects =
    [
    ]

options_9_6_all :: [String]
options_9_6_all =
    [
    ] ++ options_9_6_affects

options_9_6_affects :: [String]
options_9_6_affects =
    [
    ]

options_9_8_all :: [String]
options_9_8_all =
    [
    ] ++ options_9_8_affects

options_9_8_affects :: [String]
options_9_8_affects =
    [
    ]
