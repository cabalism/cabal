{-# LANGUAGE CPP #-}
module Main
    ( main
    ) where

import Prelude ()
import Prelude.Compat

import Test.Tasty
import Test.Tasty.Golden.Advanced (goldenTest)
import Test.Tasty.HUnit

import Control.Monad                               (unless, void)
import Data.Algorithm.Diff                         (PolyDiff (..), getGroupedDiff)
import Data.Maybe                                  (isNothing)
import Distribution.Fields                         (pwarning)
import Distribution.PackageDescription             (GenericPackageDescription)
import Distribution.PackageDescription.Parsec      (parseGenericPackageDescription)
import Distribution.PackageDescription.PrettyPrint (showGenericPackageDescription)
import Distribution.Parsec                         (PWarnType (..), PWarning (..), showPErrorWithSource, showPWarningWithSource)
import Distribution.Pretty                         (prettyShow)
import Distribution.Fields.ParseResult
import Distribution.Utils.Generic                  (fromUTF8BS, toUTF8BS)
import System.Directory                            (setCurrentDirectory)
import System.Environment                          (getArgs, withArgs)
import System.FilePath                             (replaceExtension, (</>))
import Distribution.Parsec.Source

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List.NonEmpty    as NE

import qualified Distribution.InstalledPackageInfo as IPI

#ifdef MIN_VERSION_tree_diff
import Data.TreeDiff                 (ansiWlEditExpr, ediff, toExpr)
import Data.TreeDiff.Golden          (ediffGolden)
import Data.TreeDiff.Instances.Cabal ()
#endif

tests :: TestTree
tests = testGroup "parsec tests"
    [ regressionTests
    , warningTests
    , errorTests
    , ipiTests
    ]

-------------------------------------------------------------------------------
-- Warnings
-------------------------------------------------------------------------------

-- Verify that we trigger warnings
warningTests :: TestTree
warningTests = testGroup "warnings triggered"
    [ warningTest PWTLexBOM            "bom.cabal"
    , warningTest PWTLexNBSP           "nbsp.cabal"
    , warningTest PWTLexTab            "tab.cabal"
    , warningTest PWTUTF               "utf8.cabal"
    , warningTest PWTBoolCase          "bool.cabal"
    , warningTest PWTVersionTag        "versiontag.cabal"
    , warningTest PWTNewSyntax         "newsyntax.cabal"
    , warningTest PWTOldSyntax         "oldsyntax.cabal"
    , warningTest PWTDeprecatedField   "deprecatedfield.cabal"
    , warningTest PWTInvalidSubsection "subsection.cabal"
    , warningTest PWTUnknownField      "unknownfield.cabal"
    , warningTest PWTUnknownSection    "unknownsection.cabal"
    , warningTest PWTTrailingFields    "trailingfield.cabal"
    , warningTest PWTDoubleDash        "doubledash.cabal"
    , warningTest PWTMultipleSingularField "multiplesingular.cabal"
    , warningTest PWTVersionWildcard   "wildcard.cabal"
    , warningTest PWTVersionOperator   "operator.cabal"
    , warningTest PWTSpecVersion       "specversion-a.cabal"
    , warningTest PWTSpecVersion       "specversion-b.cabal"
    , warningTest PWTSpecVersion       "specversion-c.cabal"
    -- TODO: not implemented yet
    -- , warningTest PWTExtraTestModule   "extratestmodule.cabal"
    ]

warningTest :: PWarnType -> FilePath -> TestTree
warningTest wt fp = testCase (show wt) $ do
    contents <- BS.readFile $ "tests" </> "ParserTests" </> "warnings" </> fp

    let res =  withSource (PCabalFile (fp, contents)) $ parseGenericPackageDescription contents
    let (warns, x) = runParseResult res

    assertBool ("should parse successfully: " ++ show x) $ isRight x

    case map pwarning warns of
        [PWarning wt' _ _] -> assertEqual "warning type" wt wt'
        []                 -> assertFailure "got no warnings"
        _                  -> assertFailure $ "got multiple warnings: " ++ show warns
  where
    isRight (Right _) = True
    isRight _         = False

-------------------------------------------------------------------------------
-- Errors
-------------------------------------------------------------------------------

errorTests :: TestTree
errorTests = testGroup "errors"
    [ errorTest "common1.cabal"
    , errorTest "common2.cabal"
    , errorTest "common3.cabal"
    , errorTest "leading-comma.cabal"
    , errorTest "leading-comma-2.cabal"
    , errorTest "leading-comma-2b.cabal"
    , errorTest "leading-comma-2c.cabal"
    , errorTest "range-ge-wild.cabal"
    , errorTest "forward-compat.cabal"
    , errorTest "forward-compat2.cabal"
    , errorTest "forward-compat3.cabal"
    , errorTest "issue-5055.cabal"
    , errorTest "issue-5055-2.cabal"
    , errorTest "noVersion.cabal"
    , errorTest "noVersion2.cabal"
    , errorTest "multiple-libs.cabal"
    , errorTest "spdx-1.cabal"
    , errorTest "spdx-2.cabal"
    , errorTest "spdx-3.cabal"
    , errorTest "removed-fields.cabal"
    , errorTest "version-sets-1.cabal"
    , errorTest "version-sets-2.cabal"
    , errorTest "version-sets-3.cabal"
    , errorTest "version-sets-4.cabal"
    , errorTest "undefined-flag.cabal"
    , errorTest "mixin-1.cabal"
    , errorTest "mixin-2.cabal"
    , errorTest "libpq1.cabal"
    , errorTest "libpq2.cabal"
    , errorTest "MiniAgda.cabal"
    , errorTest "big-version.cabal"
    , errorTest "anynone.cabal"
    ]

errorTest :: FilePath -> TestTree
errorTest fp = cabalGoldenTest fp correct $ do
    contents <- BS.readFile input
    let res =  withSource (PCabalFile (fp, contents)) $ parseGenericPackageDescription contents
    let (_, x) = runParseResult res

    return $ toUTF8BS $ case x of
        Right gpd ->
            "UNEXPECTED SUCCESS\n" ++
            showGenericPackageDescription gpd
        Left (v, errs) ->
            unlines $ ("VERSION: " ++ show v) : map (showPErrorWithSource . fmap renderCabalFileSource) (NE.toList errs)
  where
    input = "tests" </> "ParserTests" </> "errors" </> fp
    correct = replaceExtension input "errors"

-------------------------------------------------------------------------------
-- Regressions
-------------------------------------------------------------------------------

regressionTests :: TestTree
regressionTests = testGroup "regressions"
    [ regressionTest "encoding-0.8.cabal"
    , regressionTest "Octree-0.5.cabal"
    , regressionTest "nothing-unicode.cabal"
    , regressionTest "multiple-libs-2.cabal"
    , regressionTest "issue-774.cabal"
    , regressionTest "generics-sop.cabal"
    , regressionTest "elif.cabal"
    , regressionTest "elif2.cabal"
    , regressionTest "shake.cabal"
    , regressionTest "common.cabal"
    , regressionTest "common2.cabal"
    , regressionTest "common3.cabal"
    , regressionTest "common-conditional.cabal"
    , regressionTest "leading-comma.cabal"
    , regressionTest "leading-comma-2.cabal"
    , regressionTest "wl-pprint-indef.cabal"
    , regressionTest "th-lift-instances.cabal"
    , regressionTest "issue-5055.cabal"
    , regressionTest "issue-6083-pkg-pkg.cabal"
    , regressionTest "issue-6083-a.cabal"
    , regressionTest "issue-6083-b.cabal"
    , regressionTest "issue-6083-c.cabal"
    , regressionTest "noVersion.cabal"
    , regressionTest "spdx-1.cabal"
    , regressionTest "spdx-2.cabal"
    , regressionTest "spdx-3.cabal"
    , regressionTest "hidden-main-lib.cabal"
    , regressionTest "jaeger-flamegraph.cabal"
    , regressionTest "version-sets.cabal"
    , regressionTest "mixin-1.cabal"
    , regressionTest "mixin-2.cabal"
    , regressionTest "mixin-3.cabal"
    , regressionTest "libpq1.cabal"
    , regressionTest "libpq2.cabal"
    , regressionTest "issue-5846.cabal"
    , regressionTest "indentation.cabal"
    , regressionTest "indentation2.cabal"
    , regressionTest "indentation3.cabal"
    , regressionTest "big-version.cabal"
    , regressionTest "anynone.cabal"
    , regressionTest "monad-param.cabal"
    , regressionTest "hasktorch.cabal"
    ]

regressionTest :: FilePath -> TestTree
regressionTest fp = testGroup fp
{- FOURMOLU_DISABLE -}
    [ formatGoldenTest fp
    , formatRoundTripTest fp
#ifdef MIN_VERSION_tree_diff
    , treeDiffGoldenTest fp
#endif
    ]
{- FOURMOLU_ENABLE -}

formatGoldenTest :: FilePath -> TestTree
formatGoldenTest fp = cabalGoldenTest "format" correct $ do
    contents <- BS.readFile input
    let res = withSource (PCabalFile (fp, contents)) $ parseGenericPackageDescription contents
    let (warns, x) = runParseResult res

    return $ toUTF8BS $ case x of
        Right gpd ->
            unlines (map (showPWarningWithSource . fmap renderCabalFileSource) warns)
            ++ showGenericPackageDescription gpd
        Left (csv, errs) ->
            unlines $
                "ERROR" :
                maybe "unknown-version" prettyShow csv :
                map (showPErrorWithSource . fmap renderCabalFileSource) (NE.toList errs)
  where
    input = "tests" </> "ParserTests" </> "regressions" </> fp
    correct = replaceExtension input "format"

#ifdef MIN_VERSION_tree_diff
treeDiffGoldenTest :: FilePath -> TestTree
treeDiffGoldenTest fp = ediffGolden goldenTest "expr" exprFile $ do
    contents <- BS.readFile input
    let res = withSource (PCabalFile (fp, contents)) $ parseGenericPackageDescription contents
    let (_, x) = runParseResult res
    case x of
        Right gpd      -> pure (toExpr gpd)
        Left (_, errs) -> fail $ unlines $ "ERROR" : map (showPErrorWithSource . fmap renderCabalFileSource) (NE.toList errs)
  where
    input = "tests" </> "ParserTests" </> "regressions" </> fp
    exprFile = replaceExtension input "expr"
#endif

formatRoundTripTest :: FilePath -> TestTree
formatRoundTripTest fp = testCase "roundtrip" $ do
    contents <- BS.readFile input
    x <- parse contents
    let contents' = showGenericPackageDescription x
    y <- parse (toUTF8BS contents')
    -- previously we mangled licenses a bit
    let y' = y
{- FOURMOLU_DISABLE -}
    unless (x == y') $
#ifdef MIN_VERSION_tree_diff
        assertFailure $ unlines
            [ "re-parsed doesn't match"
            , show $ ansiWlEditExpr $ ediff x y
            ]
#else
        assertFailure $ unlines
            [ "re-parsed doesn't match"
            , "expected"
            , show x
            , "actual"
            , show y
            ]
#endif
  where
    parse :: BS.ByteString -> IO GenericPackageDescription
    parse c = do
        let (_, x') = runParseResult $ withSource (PCabalFile (fp, c)) $ parseGenericPackageDescription c
        case x' of
            Right gpd      -> pure gpd
            Left (_, errs) -> do
                void $ assertFailure $ unlines (map (showPErrorWithSource . fmap renderCabalFileSource) $ NE.toList errs)
                fail "failure"
    input = "tests" </> "ParserTests" </> "regressions" </> fp
{- FOURMOLU_ENABLE -}

-------------------------------------------------------------------------------
-- InstalledPackageInfo regressions
-------------------------------------------------------------------------------

ipiTests :: TestTree
ipiTests = testGroup "ipis"
    [ ipiTest "transformers.cabal"
    , ipiTest "Includes2.cabal"
    , ipiTest "issue-2276-ghc-9885.cabal"
    , ipiTest "internal-preprocessor-test.cabal"
    ]

ipiTest :: FilePath -> TestTree
{- FOURMOLU_DISABLE -}
ipiTest fp = testGroup fp $
#ifdef MIN_VERSION_tree_diff
    [ ipiTreeDiffGoldenTest fp ] ++
#endif
    [ ipiFormatGoldenTest fp
    , ipiFormatRoundTripTest fp
    ]
{- FOURMOLU_ENABLE -}

ipiFormatGoldenTest :: FilePath -> TestTree
ipiFormatGoldenTest fp = cabalGoldenTest "format" correct $ do
    contents <- BS.readFile input
    let res = IPI.parseInstalledPackageInfo contents
    return $ toUTF8BS $ case res of
        Left err -> "ERROR " ++ show err
        Right (ws, ipi)  ->
            unlines ws ++ IPI.showInstalledPackageInfo ipi
  where
    input = "tests" </> "ParserTests" </> "ipi" </> fp
    correct = replaceExtension input "format"

#ifdef MIN_VERSION_tree_diff
ipiTreeDiffGoldenTest :: FilePath -> TestTree
ipiTreeDiffGoldenTest fp = ediffGolden goldenTest "expr" exprFile $ do
    contents <- BS.readFile input
    let res = IPI.parseInstalledPackageInfo contents
    case res of
        Left err -> fail $ "ERROR " ++ show err
        Right (_ws, ipi) -> pure (toExpr ipi)
  where
    input = "tests" </> "ParserTests" </> "ipi" </> fp
    exprFile = replaceExtension input "expr"
#endif

ipiFormatRoundTripTest :: FilePath -> TestTree
ipiFormatRoundTripTest fp = testCase "roundtrip" $ do
    contents <- BS.readFile input
    x <- parse contents
    let contents' = IPI.showInstalledPackageInfo x
    y <- parse (toUTF8BS contents')

    -- ghc-pkg prints pkgroot itself, based on cli arguments!
    let x' = x { IPI.pkgRoot = Nothing }
    let y' = y
    assertBool "pkgRoot isn't shown" (isNothing (IPI.pkgRoot y))
    assertEqual "re-parsed doesn't match" x' y'

    -- Complete round-trip
    let contents2 = IPI.showFullInstalledPackageInfo x
    z <- parse (toUTF8BS contents2)
    assertEqual "re-parsed doesn't match" x z

  where
    parse :: BS.ByteString -> IO IPI.InstalledPackageInfo
    parse c = do
        case IPI.parseInstalledPackageInfo c of
            Right (_, ipi) -> return ipi
            Left err       -> do
              void $ assertFailure $ show err
              fail "failure"
    input = "tests" </> "ParserTests" </> "ipi" </> fp

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("--cwd" : cwd : args') -> do
            setCurrentDirectory cwd
            withArgs args' $ defaultMain tests
        _ -> defaultMain tests

cabalGoldenTest :: TestName -> FilePath -> IO BS.ByteString -> TestTree
cabalGoldenTest name ref act = goldenTest name (BS.readFile ref) act cmp upd
  where
    upd = BS.writeFile ref
    cmp x y | x == y = return Nothing
    cmp x y = return $ Just $ unlines $
        concatMap f (getGroupedDiff (BS8.lines x) (BS8.lines y))
      where
        f (First xs)  = map (cons3 '-' . fromUTF8BS) xs
        f (Second ys) = map (cons3 '+' . fromUTF8BS) ys
        -- we print unchanged lines too. It shouldn't be a problem while we have
        -- reasonably small examples
        f (Both xs _) = map (cons3 ' ' . fromUTF8BS) xs
        -- we add three characters, so the changed lines are easier to spot
        cons3 c cs = c : c : c : ' ' : cs
