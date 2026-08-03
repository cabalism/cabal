{-# LANGUAGE LambdaCase #-}

module UnitTests.Distribution.Client.CmdBuildOptions (tests) where

import Distribution.Client.CmdBuild (buildCommand, parseBuildCommand)
import Distribution.Simple.Command
  ( CommandParse (..)
  , CommandUI (..)
  , OptDescr (..)
  , OptionField (..)
  , ShowOrParseArgs (ParseArgs)
  )

import Data.Char (toLower)
import Data.List (isInfixOf)
import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testCase "build optparse parser recognizes options advertised by CommandUI(ParseArgs)" $ do
      let probes = commandOptionProbes buildCommand
          failures =
            [ (probeDisplay probe, unlines errs)
            | probe <- probes
            , CommandErrors errs <- [parseBuildCommand "build" (probeArgv probe)]
            , any isUnrecognizedError errs
            ]

      assertBool
        ( "Unrecognized options found:\n"
            <> unlines
              [ "  " <> opt <> "\n" <> indent err
              | (opt, err) <- failures
              ]
        )
        (null failures)
  ]

type OptionProbe = (String, [String])

probeDisplay :: OptionProbe -> String
probeDisplay (display, _) = display

probeArgv :: OptionProbe -> [String]
probeArgv (_, argv) = argv

commandOptionProbes :: CommandUI flags -> [OptionProbe]
commandOptionProbes cmd =
  concatMap optionFieldProbes (commandOptions cmd ParseArgs)

optionFieldProbes :: OptionField flags -> [OptionProbe]
optionFieldProbes (OptionField _ descrs) = concatMap optDescrProbes descrs

optDescrProbes :: OptDescr flags -> [OptionProbe]
optDescrProbes = \case
  ReqArg _ (shortFlags, longFlags) _ _ _ ->
    requiredArgProbes shortFlags longFlags
  OptArg _ (shortFlags, longFlags) _ _ _ _ ->
    noArgProbes shortFlags longFlags
  ChoiceOpt choices ->
    concatMap (\(_, (shortFlags, longFlags), _, _) -> noArgProbes shortFlags longFlags) choices
  BoolOpt _ (shortTrue, longTrue) (shortFalse, longFalse) _ _ ->
    noArgProbes shortTrue longTrue <> noArgProbes shortFalse longFalse

requiredArgProbes :: [Char] -> [String] -> [OptionProbe]
requiredArgProbes shortFlags longFlags =
  [ ("--" <> longFlag, ["--" <> longFlag])
  | longFlag <- longFlags
  ]
    <> [ ("-" <> [shortFlag], ["-" <> [shortFlag]])
       | shortFlag <- shortFlags
       ]

noArgProbes :: [Char] -> [String] -> [OptionProbe]
noArgProbes shortFlags longFlags =
  [ ("--" <> longFlag, ["--" <> longFlag])
  | longFlag <- longFlags
  ]
    <> [ ("-" <> [shortFlag], ["-" <> [shortFlag]])
       | shortFlag <- shortFlags
       ]

isUnrecognizedError :: String -> Bool
isUnrecognizedError msg =
  let lower = map toLower msg
   in "invalid option" `isInfixOf` lower
        || "unrecognized" `isInfixOf` lower

indent :: String -> String
indent = unlines . map ("    " <>) . lines
