{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Tests for the optparse-applicative command parsers wired up in
-- "Distribution.Client.Cmd.UI".
--
-- The optparse-applicative migration reuses the same 'OptionField' definitions
-- as the legacy GetOpt-based parser, so the two parsers should agree on how
-- command-line arguments map to flags. These tests exercise both parsers
-- in-process and compare their results, using the @-j@ / @--jobs@
-- optional-argument option (@-j[NUM]@, @--jobs[=NUM]@) as the archetype.
module UnitTests.Distribution.Client.CmdUI
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Distribution.Client.Cmd.UI
  ( commandParserByName
  , parseCommandWithOptparseMany
  )
import qualified Distribution.Client.CmdBuild as CmdBuild
import Distribution.Client.NixStyleOptions (NixStyleFlags (..))
import Distribution.Client.Setup (InstallFlags (..), globalCommand)
import Distribution.Simple.Command
  ( Command
  , CommandParse (..)
  , CommandUI (..)
  , commandParseArgs
  )
import Distribution.Simple.Setup (Flag, pattern Flag, pattern NoFlag)

tests :: [TestTree]
tests =
  [ testGroup
      "GetOpt and optparse parsers agree (v2-build -j/--jobs)"
      [ testGroup "-j[NUM], --jobs[=NUM]"
        [ testCase (unwords ("build" : args)) $
          viaOptparse args @?= viaGetOpt args
        | args <- argMatrix
        ]
      ]
  , testGroup
      "v2-build -j/--jobs parsed values"
      [ testCase "absent leaves numJobs unset" $
          viaOptparse [] @?= Ready NoFlag []
      , testCase "bare -j means $ncpus (Flag Nothing)" $
          viaOptparse ["-j"] @?= Ready (Flag Nothing) []
      , testCase "-j does not consume a following target" $
          viaOptparse ["-j", "all"] @?= Ready (Flag Nothing) ["all"]
      , testCase "-j4 sets Flag (Just 4)" $
          viaOptparse ["-j4"] @?= Ready (Flag (Just 4)) []
      , testCase "-j4 with a target" $
          viaOptparse ["-j4", "all"] @?= Ready (Flag (Just 4)) ["all"]
      , testCase "--jobs means $ncpus (Flag Nothing)" $
          viaOptparse ["--jobs"] @?= Ready (Flag Nothing) []
      , testCase "--jobs=4 sets Flag (Just 4)" $
          viaOptparse ["--jobs=4"] @?= Ready (Flag (Just 4)) []
      , testCase "--jobs=$ncpus means Flag Nothing" $
          viaOptparse ["--jobs=$ncpus"] @?= Ready (Flag Nothing) []
      , testCase "--jobs=0 is rejected" $
          isError (viaOptparse ["--jobs=0"]) @? "expected a parse error"
      , testCase "-j0 is rejected" $
          isError (viaOptparse ["-j0"]) @? "expected a parse error"
      ]
  ]

-- | The set of argument lists exercised by the agreement tests.
argMatrix :: [[String]]
argMatrix =
  [ []
  , ["-j"]
  , ["-j4"]
  , ["-j", "4"]
  , ["-j", "all"]
  , ["all", "-j"]
  , ["-j4", "all"]
  , ["all", "-j4"]
  , ["--jobs"]
  , ["--jobs=4"]
  , ["--jobs", "4"]
  , ["--jobs=$ncpus"]
  , ["--jobs=0"]
  , ["-j0"]
  ]

-- | A small, comparable summary of a parse outcome, capturing just the parsed
-- @-j@ / @--jobs@ value and the positional targets. This lets us compare the
-- two parsers (and use '@?=') without needing 'Eq'/'Show' on the full flag
-- records.
data ParseSummary
  = Ready (Flag (Maybe Int)) [String]
  | Help
  | List
  | Err
  deriving (Eq, Show)

isError :: ParseSummary -> Bool
isError Err = True
isError _ = False

summarise :: CommandParse (NixStyleFlags CmdBuild.BuildFlags, [String]) -> ParseSummary
summarise = \case
  CommandReadyToGo (flags, targets) ->
    Ready (installNumJobs (installFlags flags)) targets
  CommandHelp _ -> Help
  CommandList _ -> List
  CommandErrors _ -> Err

-- | Parse @cabal build@ arguments via the legacy GetOpt-based parser.
viaGetOpt :: [String] -> ParseSummary
viaGetOpt args =
  summarise $
    case commandParseArgs CmdBuild.buildCommand False args of
      CommandReadyToGo (mkFlags, targets) ->
        CommandReadyToGo (mkFlags (commandDefaultFlags CmdBuild.buildCommand), targets)
      CommandHelp help -> CommandHelp help
      CommandList opts -> CommandList opts
      CommandErrors errs -> CommandErrors errs

-- | Parse @cabal build@ arguments via the new optparse-applicative parser,
-- exercising the same dispatch path as @Main@. The command action is replaced
-- with a tuple constructor so we can inspect the parsed flags and targets
-- directly instead of running the command.
viaOptparse :: [String] -> ParseSummary
viaOptparse args =
  summarise $
    case parseCommandWithOptparseMany (globalCommand noCommands) [buildParser] ("build" : args) of
      Just (CommandReadyToGo (_globalFlags, inner)) -> inner
      Just (CommandErrors errs) -> CommandErrors errs
      Just (CommandHelp mkHelp) -> CommandHelp mkHelp
      Just (CommandList opts) -> CommandList opts
      Nothing -> CommandErrors ["global flag parsing failed or command not found"]
  where
    buildParser =
      commandParserByName CmdBuild.examples CmdBuild.buildCommand (,)

    noCommands :: [Command ()]
    noCommands = []
