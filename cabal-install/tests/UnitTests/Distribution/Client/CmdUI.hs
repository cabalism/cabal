{-# LANGUAGE PatternSynonyms #-}

-- | Tests for the optparse-applicative command parsers wired up in
-- "Distribution.Client.Cmd.UI".
--
-- These exercise the same parsing path used by @Main@ (global-flag parsing
-- followed by a command's optparse parser) and assert on the resulting
-- 'NixStyleFlags', rather than only checking rendered @--help@ output.
module UnitTests.Distribution.Client.CmdUI
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.ExpectedFailure (expectFailBecause)
import Test.Tasty.HUnit

import qualified Distribution.Client.CmdBuild as CmdBuild
import Distribution.Client.Cmd.UI
  ( commandParserByName
  , parseCommandWithOptparseMany
  )
import Distribution.Client.NixStyleOptions (NixStyleFlags (..))
import Distribution.Client.Setup (InstallFlags (..), globalCommand)
import Distribution.Simple.Command (Command, CommandParse (..))
import Distribution.Simple.Setup (Flag, pattern Flag, pattern NoFlag)

tests :: [TestTree]
tests =
  [ testGroup
      "v2-build -j/--jobs parsing"
      [ testCase "absent leaves numJobs unset" $
          parsedNumJobs [] @?= ReadyTo NoFlag
      , testCase "-j4 sets Flag (Just 4)" $
          parsedNumJobs ["-j4"] @?= ReadyTo (Flag (Just 4))
      , testCase "--jobs=4 sets Flag (Just 4)" $
          parsedNumJobs ["--jobs=4"] @?= ReadyTo (Flag (Just 4))
      , testCase "--jobs=$ncpus means Flag Nothing" $
          parsedNumJobs ["--jobs=$ncpus"] @?= ReadyTo (Flag Nothing)
      , testCase "--jobs=0 is rejected" $
          isError (parsedNumJobs ["--jobs=0"]) @? "expected a parse error"
      , -- Known regression in the optparse-applicative path: '-j'/'--jobs'
        -- is an *optional-argument* option, but the parser in
        -- "Distribution.Client.Cmd.UI" models it with 'Options.Applicative.option',
        -- which unconditionally consumes the following token (and hard-fails
        -- on a bare '-j'). The GetOpt-based path (e.g. cabal-3.18) accepts
        -- these. Remove the 'expectFailBecause' wrappers once the parser
        -- treats the argument as optional again.
        expectFailBecause "optparse path swallows the next token after -j" $
          testGroup
            "optional argument for -j"
            [ testCase "bare -j means $ncpus (Flag Nothing)" $
                parsedNumJobs ["-j"] @?= ReadyTo (Flag Nothing)
            , testCase "-j does not consume a following target" $
                parsedNumJobsAndTargets ["-j", "all"]
                  @?= ReadyTo (Flag Nothing, ["all"])
            ]
      ]
  ]

-- | A small, comparable summary of a 'CommandParse' outcome so tests can use
-- '@?=' without needing 'Eq'/'Show' on the full flag records.
data ParseResult a
  = ReadyTo a
  | GotHelp
  | GotList
  | GotError
  deriving (Eq, Show)

isError :: ParseResult a -> Bool
isError GotError = True
isError _ = False

-- | Run the real @cabal build@ parser and report the parsed value of the
-- @-j@/@--jobs@ flag.
parsedNumJobs :: [String] -> ParseResult (Flag (Maybe Int))
parsedNumJobs args =
  case runBuildParser args of
    CommandReadyToGo (flags, _targets) ->
      ReadyTo (installNumJobs (installFlags flags))
    CommandHelp _ -> GotHelp
    CommandList _ -> GotList
    CommandErrors _ -> GotError

-- | Like 'parsedNumJobs', but also returns the parsed positional targets, so we
-- can check that @-j@ does not steal a following target argument.
parsedNumJobsAndTargets :: [String] -> ParseResult (Flag (Maybe Int), [String])
parsedNumJobsAndTargets args =
  case runBuildParser args of
    CommandReadyToGo (flags, targets) ->
      ReadyTo (installNumJobs (installFlags flags), targets)
    CommandHelp _ -> GotHelp
    CommandList _ -> GotList
    CommandErrors _ -> GotError

-- | Feed arguments through the same command-dispatch path as @Main@. The
-- command action is replaced with a tuple constructor so we can inspect the
-- parsed flags and targets directly instead of running the command.
runBuildParser
  :: [String]
  -> CommandParse (NixStyleFlags CmdBuild.BuildFlags, [String])
runBuildParser args =
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
