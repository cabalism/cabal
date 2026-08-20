{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Tests for the optparse-applicative command parsers wired up in
-- "Distribution.Client.Cmd.UI".
--
-- The optparse-applicative migration reuses the same 'OptionField' definitions
-- as the legacy GetOpt-based parser, so the two parsers should agree on how
-- command-line arguments map to flags. These tests exercise both parsers
-- in-process and compare their results, using the optional-argument options
-- @-j@ / @--jobs@ (@-j[NUM]@, @--jobs[=NUM]@), @-O@ / @--enable-optimization@
-- (@-O[n]@, @--enable-optimization[=n]@) and @-v@ / @--verbose@
-- (@-v[n]@, @--verbose[=n]@) as the archetypes.
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
import Distribution.Simple.Compiler (OptimisationLevel (..))
import Distribution.Simple.Setup
  ( CommonSetupFlags (..)
  , ConfigFlags (..)
  , Flag
  , pattern Flag
  , pattern NoFlag
  )
import Distribution.Verbosity (VerbosityFlags, deafening, normal, silent, verbose)

tests :: [TestTree]
tests =
  [ testGroup
      "GetOpt and optparse parsers agree (v2-build)"
      [ testGroup "-j[NUM], --jobs[=NUM]" $
          agreementCases numJobs jobsMatrix
      , testGroup "-O[n], --enable-optimization[=n]" $
          agreementCases optimisation optMatrix
      , testGroup "-v[n], --verbose[=n]" $
          agreementCases verbosity verbosityMatrix
      ]
  , testGroup
      "v2-build -j/--jobs parsed values"
      [ testCase "absent leaves numJobs unset" $
          viaOptparse numJobs [] @?= Ready NoFlag []
      , testCase "bare -j means $ncpus (Flag Nothing)" $
          viaOptparse numJobs ["-j"] @?= Ready (Flag Nothing) []
      , testCase "-j does not consume a following target" $
          viaOptparse numJobs ["-j", "all"] @?= Ready (Flag Nothing) ["all"]
      , testCase "-j4 sets Flag (Just 4)" $
          viaOptparse numJobs ["-j4"] @?= Ready (Flag (Just 4)) []
      , testCase "-j4 with a target" $
          viaOptparse numJobs ["-j4", "all"] @?= Ready (Flag (Just 4)) ["all"]
      , testCase "--jobs means $ncpus (Flag Nothing)" $
          viaOptparse numJobs ["--jobs"] @?= Ready (Flag Nothing) []
      , testCase "--jobs=4 sets Flag (Just 4)" $
          viaOptparse numJobs ["--jobs=4"] @?= Ready (Flag (Just 4)) []
      , testCase "--jobs=$ncpus means Flag Nothing" $
          viaOptparse numJobs ["--jobs=$ncpus"] @?= Ready (Flag Nothing) []
      , testCase "--jobs=0 is rejected" $
          isError (viaOptparse numJobs ["--jobs=0"]) @? "expected a parse error"
      , testCase "-j0 is rejected" $
          isError (viaOptparse numJobs ["-j0"]) @? "expected a parse error"
      ]
  , testGroup
      "v2-build -O/--enable-optimization parsed values"
      [ testCase "absent leaves optimization unset" $
          viaOptparse optimisation [] @?= Ready NoFlag []
      , testCase "bare -O means normal optimisation" $
          viaOptparse optimisation ["-O"] @?= Ready (Flag NormalOptimisation) []
      , testCase "-O does not consume a following target" $
          viaOptparse optimisation ["-O", "all"] @?= Ready (Flag NormalOptimisation) ["all"]
      , testCase "-O0 disables optimisation" $
          viaOptparse optimisation ["-O0"] @?= Ready (Flag NoOptimisation) []
      , testCase "-O2 sets maximum optimisation" $
          viaOptparse optimisation ["-O2"] @?= Ready (Flag MaximumOptimisation) []
      , testCase "-O2 with a target" $
          viaOptparse optimisation ["-O2", "all"] @?= Ready (Flag MaximumOptimisation) ["all"]
      , testCase "--enable-optimization means normal optimisation" $
          viaOptparse optimisation ["--enable-optimization"] @?= Ready (Flag NormalOptimisation) []
      , testCase "--enable-optimization=0 disables optimisation" $
          viaOptparse optimisation ["--enable-optimization=0"] @?= Ready (Flag NoOptimisation) []
      , testCase "--enable-optimization=2 sets maximum optimisation" $
          viaOptparse optimisation ["--enable-optimization=2"] @?= Ready (Flag MaximumOptimisation) []
      , testCase "--disable-optimization disables optimisation" $
          viaOptparse optimisation ["--disable-optimization"] @?= Ready (Flag NoOptimisation) []
      ]
  , testGroup
      "v2-build -v/--verbose parsed values"
      [ testCase "absent leaves verbosity unset" $
          viaOptparse verbosity [] @?= Ready NoFlag []
      , testCase "bare -v means verbose (level 2)" $
          viaOptparse verbosity ["-v"] @?= Ready (Flag verbose) []
      , testCase "-v does not consume a following target" $
          viaOptparse verbosity ["-v", "all"] @?= Ready (Flag verbose) ["all"]
      , testCase "-v0 is silent" $
          viaOptparse verbosity ["-v0"] @?= Ready (Flag silent) []
      , testCase "-v1 is normal" $
          viaOptparse verbosity ["-v1"] @?= Ready (Flag normal) []
      , testCase "-v2 is verbose" $
          viaOptparse verbosity ["-v2"] @?= Ready (Flag verbose) []
      , testCase "-v3 is deafening" $
          viaOptparse verbosity ["-v3"] @?= Ready (Flag deafening) []
      , testCase "--verbose means verbose" $
          viaOptparse verbosity ["--verbose"] @?= Ready (Flag verbose) []
      , testCase "--verbose=0 is silent" $
          viaOptparse verbosity ["--verbose=0"] @?= Ready (Flag silent) []
      ]
  ]

-- | Build agreement test cases: for each argument list, the optparse parser and
-- the legacy GetOpt parser must produce the same summary.
agreementCases
  :: (Eq a, Show a)
  => (NixStyleFlags CmdBuild.BuildFlags -> a)
  -> [[String]]
  -> [TestTree]
agreementCases extract matrix =
  [ testCase (unwords ("build" : args)) $
      viaOptparse extract args @?= viaGetOpt extract args
  | args <- matrix
  ]

-- | The argument lists exercised by the @-j@ / @--jobs@ agreement tests.
jobsMatrix :: [[String]]
jobsMatrix =
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

-- | The argument lists exercised by the @-O@ / @--enable-optimization@
-- agreement tests.
optMatrix :: [[String]]
optMatrix =
  [ []
  , ["-O"]
  , ["-O0"]
  , ["-O1"]
  , ["-O2"]
  , ["-O", "2"]
  , ["-O", "all"]
  , ["all", "-O"]
  , ["-O2", "all"]
  , ["all", "-O2"]
  , ["--enable-optimization"]
  , ["--enable-optimization=0"]
  , ["--enable-optimization=2"]
  , ["--enable-optimization", "all"]
  , ["--disable-optimization"]
  ]

-- | The argument lists exercised by the @-v@ / @--verbose@ agreement tests.
verbosityMatrix :: [[String]]
verbosityMatrix =
  [ []
  , ["-v"]
  , ["-v0"]
  , ["-v1"]
  , ["-v2"]
  , ["-v3"]
  , ["-v", "2"]
  , ["-v", "all"]
  , ["all", "-v"]
  , ["-v2", "all"]
  , ["all", "-v2"]
  , ["--verbose"]
  , ["--verbose=0"]
  , ["--verbose=2"]
  , ["--verbose", "all"]
  ]

-- | Extract the parsed @-j@ / @--jobs@ value.
numJobs :: NixStyleFlags CmdBuild.BuildFlags -> Flag (Maybe Int)
numJobs = installNumJobs . installFlags

-- | Extract the parsed @-O@ / @--enable-optimization@ value.
optimisation :: NixStyleFlags CmdBuild.BuildFlags -> Flag OptimisationLevel
optimisation = configOptimization . configFlags

-- | Extract the parsed @-v@ / @--verbose@ value.
verbosity :: NixStyleFlags CmdBuild.BuildFlags -> Flag VerbosityFlags
verbosity = setupVerbosity . configCommonFlags . configFlags

-- | A small, comparable summary of a parse outcome, capturing just the value of
-- interest and the positional targets. This lets us compare the two parsers
-- (and use '@?=') without needing 'Eq'/'Show' on the full flag records.
data ParseSummary a
  = Ready a [String]
  | Help
  | List
  | Err
  deriving (Eq, Show)

isError :: ParseSummary a -> Bool
isError Err = True
isError _ = False

summarise
  :: (NixStyleFlags CmdBuild.BuildFlags -> a)
  -> CommandParse (NixStyleFlags CmdBuild.BuildFlags, [String])
  -> ParseSummary a
summarise extract = \case
  CommandReadyToGo (flags, targets) -> Ready (extract flags) targets
  CommandHelp _ -> Help
  CommandList _ -> List
  CommandErrors _ -> Err

-- | Parse @cabal build@ arguments via the legacy GetOpt-based parser.
viaGetOpt
  :: (NixStyleFlags CmdBuild.BuildFlags -> a)
  -> [String]
  -> ParseSummary a
viaGetOpt extract args =
  summarise extract $
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
viaOptparse
  :: (NixStyleFlags CmdBuild.BuildFlags -> a)
  -> [String]
  -> ParseSummary a
viaOptparse extract args =
  summarise extract $
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
