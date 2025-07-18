{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      :  Distribution.Simple.Setup.Repl
-- Copyright   :  Isaac Jones 2003-2004
--                Duncan Coutts 2007
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Definition of the repl command-line options.
-- See: @Distribution.Simple.Setup@
module Distribution.Simple.Setup.Repl
  ( ReplFlags
      ( ReplCommonFlags
      , replVerbosity
      , replDistPref
      , replCabalFilePath
      , replWorkingDir
      , replTargets
      , ..
      )
  , defaultReplFlags
  , replCommand
  , ReplOptions (..)
  , replOptions
  ) where

import Distribution.Compat.Prelude hiding (get)
import Prelude ()

import Distribution.ReadE
import Distribution.Simple.Command hiding (boolOpt, boolOpt')
import Distribution.Simple.Flag
import Distribution.Simple.Program
import Distribution.Simple.Setup.Common
import Distribution.Simple.Utils
import Distribution.Utils.Path
import Distribution.Verbosity

-- ------------------------------------------------------------

-- * REPL Flags

-- ------------------------------------------------------------

data ReplOptions = ReplOptions
  { replOptionsFlags :: [String]
  , replOptionsNoLoad :: Flag Bool
  , replOptionsFlagOutput :: Flag FilePath
  , replWithRepl :: Flag FilePath
  }
  deriving (Show, Generic)

pattern ReplCommonFlags
  :: Flag Verbosity
  -> Flag (SymbolicPath Pkg (Dir Dist))
  -> Flag (SymbolicPath CWD (Dir Pkg))
  -> Flag (SymbolicPath Pkg File)
  -> [String]
  -> ReplFlags
pattern ReplCommonFlags
  { replVerbosity
  , replDistPref
  , replWorkingDir
  , replCabalFilePath
  , replTargets
  } <-
  ( replCommonFlags ->
      CommonSetupFlags
        { setupVerbosity = replVerbosity
        , setupDistPref = replDistPref
        , setupWorkingDir = replWorkingDir
        , setupCabalFilePath = replCabalFilePath
        , setupTargets = replTargets
        }
    )

instance Binary ReplOptions
instance Structured ReplOptions

instance Monoid ReplOptions where
  mempty = ReplOptions mempty (Flag False) NoFlag NoFlag
  mappend = (<>)

instance Semigroup ReplOptions where
  (<>) = gmappend

data ReplFlags = ReplFlags
  { replCommonFlags :: !CommonSetupFlags
  , replProgramPaths :: [(String, FilePath)]
  , replProgramArgs :: [(String, [String])]
  , replReload :: Flag Bool
  , replReplOptions :: ReplOptions
  }
  deriving (Show, Generic)

instance Binary ReplFlags
instance Structured ReplFlags

defaultReplFlags :: ReplFlags
defaultReplFlags =
  ReplFlags
    { replCommonFlags = defaultCommonSetupFlags
    , replProgramPaths = mempty
    , replProgramArgs = []
    , replReload = Flag False
    , replReplOptions = mempty
    }

instance Monoid ReplFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup ReplFlags where
  (<>) = gmappend

replCommand :: ProgramDb -> CommandUI ReplFlags
replCommand progDb =
  CommandUI
    { commandName = "repl"
    , commandSynopsis =
        "Open an interpreter session for the given component."
    , commandDescription = Just $ \pname ->
        wrapText $
          "If the current directory contains no package, ignores COMPONENT "
            ++ "parameters and opens an interactive interpreter session.\n"
            ++ "\n"
            ++ "Otherwise, (re)configures with the given or default flags, and "
            ++ "loads the interpreter with the relevant modules. For executables, "
            ++ "tests and benchmarks, loads the main module (and its "
            ++ "dependencies); for libraries all exposed/other modules.\n"
            ++ "\n"
            ++ "The default component is the library itself, or the executable "
            ++ "if that is the only component.\n"
            ++ "\n"
            ++ "Support for loading specific modules is planned but not "
            ++ "implemented yet. For certain scenarios, `"
            ++ pname
            ++ " exec -- ghci :l Foo` may be used instead. Note that `exec` will "
            ++ "not (re)configure and you will have to specify the location of "
            ++ "other modules, if required.\n"
    , commandNotes = Just $ \pname ->
        "Examples:\n"
          ++ "  "
          ++ pname
          ++ " repl           "
          ++ "    The first component in the package\n"
          ++ "  "
          ++ pname
          ++ " repl foo       "
          ++ "    A named component (i.e. lib, exe, test suite)\n"
          ++ "  "
          ++ pname
          ++ " repl --repl-options=\"-lstdc++\""
          ++ "  Specifying flags for interpreter\n"
    , -- TODO: re-enable once we have support for module/file targets
      --        ++ "  " ++ pname ++ " repl Foo.Bar   "
      --        ++ "    A module\n"
      --        ++ "  " ++ pname ++ " repl Foo/Bar.hs"
      --        ++ "    A file\n\n"
      --        ++ "If a target is ambiguous it can be qualified with the component "
      --        ++ "name, e.g.\n"
      --        ++ "  " ++ pname ++ " repl foo:Foo.Bar\n"
      --        ++ "  " ++ pname ++ " repl testsuite1:Foo/Bar.hs\n"
      commandUsage = \pname -> "Usage: " ++ pname ++ " repl [COMPONENT] [FLAGS]\n"
    , commandDefaultFlags = defaultReplFlags
    , commandOptions = \showOrParseArgs ->
        withCommonSetupOptions
          replCommonFlags
          (\c f -> f{replCommonFlags = c})
          showOrParseArgs
          $ programDbPaths
            progDb
            showOrParseArgs
            replProgramPaths
            (\v flags -> flags{replProgramPaths = v})
            ++ programDbOption
              progDb
              showOrParseArgs
              replProgramArgs
              (\v flags -> flags{replProgramArgs = v})
            ++ programDbOptions
              progDb
              showOrParseArgs
              replProgramArgs
              (\v flags -> flags{replProgramArgs = v})
            ++ case showOrParseArgs of
              ParseArgs ->
                [ option
                    ""
                    ["reload"]
                    "Used from within an interpreter to update files."
                    replReload
                    (\v flags -> flags{replReload = v})
                    trueArg
                ]
              _ -> []
            ++ map liftReplOption (replOptions showOrParseArgs)
    }
  where
    liftReplOption = liftOption replReplOptions (\v flags -> flags{replReplOptions = v})

replOptions :: ShowOrParseArgs -> [OptionField ReplOptions]
replOptions _ =
  [ option
      []
      ["repl-no-load"]
      "Disable loading of project modules at REPL startup."
      replOptionsNoLoad
      (\p flags -> flags{replOptionsNoLoad = p})
      trueArg
  , option
      []
      ["repl-options"]
      "Use the option(s) for the repl"
      replOptionsFlags
      (\p flags -> flags{replOptionsFlags = p})
      (reqArg "FLAG" (succeedReadE words) id)
  , option
      []
      ["repl-multi-file"]
      "Write repl options to this directory rather than starting repl mode"
      replOptionsFlagOutput
      (\p flags -> flags{replOptionsFlagOutput = p})
      (reqArg "DIR" (succeedReadE Flag) flagToList)
  , option
      []
      ["with-repl"]
      "Give the path to a program to use for REPL"
      replWithRepl
      (\v flags -> flags{replWithRepl = v})
      (reqArgFlag "PATH")
  ]
