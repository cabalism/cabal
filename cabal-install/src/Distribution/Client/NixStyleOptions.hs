{-# LANGUAGE ViewPatterns #-}

-- | Command line options for nix-style / v2 commands.
--
-- The commands take a lot of the same options, which affect how install plan
-- is constructed.
module Distribution.Client.NixStyleOptions
  ( NixStyleFlags (..)
  , nixStyleOptions
  , defaultNixStyleFlags
  , updNixStyleCommonSetupFlags
  , cfgVerbosity

    -- * Option filtering
  , removeUnsupportedOptions
  , removeInstallOptions
  , removeIrrelevantOptions
  , removeHaddockOptions
  , removeTestOptions
  , removeBenchOptions
  , removeProfilingOptions
  , removeSolvingOptions
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Simple.Command (OptionField (..), ShowOrParseArgs)
import Distribution.Simple.Setup
  ( BenchmarkFlags (benchmarkCommonFlags)
  , CommonSetupFlags (..)
  , HaddockFlags (..)
  , TestFlags (testCommonFlags)
  , fromFlagOrDefault
  )
import Distribution.Solver.Types.ConstraintSource (ConstraintSource (..))

import Distribution.Client.ProjectFlags
  ( ProjectFlags (..)
  , defaultProjectFlags
  , projectFlagsOptions
  )
import Distribution.Client.Setup
  ( ConfigExFlags
  , ConfigFlags (..)
  , InstallFlags (..)
  , benchmarkOptions
  , configureExOptions
  , configureOptions
  , haddockOptions
  , installOptions
  , liftOptions
  , testOptions
  )
import Distribution.Simple.Utils (isInfixOf)
import Distribution.Verbosity (VerbosityFlags, defaultVerbosityHandles, mkVerbosity)

data NixStyleFlags a = NixStyleFlags
  { configFlags :: ConfigFlags
  , configExFlags :: ConfigExFlags
  , installFlags :: InstallFlags
  , haddockFlags :: HaddockFlags
  , testFlags :: TestFlags
  , benchmarkFlags :: BenchmarkFlags
  , projectFlags :: ProjectFlags
  , extraFlags :: a
  }

nixStyleOptions
  :: (ShowOrParseArgs -> [OptionField a])
  -> ShowOrParseArgs
  -> [OptionField (NixStyleFlags a)]
nixStyleOptions commandOptions showOrParseArgs =
  liftOptions
    configFlags
    set1
    -- Note: [Hidden Flags]
    -- We reuse the configure options from v1 commands which on their turn
    -- reuse the ones from Cabal) but we hide some of them in v2 commands.
    ( filter
        ( ( `notElem`
              [ "cabal-file"
              , "constraint"
              , "dependency"
              , "promised-dependency"
              , "exact-configuration"
              ]
          )
            . optionName
        )
        $ configureOptions showOrParseArgs
    )
    ++ liftOptions
      configExFlags
      set2
      ( configureExOptions
          showOrParseArgs
          ConstraintSourceCommandlineFlag
      )
    ++ liftOptions
      installFlags
      set3
      -- hide "target-package-db" and "symlink-bindir" flags from the
      -- install options.
      -- "symlink-bindir" is obsoleted by "installdir" in ClientInstallFlags
      ( filter
          ( (`notElem` ["target-package-db", "symlink-bindir"])
              . optionName
          )
          $ installOptions showOrParseArgs
      )
    ++ liftOptions
      haddockFlags
      set4
      -- hide "verbose" and "builddir" flags from the
      -- haddock options.
      ( filter
          ( (`notElem` ["v", "verbose", "builddir"])
              . optionName
          )
          $ haddockOptions showOrParseArgs
      )
    ++ liftOptions testFlags set5 (testOptions showOrParseArgs)
    ++ liftOptions benchmarkFlags set6 (benchmarkOptions showOrParseArgs)
    ++ liftOptions projectFlags set7 (projectFlagsOptions showOrParseArgs)
    ++ liftOptions extraFlags set8 (commandOptions showOrParseArgs)
  where
    set1 x flags = flags{configFlags = x}
    set2 x flags = flags{configExFlags = x}
    set3 x flags = flags{installFlags = x}
    set4 x flags = flags{haddockFlags = x}
    set5 x flags = flags{testFlags = x}
    set6 x flags = flags{benchmarkFlags = x}
    set7 x flags = flags{projectFlags = x}
    set8 x flags = flags{extraFlags = x}

defaultNixStyleFlags :: a -> NixStyleFlags a
defaultNixStyleFlags x =
  NixStyleFlags
    { configFlags = mempty
    , configExFlags = mempty
    , installFlags = mempty
    , haddockFlags = mempty
    , testFlags = mempty
    , benchmarkFlags = mempty
    , projectFlags = defaultProjectFlags
    , extraFlags = x
    }

updNixStyleCommonSetupFlags
  :: (CommonSetupFlags -> CommonSetupFlags)
  -> NixStyleFlags a
  -> NixStyleFlags a
updNixStyleCommonSetupFlags setFlag nixFlags =
  nixFlags
    { configFlags =
        let flags = configFlags nixFlags
            common = configCommonFlags flags
         in flags{configCommonFlags = setFlag common}
    , haddockFlags =
        let flags = haddockFlags nixFlags
            common = haddockCommonFlags flags
         in flags{haddockCommonFlags = setFlag common}
    , testFlags =
        let flags = testFlags nixFlags
            common = testCommonFlags flags
         in flags{testCommonFlags = setFlag common}
    , benchmarkFlags =
        let flags = benchmarkFlags nixFlags
            common = benchmarkCommonFlags flags
         in flags{benchmarkCommonFlags = setFlag common}
    }

cfgVerbosity :: VerbosityFlags -> NixStyleFlags a -> Verbosity
cfgVerbosity v flags =
  mkVerbosity defaultVerbosityHandles $
    fromFlagOrDefault v (setupVerbosity . configCommonFlags $ configFlags flags)

removeUnsupportedOptions :: [OptionField a] -> [OptionField a]
removeUnsupportedOptions = filter (\(optionName -> o) -> not ("root-cmd" == o || "allow-boot-library-installs" == o))

removeInstallOptions :: [OptionField a] -> [OptionField a]
removeInstallOptions =
  filter
    ( \(optionName -> o) ->
        not
          ( "dir" `isSuffixOf` o
              || "reinstall" `isInfixOf` o
              || "run-tests" == o
              || "root-cmd" == o
              || "allow-boot-library-installs" == o
          )
    )

removeIrrelevantOptions :: [OptionField a] -> [OptionField a]
removeIrrelevantOptions = filter (\(optionName -> o) -> not ("per-component" `isSuffixOf` o))

removeHaddockOptions :: [OptionField a] -> [OptionField a]
removeHaddockOptions =
  filter
    ( \(optionName -> o) ->
        not
          ( "haddock" `isPrefixOf` o
              || "test" `isPrefixOf` o
              || "bench" `isPrefixOf` o
          )
    )

removeTestOptions :: [OptionField a] -> [OptionField a]
removeTestOptions = filter (\(optionName -> o) -> not ("test" `isPrefixOf` o))

removeBenchOptions :: [OptionField a] -> [OptionField a]
removeBenchOptions = filter (\(optionName -> o) -> not ("bench" `isPrefixOf` o))

removeProfilingOptions :: [OptionField a] -> [OptionField a]
removeProfilingOptions = filter (\(optionName -> o) -> not ("profiling" `isInfixOf` o))

removeSolvingOptions :: [OptionField a] -> [OptionField a]
removeSolvingOptions =
  filter
    ( \(optionName -> o) ->
        not
          ( "max-backjumps" == o
              || "conflicts" `isInfixOf` o
              || "goals" `isInfixOf` o
              || "index-state" == o
              || "upgrade-dependencies" == o
              || "reject-unconstrained-dependencies" == o
              || "prefer-oldest" == o
              || "allow-older" == o
              || "allow-newer" == o
              || "preference" == o
              || "shadow-installed-packages" == o
          )
    )
