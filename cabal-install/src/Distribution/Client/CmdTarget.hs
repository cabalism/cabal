{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Client.CmdTarget
  ( targetCommand
  , targetAction
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.CmdBuild (selectComponentTarget, selectPackageTargets)
import Distribution.Client.NixStyleOptions
  ( NixStyleFlags (..)
  , defaultNixStyleFlags
  , nixStyleOptions
  )
import Distribution.Client.Setup
  ( ConfigFlags (..)
  , GlobalFlags
  )
import Distribution.Client.TargetForms
  ( printTargetForms
  , resolveTargetForms
  )
import Distribution.Simple.Command
  ( CommandUI (..)
  , usageAlternatives
  )
import Distribution.Simple.Flag (fromFlagOrDefault)
import Distribution.Simple.Utils
  ( wrapText
  )
import Distribution.Verbosity
  ( defaultVerbosityHandles
  , mkVerbosity
  , normal
  )
import Text.PrettyPrint
import qualified Text.PrettyPrint as Pretty

-------------------------------------------------------------------------------
-- Command
-------------------------------------------------------------------------------

targetCommand :: CommandUI (NixStyleFlags ())
targetCommand =
  CommandUI
    { commandName = "v2-target"
    , commandSynopsis = "Target a subset of all targets."
    , commandUsage = usageAlternatives "v2-target" ["[TARGETS]"]
    , commandDescription =
        Just . const . render $
          vcat
            [ intro
            , vcat $ punctuate (text "\n") [targetForms, ctypes, Pretty.empty]
            , caution
            , unique
            ]
    , commandNotes = Just $ \pname -> render (examples pname) ++ "\n"
    , commandDefaultFlags = defaultNixStyleFlags ()
    , commandOptions = nixStyleOptions (const [])
    }
  where
    intro =
      text . wrapText $
        "Discover targets in a project for use with other commands taking [TARGETS].\n\n"
          ++ "This command, like many others, takes [TARGETS]. Taken together, these will"
          ++ " select for a set of targets in the project. When none are supplied, the"
          ++ " command acts as if 'all' was supplied."
          ++ " Targets in the returned subset are shown sorted and fully-qualified."

    targetForms =
      vcat
        [ text "A [TARGETS] item can be one of these target forms:"
        , nest 1 . vcat $
            (char '-' <+>)
              <$> [ text "a package target (e.g. [pkg:]package)"
                  , text "a component target (e.g. [package:][ctype:]component)"
                  , text "all packages (e.g. all)"
                  , text "components of a particular type (e.g. package:ctypes or all:ctypes)"
                  , text "a module target: (e.g. [package:][ctype:]module)"
                  , text "a filepath target: (e.g. [package:][ctype:]filepath)"
                  ]
        ]

    ctypes =
      vcat
        [ text "The ctypes, in short form and (long form), can be one of:"
        , nest 1 . vcat $
            (char '-' <+>)
              <$> [ "libs" <+> parens "libraries"
                  , "exes" <+> parens "executables"
                  , "tests"
                  , "benches" <+> parens "benchmarks"
                  , "flibs" <+> parens "foreign-libraries"
                  ]
        ]

    caution =
      text . wrapText $
        "WARNING: For a package, all, module or filepath target, cabal target [TARGETS] \
        \ will only show 'libs' and 'exes' of the [TARGETS] by default. To also show \
        \ tests and benchmarks, enable them with '--enable-tests' and \
        \ '--enable-benchmarks'."

    unique =
      text . wrapText $
        "NOTE: For commands expecting a unique TARGET, a fully-qualified target is the safe \
        \ way to go but it may be convenient to type out a shorter TARGET. For example, if the \
        \ set of 'cabal target all:exes' has one item then 'cabal list-bin all:exes' will \
        \ work too."

    examples pname =
      vcat
        [ text "Examples" Pretty.<> colon
        , nest 2 $
            vcat
              [ vcat
                  [ text pname <+> text "v2-target all"
                  , nest 2 $ text "Targets of the package in the current directory or all packages in the project"
                  ]
              , vcat
                  [ text pname <+> text "v2-target pkgname"
                  , nest 2 $ text "Targets of the package named pkgname in the project"
                  ]
              , vcat
                  [ text pname <+> text "v2-target ./pkgfoo"
                  , nest 2 $ text "Targets of the package in the ./pkgfoo directory"
                  ]
              , vcat
                  [ text pname <+> text "v2-target cname"
                  , nest 2 $ text "Targets of the component named cname in the project"
                  ]
              ]
        ]

-------------------------------------------------------------------------------
-- Action
-------------------------------------------------------------------------------

targetAction :: NixStyleFlags () -> [String] -> GlobalFlags -> IO ()
targetAction flags@NixStyleFlags{..} ts globalFlags = do
  (targets, elaboratedPlan) <-
    resolveTargetForms
      verbosity
      selectPackageTargets
      selectComponentTarget
      flags
      globalFlags
      targetStrings

  printTargetForms verbosity targetStrings targets elaboratedPlan
  where
    verbosity =
      mkVerbosity defaultVerbosityHandles $
        fromFlagOrDefault normal (configVerbosity configFlags)
    targetStrings = if null ts then ["all"] else ts
