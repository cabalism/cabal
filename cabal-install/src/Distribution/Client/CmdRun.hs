{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | cabal-install CLI command: run
module Distribution.Client.CmdRun
  ( -- * The @run@ CLI and action
    runCommand
  , runAction
  , handleShebang
  , validScript

    -- * Internals exposed for testing
  , matchesMultipleProblem
  , noExesProblem
  , selectPackageTargets
  , selectComponentTarget
  , ArgKind (..)
  , ClassifiedArg (..)
  , TargetAndArgs (..)
  , classifyArgs
  , separatorPosition
  , splitTargetAndArgs

    -- ** Re-exported so tests and doctests can build a pure oracle
  , TargetMatch (..)
  , TargetOracle (..)
  , knownTargetOracle
  ) where

import Distribution.Client.Compat.Prelude hiding (toList)
import Prelude ()

import qualified Data.Set as Set
import Distribution.Client.CmdErrorMessages
  ( listPlural
  , plural
  , renderListCommaAnd
  , renderListPretty
  , renderTargetProblem
  , renderTargetProblemNoTargets
  , renderTargetSelector
  , showTargetSelector
  , targetSelectorFilter
  , targetSelectorPluralPkgs
  )
import Distribution.Client.Errors
import Distribution.Client.GlobalFlags
  ( defaultGlobalFlags
  )
import Distribution.Client.InstallPlan
  ( foldPlanPackage
  , toList
  )
import Distribution.Client.NixStyleOptions
  ( NixStyleFlags (..)
  , cfgVerbosity
  , defaultNixStyleFlags
  , nixStyleOptions
  )
import Distribution.Client.ProjectConfig.Types
  ( ProjectConfig (projectConfigShared)
  , ProjectConfigShared (projectConfigProgPathExtra)
  )
import Distribution.Client.ProjectOrchestration hiding (targetsMap)
import qualified Distribution.Client.ProjectOrchestration as Orchestration (targetsMap)
import Distribution.Client.ProjectPlanning
  ( ElaboratedConfiguredPackage (..)
  , ElaboratedInstallPlan
  , binDirectoryFor
  )
import Distribution.Client.ProjectPlanning.Types
  ( ElaboratedPackageOrComponent (..)
  , dataDirsEnvironmentForPlan
  , elabExeDependencyPaths
  )
import Distribution.Client.ScriptUtils
  ( AcceptNoTargets (..)
  , TargetContext (..)
  , movedExePath
  , updateContextAndWriteProjectFile
  , withContextAndSelectorsAndArgs
  )
import Distribution.Client.Setup
  ( GlobalFlags (..)
  )
import Distribution.Client.TargetForms
  ( TargetMatch (..)
  , TargetOracle (..)
  , knownTargetOracle
  , newTargetOracle
  , runnableComponents
  )
import Distribution.Client.TargetProblem
  ( TargetProblem (..)
  )
import Distribution.Client.TargetSelector
  ( defaultDirActions
  , readTargetSelectorsWith
  )
import Distribution.Client.Types
  ( PackageSpecifier
  , UnresolvedSourcePackage
  )
import Distribution.Client.Utils
  ( giveRTSWarning
  , occursOnlyOrBefore
  )

import Distribution.Simple.BuildToolDepends
  ( getAllInternalToolDependencies
  )
import Distribution.Simple.Command
  ( CommandUI (..)
  , usageAlternatives
  )
import Distribution.Simple.Program.Find
  ( ProgramSearchPathEntry (ProgramSearchPathDir)
  , defaultProgramSearchPath
  , logExtraProgramSearchPath
  , programSearchPathAsPATHVar
  )
import Distribution.Simple.Program.Run
  ( ProgramInvocation (..)
  , emptyProgramInvocation
  , runProgramInvocation
  )
import Distribution.Simple.Utils
  ( dieWithException
  , info
  , notice
  , sortNub
  , warn
  , wrapText
  )

import Distribution.Types.ComponentName
  ( componentNameRaw
  )
import qualified Distribution.Types.Executable as PD
  ( buildInfo
  , exeName
  )
import qualified Distribution.Types.PackageDescription as PD
  ( executables
  )
import Distribution.Types.UnitId
  ( UnitId
  )
import Distribution.Types.UnqualComponentName
  ( UnqualComponentName
  , unUnqualComponentName
  )
import Distribution.Utils.NubList
  ( fromNubList
  )
import Distribution.Verbosity
  ( normal
  , silent
  )
import GHC.Environment
  ( getFullArgs
  )
import System.Directory
  ( doesFileExist
  )
import System.FilePath
  ( isPathSeparator
  , isValid
  , (</>)
  )

runCommand :: CommandUI (NixStyleFlags ())
runCommand =
  CommandUI
    { commandName = "v2-run"
    , commandSynopsis = "Run an executable."
    , commandUsage =
        usageAlternatives
          "v2-run"
          ["[TARGET] [FLAGS] [-- EXECUTABLE_FLAGS]"]
    , commandDescription = Just $ \pname ->
        wrapText $
          "Runs the specified executable-like component (an executable, a test, "
            ++ "or a benchmark), first ensuring it is up to date.\n\n"
            ++ "Any executable-like component in any package in the project can be "
            ++ "specified. A package can be specified if contains just one "
            ++ "executable-like, preferring a single executable. The default is to "
            ++ "use the package in the current directory if it contains just one "
            ++ "executable-like.\n\n"
            ++ "Extra arguments can be passed to the program, but use '--' to "
            ++ "separate arguments for the program from arguments for "
            ++ pname
            ++ ". The executable is run in an environment where it can find its "
            ++ "data files inplace in the build tree.\n\n"
            ++ "Dependencies are built or rebuilt as necessary. Additional "
            ++ "configuration flags can be specified on the command line and these "
            ++ "extend the project configuration from the 'cabal.project', "
            ++ "'cabal.project.local' and other files."
    , commandNotes = Just $ \pname ->
        "Examples:\n"
          ++ "  "
          ++ pname
          ++ " v2-run\n"
          ++ "    Run the executable-like in the package in the current directory\n"
          ++ "  "
          ++ pname
          ++ " v2-run foo-tool\n"
          ++ "    Run the named executable-like (in any package in the project)\n"
          ++ "  "
          ++ pname
          ++ " v2-run pkgfoo:foo-tool\n"
          ++ "    Run the executable-like 'foo-tool' in the package 'pkgfoo'\n"
          ++ "  "
          ++ pname
          ++ " v2-run foo -O2 -- dothing --fooflag\n"
          ++ "    Build with '-O2' and run the program, passing it extra arguments.\n"
    , commandDefaultFlags = defaultNixStyleFlags ()
    , commandOptions = nixStyleOptions (const [])
    }

-- | The @run@ command runs a specified executable-like component, building it
-- first if necessary. The component can be either an executable, a test,
-- or a benchmark. This is particularly useful for passing arguments to
-- exes/tests/benchs by simply appending them after a @--@.
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
runAction :: NixStyleFlags () -> [String] -> GlobalFlags -> IO ()
runAction flags targetAndArgs globalFlags = do
  fullArgs <- getFullArgs
  let splitVerbosity = cfgVerbosity normal flags
      split localPackages ts = do
        oracle <- newTargetOracle defaultDirActions localPackages (Just ExeKind)
        r <- splitTargetAndArgs oracle fullArgs ts
        reportClassification splitVerbosity localPackages r
        return (taTargets r, taArgs r)
  withContextAndSelectorsAndArgs splitVerbosity RejectNoTargets (Just ExeKind) flags split targetAndArgs globalFlags OtherCommand $ \targetCtx ctx targetSelectors (targetStrings, args) -> do
    (baseCtx, defaultVerbosity) <- case targetCtx of
      ProjectContext -> return (ctx, normal)
      GlobalContext -> return (ctx, normal)
      ScriptContext path exemeta -> (,silent) <$> updateContextAndWriteProjectFile ctx path exemeta

    let verbosity = cfgVerbosity defaultVerbosity flags

    buildCtx <-
      runProjectPreBuildPhase verbosity baseCtx $ \elaboratedPlan -> do
        when (buildSettingOnlyDeps (buildSettings baseCtx)) $
          dieWithException verbosity NoSupportForRunCommand

        when (occursOnlyOrBefore fullArgs "+RTS" "--") $
          warn verbosity $
            giveRTSWarning "run"

        -- Interpret the targets on the command line as build targets
        -- (as opposed to say repl or haddock targets).
        targets <-
          either (reportTargetProblems verbosity) return $
            resolveTargetsFromSolver
              selectPackageTargets
              selectComponentTarget
              elaboratedPlan
              Nothing
              targetSelectors

        -- Reject multiple targets, or at least targets in different
        -- components. It is ok to have two module/file targets in the
        -- same component, but not two that live in different components.
        --
        -- Note that we discard the target and return the whole 'TargetsMap',
        -- so this check will be repeated (and must succeed) after
        -- the 'runProjectPreBuildPhase'. Keep it in mind when modifying this.
        _ <-
          singleExeOrElse
            ( reportTargetProblems
                verbosity
                [multipleTargetsProblem targets]
            )
            targets

        -- Several different targets that all name the same component, as in
        -- 'cabal run foo exe:foo'. That is not multiple targets, so it runs,
        -- but it is worth saying that the repetition had no effect.
        reportRepeatedTargets verbosity targetStrings

        let elaboratedPlan' =
              pruneInstallPlanToTargets
                TargetActionBuild
                targets
                elaboratedPlan
        return (elaboratedPlan', targets)

    (selectedUnitId, selectedComponent) <-
      -- Slight duplication with 'runProjectPreBuildPhase'.
      singleExeOrElse
        ( dieWithException verbosity RunPhaseReached
        )
        $ Orchestration.targetsMap buildCtx

    printPlan verbosity baseCtx buildCtx

    buildOutcomes <- runProjectBuildPhase verbosity baseCtx buildCtx
    runProjectPostBuildPhase verbosity baseCtx buildCtx buildOutcomes

    let elaboratedPlan = elaboratedPlanToExecute buildCtx
        matchingElaboratedConfiguredPackages =
          matchingPackagesByUnitId
            selectedUnitId
            elaboratedPlan

    let exeName = unUnqualComponentName selectedComponent

    -- In the common case, we expect @matchingElaboratedConfiguredPackages@
    -- to consist of a single element that provides a single way of building
    -- an appropriately-named executable. In that case we take that
    -- package and continue.
    --
    -- However, multiple packages/components could provide that
    -- executable, or it's possible we don't find the executable anywhere
    -- in the build plan. I suppose in principle it's also possible that
    -- a single package provides an executable in two different ways,
    -- though that's probably a bug if. Anyway it's a good lint to report
    -- an error in all of these cases, even if some seem like they
    -- shouldn't happen.
    pkg <- case matchingElaboratedConfiguredPackages of
      [] -> dieWithException verbosity $ UnknownExecutable exeName selectedUnitId
      [elabPkg] -> do
        info verbosity $
          "Selecting "
            ++ prettyShow selectedUnitId
            ++ " to supply "
            ++ exeName
        return elabPkg
      elabPkgs ->
        dieWithException verbosity $
          MultipleMatchingExecutables exeName (fmap (\p -> " - in package " ++ prettyShow (elabUnitId p)) elabPkgs)

    let defaultExePath =
          binDirectoryFor
            (distDirLayout baseCtx)
            (elaboratedShared buildCtx)
            pkg
            exeName
            </> exeName
        exePath = fromMaybe defaultExePath (movedExePath selectedComponent (distDirLayout baseCtx) (elaboratedShared buildCtx) pkg)

    let dryRun =
          buildSettingDryRun (buildSettings baseCtx)
            || buildSettingOnlyDownload (buildSettings baseCtx)

    let
      -- HACK alert: when doing a per-package build (e.g. with a Custom setup),
      -- 'elabExeDependencyPaths' will not contain any internal executables
      -- (they are deliberately filtered out; and even if they weren't, they have the wrong paths).
      -- We add them back in here to ensure that any "build-tool-depends" of
      -- the current executable is available in PATH at runtime.
      internalToolDepsOfThisExe
        | ElabPackage{} <- elabPkgOrComp pkg
        , let pkg_descr = elabPkgDescription pkg
        , thisExe : _ <- filter ((== exeName) . unUnqualComponentName . PD.exeName) $ PD.executables pkg_descr
        , let thisExeBI = PD.buildInfo thisExe =
            [ binDirectoryFor (distDirLayout baseCtx) (elaboratedShared buildCtx) pkg depExeNm
            | depExe <- getAllInternalToolDependencies pkg_descr thisExeBI
            , let depExeNm = unUnqualComponentName depExe
            ]
        | otherwise =
            []
      extraPath =
        elabExeDependencyPaths pkg
          ++ ( fromNubList
                . projectConfigProgPathExtra
                . projectConfigShared
                . projectConfig
                $ baseCtx
             )
          ++ internalToolDepsOfThisExe

    logExtraProgramSearchPath verbosity extraPath
    progPath <- programSearchPathAsPATHVar (map ProgramSearchPathDir extraPath ++ defaultProgramSearchPath)

    if dryRun
      then notice verbosity "Running of executable suppressed by flag(s)"
      else
        runProgramInvocation
          verbosity
          emptyProgramInvocation
            { progInvokePath = exePath
            , progInvokeArgs = args
            , progInvokeEnv =
                ("PATH", Just progPath)
                  : dataDirsEnvironmentForPlan
                    (distDirLayout baseCtx)
                    elaboratedPlan
            }

-- | What a single element of the combined target-and-argument list turned out
-- to be.
data ArgKind
  = -- | Looks like a flag, so it is never probed. Skipping flags saves the
    -- probe, and stops a flag that happens to collide with a component name
    -- from being mistaken for a target.
    ArgFlag
  | -- | Probed, and it names a target.
    ArgTarget TargetMatch
  | -- | Probed, and it does not name a target.
    ArgPlain
  deriving (Eq, Ord, Show)

-- | One element of the combined list, together with what it is and where it
-- sits relative to the @--@ separator.
data ClassifiedArg = ClassifiedArg
  { caString :: String
  , caKind :: ArgKind
  , caBeforeSep :: Bool
  -- ^ Does this element precede the @--@ separator? When there is no
  -- separator every element counts as preceding it.
  }
  deriving (Eq, Show)

-- | The split, together with the evidence it was derived from.
data TargetAndArgs = TargetAndArgs
  { taTargets :: [String]
  , taArgs :: [String]
  , taClassified :: [ClassifiedArg]
  , taSeparator :: Maybe Int
  -- ^ Where the @--@ separator fell, as the number of elements preceding it.
  }
  deriving (Eq, Show)

-- | Is this element a target?
isTargetArg :: ClassifiedArg -> Bool
isTargetArg ca = case caKind ca of
  ArgTarget{} -> True
  _ -> False

-- | Does this look like a flag rather than something worth probing?
isFlagString :: String -> Bool
isFlagString s = "-" `isPrefixOf` s || s == "+RTS"

-- | Where the @--@ separator falls, expressed as the number of elements of the
-- combined list that precede it. 'Nothing' when there is no separator.
--
-- The full command line is the original from 'getFullArgs'; it is the only
-- place the separator survives, as the option parser drops it.
separatorPosition
  :: [String]
  -- ^ Full command line arguments.
  -> [String]
  -- ^ The parser-produced list combining targets and their arguments.
  -> Maybe Int
separatorPosition fullArgs targetAndArgs = case dropWhile (/= "--") fullArgs of
  -- The combined list ends with everything that followed the separator, so the
  -- difference is what preceded it.
  ("--" : exeArgs) -> Just (length targetAndArgs - length exeArgs)
  _ -> Nothing

-- | Probe every element of the combined list that does not look like a flag.
--
-- Classification does not depend on where the @--@ separator falls; only
-- 'caBeforeSep' does. Deciding what each string /is/ separately from where it
-- /sits/ is what lets 'splitTargetAndArgs' explain itself afterwards.
classifyArgs
  :: Monad m
  => TargetOracle m
  -> [String]
  -- ^ Full command line arguments, used only to locate the @--@ separator.
  -> [String]
  -- ^ The parser-produced list combining targets and their arguments. These
  -- do not include arguments passed to @cabal@ itself, such as a @+RTS@
  -- preceding the @--@ separator.
  -> m [ClassifiedArg]
classifyArgs oracle fullArgs targetAndArgs =
  sequenceA
    [ classify i s
    | (i, s) <- zip [0 :: Int ..] targetAndArgs
    ]
  where
    sep = separatorPosition fullArgs targetAndArgs

    beforeSep i = maybe True (i <) sep

    classify i s
      | isFlagString s = return (ClassifiedArg s ArgFlag (beforeSep i))
      | otherwise = do
          match <- probeTarget oracle s
          return $ ClassifiedArg s (maybe ArgPlain ArgTarget match) (beforeSep i)

-- | Split @cabal run@ arguments (@exe cmd@ arguments in the examples) into
-- target selectors and target executable arguments.
--
-- The @--@ separator says where the executable's arguments may begin; target
-- resolution says which of the leading strings really are targets. Neither
-- alone is enough: counting the strings around @--@ loses a target given only
-- after it (<https://github.com/haskell/cabal/issues/12231>), and resolution
-- alone would let an unrecognised word silently become an argument.
--
-- The examples below use an oracle that recognises a fixed set of names:
--
-- >>> let run known fullArgs targetAndArgs = (\r -> (taTargets r, taArgs r)) (runIdentity (splitTargetAndArgs (knownTargetOracle known) fullArgs targetAndArgs))
--
-- When a target is given it appears in both lists:
--
-- >>> run ["target"] ["exe", "cmd", "target"] ["target"]
-- (["target"],[])
--
-- The @+RTS@ argument is passed to the executable so only appears in the first
-- list:
--
-- >>> run ["target"] ["exe", "cmd", "target", "+RTS"] ["target"]
-- (["target"],[])
--
-- The @--@ follows the @+RTS@ argument, so @+RTS@ is passed to the executable
-- and only appears in the first list:
--
-- >>> run ["target"] ["exe", "cmd", "target", "+RTS", "--"] ["target"]
-- (["target"],[])
--
-- The @--@ precedes the @+RTS@ argument, so @+RTS@ is included in the
-- combined list as well:
--
-- >>> run ["target"] ["exe", "cmd", "target", "--", "+RTS"] ["target", "+RTS"]
-- (["target"],["+RTS"])
--
-- Same examples as above but when no target is given:
--
-- >>> run [] ["exe", "cmd"] []
-- ([],[])
-- >>> run [] ["exe", "cmd", "+RTS"] []
-- ([],[])
-- >>> run [] ["exe", "cmd", "+RTS", "--"] []
-- ([],[])
-- >>> run [] ["exe", "cmd", "--", "+RTS"] ["+RTS"]
-- ([],["+RTS"])
--
-- >>> run ["cabal-install:parser-tests"] ["-v2", "repl", "--dry-run", "cabal-install:parser-tests", "--", "--dry-run", "cabal-install:parser-tests", "--dry-run"] ["cabal-install:parser-tests", "--dry-run", "cabal-install:parser-tests", "--dry-run"]
-- (["cabal-install:parser-tests"],["--dry-run","cabal-install:parser-tests","--dry-run"])
--
-- A target given only after the separator is still found:
--
-- >>> run ["saturn-test-suite"] ["run", "--", "saturn-test-suite", "--randomize"] ["saturn-test-suite", "--randomize"]
-- (["saturn-test-suite"],["--randomize"])
--
-- but a leading flag after the separator is not mistaken for one:
--
-- >>> run ["foo"] ["run", "--", "--randomize"] ["--randomize"]
-- ([],["--randomize"])
--
-- With no separator at all the leading word is a target claim, so an
-- unrecognised one is kept and left to fail with a proper error rather than
-- quietly becoming an argument:
--
-- >>> run ["foo"] ["run", "bar"] ["bar"]
-- (["bar"],[])
splitTargetAndArgs
  :: Monad m
  => TargetOracle m
  -> [String]
  -- ^ Full command line arguments, the original command line from
  -- 'getFullArgs', which is only used to locate the @--@ separator.
  -> [String]
  -- ^ The parser-produced list that combines targets and their arguments.
  -> m TargetAndArgs
splitTargetAndArgs oracle fullArgs targetAndArgs = do
  classified <- classifyArgs oracle fullArgs targetAndArgs
  return $ splitClassifiedArgs (separatorPosition fullArgs targetAndArgs) classified

-- | The rule itself, over an already-classified command line.
splitClassifiedArgs :: Maybe Int -> [ClassifiedArg] -> TargetAndArgs
splitClassifiedArgs sep classified =
  TargetAndArgs
    { taTargets = map caString targets
    , taArgs = map caString args
    , taClassified = classified
    , taSeparator = sep
    }
  where
    -- Where targets may be looked for. When something precedes the separator
    -- the targets are among those; when nothing does, the whole list is fair
    -- game, which is what keeps a target given only after @--@ reachable.
    candidates = case sep of
      Just n | n > 0 -> take n classified
      _ -> classified

    resolved = takeWhile isTargetArg candidates

    -- Without a separator the user has not signalled that arguments follow, so
    -- the leading word is a target claim even when it does not resolve. It
    -- then fails with the usual unrecognised-target error and its
    -- suggestions. A leading flag claims nothing, so it is exempt.
    targets = case (sep, resolved, classified) of
      (Nothing, [], ca : _) | caKind ca /= ArgFlag -> [ca]
      _ -> resolved

    args = drop (length targets) classified

-- | Warn when the same component was named by more than one target.
--
-- Called only once 'singleExeOrElse' has established that every target landed
-- on one component, so any extra target string is by definition a repetition.
-- Doing it here rather than at classification time is what keeps it to a
-- single warning: 'solo' and 'single:exe:solo' resolve to the very same
-- selector while 'single:exes' resolves to a different one that only collapses
-- against them here, and neither half of that can see the other.
reportRepeatedTargets :: Verbosity -> [String] -> IO ()
reportRepeatedTargets verbosity targetStrings
  | length targetStrings < 2 = return ()
  | otherwise =
      warn verbosity $ case sortNub targetStrings of
        [one] ->
          "The target '" ++ one ++ "' was given more than once; it is run once."
        several ->
          "The targets "
            ++ renderListCommaAnd (map (\s -> "'" ++ s ++ "'") several)
            ++ " all name the same component, which is run once."

-- | Say out loud anything about the split that the user is unlikely to have
-- intended. Nothing here changes the split; it only explains it.
reportClassification
  :: Verbosity
  -> [PackageSpecifier UnresolvedSourcePackage]
  -> TargetAndArgs
  -> IO ()
reportClassification verbosity localPackages TargetAndArgs{..} = do
  -- A string that names a component but was left on the argument side, without
  -- the user having put a '--' in front of it. Matches on a mere existing file
  -- are ignored: passing a filename to an executable is entirely ordinary.
  unless (null namedButPassed) $
    warn verbosity $
      renderListCommaAnd (map (\s -> "'" ++ s ++ "'") namedButPassed)
        ++ " "
        ++ plural (listPlural namedButPassed) "names a component" "name components"
        ++ " in this project but "
        ++ plural (listPlural namedButPassed) "is" "are"
        ++ " being passed to the executable as "
        ++ plural (listPlural namedButPassed) "an argument" "arguments"
        ++ ". Put '--' before "
        ++ plural (listPlural namedButPassed) "it" "them"
        ++ " to silence this, or move the target to the front."

  -- Something before an explicit '--' that does not name a target. Before this
  -- became a resolved split it would have been reported as an unrecognised
  -- target, so do not demote it silently.
  unless (null demoted) $
    warn verbosity $
      renderListCommaAnd (map (\s -> "'" ++ s ++ "'") demoted)
        ++ " "
        ++ plural (listPlural demoted) "precedes" "precede"
        ++ " '--' but "
        ++ plural (listPlural demoted) "does" "do"
        ++ " not name a target, so "
        ++ plural (listPlural demoted) "it is" "they are"
        ++ " being passed to the executable as "
        ++ plural (listPlural demoted) "an argument" "arguments"
        ++ "."

  -- About to fail: the leading word was kept as a target only because no '--'
  -- said otherwise. If there is exactly one thing we could have run, the user
  -- probably meant it as an argument.
  when (isNothing taSeparator && leadingIsPlain) $ do
    runnable <- soleRunnableComponent
    for_ runnable $ \cname ->
      notice verbosity $
        "There is only one component to run, "
          ++ componentNameRaw cname
          ++ ". If '"
          ++ concat (take 1 taTargets)
          ++ "' was meant as an argument to it rather than as a target, pass it after '--'."
  where
    argSide = drop (length taTargets) taClassified

    -- A match on a mere existing file does not count: passing a filename to an
    -- executable is entirely ordinary.
    namesComponent ca = case caKind ca of
      ArgTarget MatchSelector{} -> True
      _ -> False

    namedButPassed =
      [caString ca | ca <- argSide, caBeforeSep ca, namesComponent ca]

    -- The part of the candidate region that the target prefix did not reach.
    demoted = case taSeparator of
      Just n
        | n > 0 ->
            [ caString ca
            | ca <- drop (length taTargets) (take n taClassified)
            , caKind ca == ArgPlain
            ]
      _ -> []

    leadingIsPlain = case taClassified of
      ca : _ -> caKind ca == ArgPlain && not (null taTargets)
      [] -> False

    soleRunnableComponent = do
      selectors <- readTargetSelectorsWith defaultDirActions localPackages (Just ExeKind) []
      return $ case selectors of
        Right sels -> case runnableComponents localPackages sels of
          [cname] -> Just cname
          _ -> Nothing
        Left _ -> Nothing

-- | Used by the main CLI parser as heuristic to decide whether @cabal@ was
-- invoked as a script interpreter, i.e. via
--
-- > #! /usr/bin/env cabal
--
-- or
--
-- > #! /usr/bin/cabal
--
-- As the first argument passed to `cabal` will be a filepath to the
-- script to be interpreted.
--
-- See also 'handleShebang'
validScript :: String -> IO Bool
validScript script
  | isValid script && any isPathSeparator script = doesFileExist script
  | otherwise = return False

-- | Handle @cabal@ invoked as script interpreter, see also 'validScript'
--
-- First argument is the 'FilePath' to the script to be executed; second
-- argument is a list of arguments to be passed to the script.
handleShebang :: FilePath -> [String] -> IO ()
handleShebang script args =
  runAction (commandDefaultFlags runCommand) (script : args) defaultGlobalFlags

singleExeOrElse :: IO (UnitId, UnqualComponentName) -> TargetsMap -> IO (UnitId, UnqualComponentName)
singleExeOrElse action targetsMap =
  case Set.toList . distinctTargetComponents $ targetsMap of
    [(unitId, CExeName component)] -> return (unitId, component)
    [(unitId, CTestName component)] -> return (unitId, component)
    [(unitId, CBenchName component)] -> return (unitId, component)
    _ -> action

-- | Filter the 'ElaboratedInstallPlan' keeping only the
-- 'ElaboratedConfiguredPackage's that match the specified
-- 'UnitId'.
matchingPackagesByUnitId
  :: UnitId
  -> ElaboratedInstallPlan
  -> [ElaboratedConfiguredPackage]
matchingPackagesByUnitId uid =
  mapMaybe
    ( foldPlanPackage
        (const Nothing)
        ( \x ->
            if elabUnitId x == uid
              then Just x
              else Nothing
        )
    )
    . toList

-- | This defines what a 'TargetSelector' means for the @run@ command.
-- It selects the 'AvailableTarget's that the 'TargetSelector' refers to,
-- or otherwise classifies the problem.
--
-- For the @run@ command we select the exe if there is only one and it's
-- buildable. Fail if there are no or multiple buildable exe components.
selectPackageTargets
  :: TargetSelector
  -> [AvailableTarget k]
  -> Either RunTargetProblem [k]
selectPackageTargets targetSelector targets
  -- If there is a single executable component, select that. See #7403
  | [target] <- targetsExesBuildable =
      Right [target]
  -- Otherwise, if there is a single executable-like component left, select that.
  | [target] <- targetsExeLikesBuildable =
      Right [target]
  -- but fail if there are multiple buildable executables.
  | not (null targetsExeLikesBuildable) =
      Left (matchesMultipleProblem targetSelector targetsExeLikesBuildable')
  -- If there are executables but none are buildable then we report those
  | not (null targetsExeLikes') =
      Left (TargetProblemNoneEnabled targetSelector targetsExeLikes')
  -- If there are no executables but some other targets then we report that
  | not (null targets) =
      Left (noExesProblem targetSelector)
  -- If there are no targets at all then we report that
  | otherwise =
      Left (TargetProblemNoTargets targetSelector)
  where
    -- Targets that are precisely executables
    targetsExes = filterTargetsKind ExeKind targets
    targetsExesBuildable = selectBuildableTargets targetsExes

    -- Any target that could be executed
    targetsExeLikes =
      targetsExes
        ++ filterTargetsKind TestKind targets
        ++ filterTargetsKind BenchKind targets

    ( targetsExeLikesBuildable
      , targetsExeLikesBuildable'
      ) = selectBuildableTargets' targetsExeLikes

    targetsExeLikes' = forgetTargetsDetail targetsExeLikes

-- | For a 'TargetComponent' 'TargetSelector', check if the component can be
-- selected.
--
-- For the @run@ command we just need to check it is a executable-like
-- (an executable, a test, or a benchmark), in addition
-- to the basic checks on being buildable etc.
selectComponentTarget
  :: SubComponentTarget
  -> AvailableTarget k
  -> Either RunTargetProblem k
selectComponentTarget subtarget@WholeComponent t =
  case availableTargetComponentName t of
    CExeName _ -> component
    CTestName _ -> component
    CBenchName _ -> component
    _ -> Left (componentNotExeProblem pkgid cname)
  where
    pkgid = availableTargetPackageId t
    cname = availableTargetComponentName t
    component = selectComponentTargetBasic subtarget t
selectComponentTarget subtarget t =
  Left
    ( isSubComponentProblem
        (availableTargetPackageId t)
        (availableTargetComponentName t)
        subtarget
    )

-- | The various error conditions that can occur when matching a
-- 'TargetSelector' against 'AvailableTarget's for the @run@ command.
data RunProblem
  = -- | The 'TargetSelector' matches targets but no executables
    TargetProblemNoExes TargetSelector
  | -- | A single 'TargetSelector' matches multiple targets
    TargetProblemMatchesMultiple TargetSelector [AvailableTarget ()]
  | -- | Multiple 'TargetSelector's match multiple targets
    TargetProblemMultipleTargets TargetsMap
  | -- | The 'TargetSelector' refers to a component that is not an executable
    TargetProblemComponentNotExe PackageId ComponentName
  | -- | Asking to run an individual file or module is not supported
    TargetProblemIsSubComponent PackageId ComponentName SubComponentTarget
  deriving (Eq, Show)

type RunTargetProblem = TargetProblem RunProblem

noExesProblem :: TargetSelector -> RunTargetProblem
noExesProblem = CustomTargetProblem . TargetProblemNoExes

matchesMultipleProblem :: TargetSelector -> [AvailableTarget ()] -> RunTargetProblem
matchesMultipleProblem selector targets =
  CustomTargetProblem $
    TargetProblemMatchesMultiple selector targets

multipleTargetsProblem :: TargetsMap -> TargetProblem RunProblem
multipleTargetsProblem = CustomTargetProblem . TargetProblemMultipleTargets

componentNotExeProblem :: PackageId -> ComponentName -> TargetProblem RunProblem
componentNotExeProblem pkgid name =
  CustomTargetProblem $
    TargetProblemComponentNotExe pkgid name

isSubComponentProblem
  :: PackageId
  -> ComponentName
  -> SubComponentTarget
  -> TargetProblem RunProblem
isSubComponentProblem pkgid name subcomponent =
  CustomTargetProblem $
    TargetProblemIsSubComponent pkgid name subcomponent

reportTargetProblems :: Verbosity -> [RunTargetProblem] -> IO a
reportTargetProblems verbosity =
  dieWithException verbosity . CmdRunReportTargetProblems . unlines . map renderRunTargetProblem

renderRunTargetProblem :: RunTargetProblem -> String
renderRunTargetProblem (TargetProblemNoTargets targetSelector) =
  case targetSelectorFilter targetSelector of
    Just kind
      | kind /= ExeKind ->
          "The run command is for running executables, but the target '"
            ++ showTargetSelector targetSelector
            ++ "' refers to "
            ++ renderTargetSelector targetSelector
            ++ "."
    _ -> renderTargetProblemNoTargets "run" targetSelector
renderRunTargetProblem problem =
  renderTargetProblem "run" renderRunProblem problem

renderRunProblem :: RunProblem -> String
renderRunProblem (TargetProblemMatchesMultiple targetSelector targets) =
  "The run command is for running a single executable at once. The target '"
    ++ showTargetSelector targetSelector
    ++ "' refers to "
    ++ renderTargetSelector targetSelector
    ++ " which includes \n"
    ++ unlines
      ( (\(label, xs) -> "- " ++ label ++ ": " ++ renderListPretty xs)
          <$> zip
            ["executables", "test-suites", "benchmarks"]
            ( filter (not . null) . map sortNub $
                (map (componentNameRaw . availableTargetComponentName) . (`filterTargetsKind` targets) <$> [ExeKind, TestKind, BenchKind])
            )
      )
renderRunProblem (TargetProblemMultipleTargets selectorMap) =
  "The run command is for running a single executable at once. The targets "
    ++ renderListCommaAnd
      [ "'" ++ showTargetSelector ts ++ "'"
      | ts <- uniqueTargetSelectors selectorMap
      ]
    ++ " refer to different executables."
renderRunProblem (TargetProblemComponentNotExe pkgid cname) =
  "The run command is for running executables, but the target '"
    ++ showTargetSelector targetSelector
    ++ "' refers to "
    ++ renderTargetSelector targetSelector
    ++ " from the package "
    ++ prettyShow pkgid
    ++ "."
  where
    targetSelector = TargetComponent pkgid cname WholeComponent
renderRunProblem (TargetProblemIsSubComponent pkgid cname subtarget) =
  "The run command can only run an executable as a whole, "
    ++ "not files or modules within them, but the target '"
    ++ showTargetSelector targetSelector
    ++ "' refers to "
    ++ renderTargetSelector targetSelector
    ++ "."
  where
    targetSelector = TargetComponent pkgid cname subtarget
renderRunProblem (TargetProblemNoExes targetSelector) =
  "Cannot run the target '"
    ++ showTargetSelector targetSelector
    ++ "' which refers to "
    ++ renderTargetSelector targetSelector
    ++ " because "
    ++ plural (targetSelectorPluralPkgs targetSelector) "it does" "they do"
    ++ " not contain any executables."
