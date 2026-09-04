{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Telling target strings apart from the arguments they arrive mixed with.
--
-- Commands that run something — @cabal run@, and in principle @cabal test@ and
-- @cabal bench@ — receive one flat list holding both the target and the
-- arguments meant for the thing being run. Neither the @--@ separator nor
-- target resolution can split that list alone: counting the strings around
-- @--@ loses a target given only after it
-- (<https://github.com/haskell/cabal/issues/12231>), and resolution alone
-- would let an unrecognised word silently become an argument.
--
-- So every string that is not a flag is probed against the project, and the
-- split, the warnings and the errors are all read off that one classification.
--
-- 'targetArgSplitter' is the whole of it for a command that just wants the
-- answer; the pieces below are exposed for tests and for a command that needs
-- to look at the evidence itself.
module Distribution.Client.TargetArgs
  ( -- * Splitting a command line
    TargetArgSplitter
  , targetArgSplitter

    -- * The pieces it is built from
  , ArgKind (..)
  , ClassifiedArg (..)
  , TargetAndArgs (..)
  , classifyArgs
  , separatorPosition
  , splitTargetAndArgs

    -- * Probing individual strings
  , TargetMatch (..)
  , TargetOracle (..)
  , newTargetOracle
  , knownTargetOracle

    -- * Diagnostics
  , runnableComponents
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import qualified Data.Map as Map
import Distribution.Client.CmdErrorMessages
  ( listPlural
  , plural
  , renderListCommaAnd
  )
import Distribution.Client.ProjectOrchestration
import Distribution.Client.TargetProblem
  ( TargetProblem'
  )
import Distribution.Client.TargetSelector
  ( ComponentKindFilter
  , DirActions (..)
  , defaultDirActions
  , newTargetSelectorReader
  , readTargetSelectorsWith
  )
import Distribution.Client.Types
  ( PackageSpecifier
  , UnresolvedSourcePackage
  )
import Distribution.Package
import Distribution.Simple.Utils
  ( notice
  , sortNub
  , warn
  )
import Distribution.Types.ComponentName
  ( componentNameRaw
  )

-------------------------------------------------------------------------------
-- Probing individual strings
-------------------------------------------------------------------------------

-- | What a single string turned out to be.
data TargetMatch
  = -- | It names something in the project.
    MatchSelector TargetSelector
  | -- | It does not name anything in the project but is an existing file, so
    -- it may be a script. This is the same tie-break
    -- 'Distribution.Client.ScriptUtils.withContextAndSelectors' applies.
    MatchScript FilePath
  deriving (Eq, Ord, Show)

-- | Probes one string at a time. Build it once per command with
-- 'newTargetOracle' and reuse it: the known-target index it closes over is the
-- expensive part.
newtype TargetOracle m = TargetOracle
  { probeTarget :: String -> m (Maybe TargetMatch)
  }

-- | Build an oracle over the project's local packages.
--
-- The staging that keeps this cheap belongs to
-- 'Distribution.Client.TargetSelector.newTargetSelectorReader': the
-- known-target index is built once here and reused for every string.
newTargetOracle
  :: Monad m
  => DirActions m
  -> [PackageSpecifier UnresolvedSourcePackage]
  -> Maybe ComponentKindFilter
  -- ^ Used only to disambiguate an otherwise ambiguous string.
  -> m (TargetOracle m)
newTargetOracle dirActions pkgs mfilter = do
  readSelector <- newTargetSelectorReader dirActions pkgs mfilter
  return . TargetOracle $ \s -> do
    resolved <- readSelector s
    case resolved of
      Right selector | isProjectSelector selector -> return (Just (MatchSelector selector))
      _ -> do
        exists <- doesFileExist dirActions s
        return $ if exists then Just (MatchScript s) else Nothing

-- | Did this selector actually match something in the project?
--
-- Resolution succeeds for any bare word, handing back a 'TargetPackageNamed'
-- that stands for \"a package by this name, from wherever\" and is only
-- checked later against the plan. That is the right answer for a command that
-- can reach beyond the project, but it is not what \"is this string a target?\"
-- means here: taking it as a yes would make every unrecognised word a target.
-- 'TargetComponentUnknown' is unverified in the same way.
isProjectSelector :: TargetSelector -> Bool
isProjectSelector selector = case selector of
  TargetPackage{} -> True
  TargetAllPackages{} -> True
  TargetComponent{} -> True
  TargetPackageNamed{} -> False
  TargetComponentUnknown{} -> False

-- | An oracle recognising a fixed set of names, for doctests and tests.
knownTargetOracle :: Applicative m => [String] -> TargetOracle m
knownTargetOracle known = TargetOracle $ \s ->
  pure $
    if s `elem` known
      then Just (MatchSelector (TargetPackageNamed (mkPackageName s) Nothing))
      else Nothing

-------------------------------------------------------------------------------
-- Classifying and splitting
-------------------------------------------------------------------------------

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
    targets = case sep of
      -- Something precedes the separator, so the user has said where the
      -- executable's arguments begin: everything before it is a target.
      -- Only targets and flags belong there, and the option parser has
      -- already taken the flags, so anything left that does not name a
      -- target is reported as the unrecognised target it is.
      Just n | n > 0 -> take n classified
      -- A separator with nothing before it. The target, if any, is still
      -- among what follows, which is what keeps a target given only after
      -- @--@ reachable.
      Just _ -> takeWhile isTargetArg classified
      -- No separator, so the user has not signalled that arguments follow.
      -- The leading word is then a target claim even when it does not
      -- resolve, and fails with the usual unrecognised-target error and its
      -- suggestions. A leading flag claims nothing, so it is exempt.
      Nothing -> case (takeWhile isTargetArg classified, classified) of
        ([], ca : _) | caKind ca /= ArgFlag -> [ca]
        (resolved, _) -> resolved

    args = drop (length targets) classified

-------------------------------------------------------------------------------
-- Splitting a whole command line
-------------------------------------------------------------------------------

-- | Separate target strings from the arguments they are mixed with, given the
-- project's local packages.
--
-- Commands like @cabal run@ receive one list holding both, and can only tell
-- them apart by asking whether a string names a target — which needs the
-- project context.
type TargetArgSplitter =
  [PackageSpecifier UnresolvedSourcePackage] -> [String] -> IO ([String], [String])

-- | The whole split for a command that just wants the answer: probe, split,
-- and say out loud anything the user is unlikely to have intended.
--
-- A command adopts this by supplying the kind of component it runs and a noun
-- for its messages. @cabal run@ passes @(Just ExeKind) \"executable\"@; @cabal
-- test@ would pass @(Just TestKind) \"test suite\"@.
targetArgSplitter
  :: Verbosity
  -> [String]
  -- ^ The full command line from @getFullArgs@, the only place the @--@
  -- separator survives, as the option parser drops it.
  -> Maybe ComponentKindFilter
  -- ^ The kind of component this command runs.
  -> String
  -- ^ What the arguments are destined for, for the messages.
  -> TargetArgSplitter
targetArgSplitter verbosity fullArgs mfilter runs localPackages targetAndArgs = do
  oracle <- newTargetOracle defaultDirActions localPackages mfilter
  split <- splitTargetAndArgs oracle fullArgs targetAndArgs
  reportClassification verbosity localPackages mfilter runs split
  return (taTargets split, taArgs split)

-- | Say out loud anything about the split that the user is unlikely to have
-- intended. Nothing here changes the split; it only explains it.
reportClassification
  :: Verbosity
  -> [PackageSpecifier UnresolvedSourcePackage]
  -> Maybe ComponentKindFilter
  -> String
  -> TargetAndArgs
  -> IO ()
reportClassification verbosity localPackages mfilter runs TargetAndArgs{..} = do
  -- A string that names a component but was left on the argument side, without
  -- the user having put a '--' in front of it.
  unless (null namedButPassed) $
    warn verbosity $
      renderListCommaAnd (map (\s -> "'" ++ s ++ "'") namedButPassed)
        ++ " "
        ++ plural (listPlural namedButPassed) "names a component" "name components"
        ++ " in this project but "
        ++ plural (listPlural namedButPassed) "is" "are"
        ++ " being passed to the "
        ++ runs
        ++ " as "
        ++ plural (listPlural namedButPassed) "an argument" "arguments"
        ++ ". Put '--' before "
        ++ plural (listPlural namedButPassed) "it" "them"
        ++ " to silence this, or move the target to the front."

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

    leadingIsPlain = case taClassified of
      ca : _ -> caKind ca == ArgPlain && not (null taTargets)
      [] -> False

    soleRunnableComponent = do
      selectors <- readTargetSelectorsWith defaultDirActions localPackages mfilter []
      return $ case selectors of
        Right sels -> case runnableComponents localPackages sels of
          [cname] -> Just cname
          _ -> Nothing
        Left _ -> Nothing

-------------------------------------------------------------------------------
-- Diagnostics
-------------------------------------------------------------------------------

-- | The executable-like components (executables, test suites and benchmarks)
-- that the given selectors reach, using only the local packages so that no
-- install plan is needed.
--
-- This is for enriching messages — naming the candidates when no target was
-- given, say. It deliberately plays no part in deciding whether a string is a
-- target: a package with two buildable executables is still a target, and
-- demoting it to an argument would replace a good \"matches multiple\" error
-- with a baffling one.
runnableComponents
  :: [PackageSpecifier UnresolvedSourcePackage]
  -> [TargetSelector]
  -> [ComponentName]
runnableComponents pkgs selectors =
  case resolveTargetsFromLocalPackages selectRunnable selectComponentTargetBasic pkgs selectors of
    Left _ -> []
    Right targets ->
      sortNub
        [ cname
        | (_, cts) <- Map.toList targets
        , (ComponentTarget cname _, _) <- cts
        ]

selectRunnable
  :: forall k
   . TargetSelector
  -> [AvailableTarget k]
  -> Either TargetProblem' [k]
selectRunnable _ targets =
  Right . selectBuildableTargets $
    concatMap (`filterTargetsKind` targets) [ExeKind, TestKind, BenchKind]
