{-# LANGUAGE LambdaCase #-}

module Distribution.Client.Cmd.UI
  ( -- * Converting CommandUI options to optparse-applicative parsers
    optionFieldFlagParsers
  , optionFieldParser
  , optDescrParser
  , optionMods
  , flagMods

    -- * Converting CommandUI options to GetOpt descriptions
  , optionFieldToGetOpt
  , optDescrToGetOpt

    -- * Help text layout helpers
  , renderOptionRows
  , getOptToColumns
  , wrapDescription
  , capitalizeDescription
  , helpText

    -- * Option grouping helpers
  , groupPredicates
  , groupSequentially
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Data.Char (isLower)
import Data.List (findIndex, isInfixOf, mapAccumL, sortOn, stripPrefix, tails)
import Data.Monoid (Endo (..))
import qualified Data.Text as T
import qualified System.Console.GetOpt as GetOpt

import Distribution.Client.NixStyleOptions
  ( NixStyleFlags (..)
  , keepBenchOptions
  , keepCompilerOptions
  , keepConfigureOptions
  , keepCoverageOptions
  , keepExeOptions
  , keepHaddockOptions
  , keepIncludeOptions
  , keepInstallOptions
  , keepIrrelevantOptions
  , keepLibOptions
  , keepLoggingOptions
  , keepOutputOptions
  , keepPhaseOptions
  , keepProfilingOptions
  , keepProgOptions
  , keepSolvingOptions
  , keepTestOptions
  , keepUnsupportedOptions
  )
import Distribution.ReadE (runReadE)
import Distribution.Simple.Command
  ( CommandUI (..)
  , OptDescr (..)
  , OptionField (..)
  , ShowOrParseArgs (ShowArgs)
  )
import Distribution.Simple.Utils (ordNub)

import qualified Options.Applicative as O

optionFieldFlagParsers :: [OptionField flags] -> [O.Parser (Endo flags)]
optionFieldFlagParsers = concatMap optionFieldParser

optionFieldParser :: OptionField flags -> [O.Parser (Endo flags)]
optionFieldParser (OptionField _ descrs) = concatMap optDescrParser descrs

optDescrParser :: OptDescr flags -> [O.Parser (Endo flags)]
optDescrParser = \case
  ReqArg desc optFlags placeHolder reader _show ->
    [ Endo
        <$> O.option
          (O.eitherReader (runReadE reader))
          (optionMods optFlags <> O.metavar placeHolder <> O.help desc)
    ]
  OptArg desc optFlags placeHolder reader (_defaultText, defaultFn) _show ->
    [ Endo
        <$> ( O.option
                (O.eitherReader (runReadE reader))
                (optionMods optFlags <> O.metavar placeHolder <> O.help desc)
                <|> O.flag' defaultFn (flagMods optFlags <> O.internal)
            )
    ]
  ChoiceOpt choices ->
    [ Endo setFn
      <$ O.flag' () (flagMods optFlags <> O.help desc)
    | (desc, optFlags, setFn, _get) <- choices
    ]
  BoolOpt desc trueFlags falseFlags setFn _get ->
    [ Endo (setFn True)
        <$ O.flag' () (flagMods trueFlags <> O.help desc)
    , Endo (setFn False)
        <$ O.flag' () (flagMods falseFlags <> O.help desc)
    ]

optionMods :: (String, [String]) -> O.Mod O.OptionFields a
optionMods (shortFlags, longFlags) =
  mconcat (map O.short shortFlags) <> mconcat (map O.long longFlags)

flagMods :: (String, [String]) -> O.Mod O.FlagFields a
flagMods (shortFlags, longFlags) =
  mconcat (map O.short shortFlags) <> mconcat (map O.long longFlags)

optionFieldToGetOpt :: OptionField flags -> [GetOpt.OptDescr ()]
optionFieldToGetOpt (OptionField _ descrs) = concatMap optDescrToGetOpt descrs

optDescrToGetOpt :: OptDescr flags -> [GetOpt.OptDescr ()]
optDescrToGetOpt = \case
  ReqArg desc (shortFlags, longFlags) placeHolder _reader _showFlag ->
    [GetOpt.Option shortFlags longFlags (GetOpt.ReqArg (const ()) placeHolder) desc]
  OptArg desc (shortFlags, longFlags) placeHolder _reader (_defaultValue, _defaultSetter) _showFlag ->
    [GetOpt.Option shortFlags longFlags (GetOpt.OptArg (const ()) placeHolder) desc]
  ChoiceOpt choices ->
    [ GetOpt.Option shortFlags longFlags (GetOpt.NoArg ()) desc
    | (desc, (shortFlags, longFlags), _setFn, _getFn) <- choices
    ]
  BoolOpt desc trueFlags@(shortTrue, longTrue) falseFlags@(shortFalse, longFalse) _setFn _getFn
    | null shortFalse && null longFalse ->
        [GetOpt.Option shortTrue longTrue (GetOpt.NoArg ()) desc]
    | null shortTrue && null longTrue ->
        [GetOpt.Option shortFalse longFalse (GetOpt.NoArg ()) desc]
    | Just groupedLongFlag <- mkGroupedBoolLongFlag trueFlags falseFlags ->
        [GetOpt.Option [] [groupedLongFlag] (GetOpt.NoArg ()) ("Toggle " <> desc)]
    | otherwise ->
        [ GetOpt.Option shortTrue longTrue (GetOpt.NoArg ()) ("Enable " <> desc)
        , GetOpt.Option shortFalse longFalse (GetOpt.NoArg ()) ("Disable " <> desc)
        ]

mkGroupedBoolLongFlag :: (String, [String]) -> (String, [String]) -> Maybe String
mkGroupedBoolLongFlag ([], [longA]) ([], [longB]) =
  checkPair longA longB <|> checkPair longB longA
  where
    checkPair longEnable longDisable = do
      suffixEnable <- stripPrefix "enable-" longEnable
      suffixDisable <- stripPrefix "disable-" longDisable
      guard (suffixEnable == suffixDisable)
      pure ("[enable|disable]-" <> suffixEnable)
mkGroupedBoolLongFlag _ _ = Nothing

renderOptionRows :: (String -> String) -> Int -> Int -> Int -> [GetOpt.OptDescr ()] -> (String, [String])
renderOptionRows colorizeWarning maxFlagColumnWidth descColumn helpOutputWidth options =
  let rendered = [renderOption (index == 0) opt | (index, opt) <- zip [0 :: Int ..] options]
   in (concatMap fst rendered, concatMap snd rendered)
  where
    descriptionMarker = "• "
    markerPadding = replicate (length descriptionMarker) ' '
    descriptionIndent = replicate (2 + descColumn) ' '
    descriptionWidth = max 20 (helpOutputWidth - (2 + descColumn) - length descriptionMarker)

    renderOption isFirstInGroup opt =
      let (flagColumn, description) = getOptToColumns opt
          (descriptionItems, warning) =
            case specialCaseTestShowDetailsDescriptions flagColumn description of
              Just descriptions -> (map (wrapDescription descriptionWidth) descriptions, [])
              Nothing ->
                let (capitalizedDescription, wasAutoCapitalized) = capitalizeDescription description
                    wrappedDescription = wrapDescription descriptionWidth capitalizedDescription
                    displayDescription =
                      if wasAutoCapitalized
                        then colorizeFirstAlpha wrappedDescription
                        else wrappedDescription
                 in ([displayDescription], ["Auto-capitalized help text for " <> flagColumn | wasAutoCapitalized])
          isStacked = length flagColumn > maxFlagColumnWidth
          spacer = if isStacked && not isFirstInGroup then "\n" else ""
          renderedRow =
            spacer
              <> if isStacked
                then renderStacked flagColumn descriptionItems
                else renderInline flagColumn descriptionItems
       in (renderedRow, warning)

    colorizeFirstAlpha :: [String] -> [String]
    colorizeFirstAlpha = go
      where
        go [] = []
        go (line : rest) =
          case colorizeFirstAlphaInLine line of
            Nothing -> line : go rest
            Just colored -> colored : rest

        colorizeFirstAlphaInLine :: String -> Maybe String
        colorizeFirstAlphaInLine = scan []
          where
            scan _ [] = Nothing
            scan acc (ch : cs)
              | isAlpha ch = Just (reverse acc <> colorizeWarning [ch] <> cs)
              | otherwise = scan (ch : acc) cs

    renderInline flagColumn descriptionItems =
      let padding = max 1 (descColumn - length flagColumn)
       in case descriptionItems of
            [] -> "  " <> flagColumn <> "\n"
            firstItem : restItems ->
              renderFirstInlineItem flagColumn padding firstItem
                <> concatMap renderContinuationItem restItems

    renderStacked flagColumn descriptionItems =
      case descriptionItems of
        [] -> "  " <> flagColumn <> "\n"
        firstItem : restItems ->
          "  "
            <> flagColumn
            <> "\n"
            <> renderIndentedItem descriptionMarker firstItem
            <> concatMap (renderIndentedItem descriptionMarker) restItems

    renderFirstInlineItem flagColumn padding = \case
      [] -> "  " <> flagColumn <> replicate padding ' ' <> descriptionMarker <> "\n"
      firstLineText : continuation ->
        "  "
          <> flagColumn
          <> replicate padding ' '
          <> descriptionMarker
          <> firstLineText
          <> "\n"
          <> concat [descriptionIndent <> markerPadding <> line <> "\n" | line <- continuation]

    renderContinuationItem = renderIndentedItem descriptionMarker

    renderIndentedItem marker = \case
      [] -> descriptionIndent <> marker <> "\n"
      firstLineText : continuation ->
        descriptionIndent
          <> marker
          <> firstLineText
          <> "\n"
          <> concat [descriptionIndent <> markerPadding <> line <> "\n" | line <- continuation]

wrapDescription :: Int -> String -> [String]
wrapDescription width description =
  case concatMap wrapParagraph (lines description) of
    [] -> [""]
    wrapped -> wrapped
  where
    wrapParagraph paragraph
      | null ws = [""]
      | otherwise = reverse (foldl' step [""] ws)
      where
        ws = words paragraph

        step (current : previous) word
          | null current = word : previous
          | length current + 1 + length word <= width = (current <> " " <> word) : previous
          | otherwise = word : current : previous
        step [] _ = []

capitalizeDescription :: String -> (String, Bool)
capitalizeDescription = go []
  where
    go acc [] = (reverse acc, False)
    go acc (ch : rest)
      | isAlpha ch =
          if isLower ch
            then (reverse acc <> (toUpper ch : rest), True)
            else (reverse acc <> (ch : rest), False)
      | otherwise = go (ch : acc) rest

specialCaseTestShowDetailsDescriptions :: String -> String -> Maybe [String]
specialCaseTestShowDetailsDescriptions flagColumn description
  | isTestShowDetailsFlag flagColumn =
      case extractLabeledDescriptions description of
        [] -> Nothing
        descriptions -> Just descriptions
  | otherwise = Nothing
  where
    isTestShowDetailsFlag flags =
      "--test-show-details=FILTER" `isInfixOf` flags
        || "--show-details=FILTER" `isInfixOf` flags

extractLabeledDescriptions :: String -> [String]
extractLabeledDescriptions description =
  let labels = ["always", "never", "failures", "streaming", "direct"]
      markers = [(label, "'" <> label <> "':") | label <- labels]
      positions =
        [ (position, marker)
        | (_label, marker) <- markers
        , Just position <- [findSubstring marker description]
        ]
      sorted = sortOn fst positions
   in case sorted of
        [] -> []
        _ ->
          let starts = map fst sorted
              marked = zip sorted (drop 1 starts ++ [length description])
           in
            [ let body = trim (take (endPos - startPos - length marker) (drop (startPos + length marker) description))
               in marker <> " " <> body
            | (((startPos, marker), endPos)) <- marked
            ]

findSubstring :: String -> String -> Maybe Int
findSubstring needle haystack
  | null needle = Just 0
  | otherwise = findIndex (isJust . stripPrefix needle) (tails haystack)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

getOptToColumns :: GetOpt.OptDescr () -> (String, String)
getOptToColumns (GetOpt.Option shortFlags longFlags argDescr description) =
  (intercalate ", " (renderShortFlags ++ renderLongFlags), description)
  where
    renderShortFlags = map renderShortFlag shortFlags

    renderShortFlag shortFlag =
      case argDescr of
        GetOpt.NoArg _ -> "-" <> [shortFlag]
        GetOpt.ReqArg _ metaVar -> "-" <> [shortFlag] <> " " <> metaVar
        GetOpt.OptArg _ metaVar -> "-" <> [shortFlag] <> "[" <> metaVar <> "]"

    renderLongFlags = map renderLongFlag longFlags

    renderLongFlag longFlag =
      case argDescr of
        GetOpt.NoArg _ -> "--" <> longFlag
        GetOpt.ReqArg _ metaVar -> "--" <> longFlag <> "=" <> metaVar
        GetOpt.OptArg _ metaVar -> "--" <> longFlag <> "[=" <> metaVar <> "]"

groupSequentially :: [a] -> [(groupName, a -> Bool)] -> ([(groupName, [a])], [a])
groupSequentially options groupingSpecs =
  let step remaining (groupName, keepPred) =
        let (groupMembers, leftovers) = partition keepPred remaining
         in (leftovers, (groupName, groupMembers))
      (leftoverOptions, groupedBuckets) = mapAccumL step options groupingSpecs
   in (groupedBuckets, leftoverOptions)

data OptionGroupKey
  = UnsupportedOptions
  | InstallLayoutOptions
  | IrrelevantOptions
  | HaddockOptions
  | TestOptions
  | BenchmarkOptions
  | ProfilingOptions
  | DependencySolvingOptions
  | ExecutableBuildOptions
  | LibraryBuildOptions
  | CoverageOptions
  | OutputAndArtifactOptions
  | ConfigurePhaseOptions
  | BuildPhaseControlOptions
  | CompilerAndParallelismOptions
  | LoggingAndReportingOptions
  | IncludeAndLinkerPathOptions
  | ProgramOverrideOptions
  deriving (Eq)

instance Show OptionGroupKey where
  show UnsupportedOptions = "Unsupported options"
  show InstallLayoutOptions = "Install layout options"
  show IrrelevantOptions = "Irrelevant options"
  show HaddockOptions = "Haddock options"
  show TestOptions = "Test options"
  show BenchmarkOptions = "Benchmark options"
  show ProfilingOptions = "Profiling options"
  show DependencySolvingOptions = "Dependency solving options"
  show ExecutableBuildOptions = "Executable build options"
  show LibraryBuildOptions = "Library build options"
  show CoverageOptions = "Coverage options"
  show OutputAndArtifactOptions = "Output and artifact options"
  show ConfigurePhaseOptions = "Configure-phase options"
  show BuildPhaseControlOptions = "Build phase control options"
  show CompilerAndParallelismOptions = "Compiler and parallelism options"
  show LoggingAndReportingOptions = "Logging and reporting options"
  show IncludeAndLinkerPathOptions = "Include and linker path options"
  show ProgramOverrideOptions = "Program override options"

groupPredicates :: [(OptionGroupKey, OptionField a -> Bool)]
groupPredicates =
  [ (UnsupportedOptions, keepUnsupportedOptions)
  , (InstallLayoutOptions, keepInstallOptions)
  , (IrrelevantOptions, keepIrrelevantOptions)
  , (HaddockOptions, keepHaddockOptions)
  , (TestOptions, keepTestOptions)
  , (BenchmarkOptions, keepBenchOptions)
  , (ProfilingOptions, keepProfilingOptions)
  , (DependencySolvingOptions, keepSolvingOptions)
  , (ExecutableBuildOptions, keepExeOptions)
  , (LibraryBuildOptions, keepLibOptions)
  , (CoverageOptions, keepCoverageOptions)
  , (OutputAndArtifactOptions, keepOutputOptions)
  , (ConfigurePhaseOptions, keepConfigureOptions)
  , (BuildPhaseControlOptions, keepPhaseOptions)
  , (CompilerAndParallelismOptions, keepCompilerOptions)
  , (LoggingAndReportingOptions, keepLoggingOptions)
  , (IncludeAndLinkerPathOptions, keepIncludeOptions)
  , (ProgramOverrideOptions, keepProgOptions)
  ]

type ReplaceCommandAlias = String -> String -> String

helpText :: ReplaceCommandAlias -> CommandUI (NixStyleFlags a) -> String -> String -> String
helpText replaceBuildAlias buildCommand invokedName pname =
  commandSynopsis buildCommand
    <> "\n\n"
    <> colorizeUsageHeader (replaceBuildAlias invokedName (commandUsage buildCommand pname))
    <> maybe "" (('\n' :) . ($ pname)) (commandDescription buildCommand)
    <> "\n"
    <> colorizeHeader "Flags for build:"
    <> "\n"
    <> ungroupedRows
    <> groupedRows
    <> warningSection
    <> maybe "" (('\n' :) . colorizeExamplesHeader . replaceBuildAlias invokedName . ($ pname)) (commandNotes buildCommand)
  where
    commonHelpOptions :: [GetOpt.OptDescr ()]
    commonHelpOptions =
      [GetOpt.Option ['h'] ["help"] (GetOpt.NoArg ()) "Show this help text"]

    maxFlagColumnWidth :: Int
    maxFlagColumnWidth = 30

    helpOutputWidth :: Int
    helpOutputWidth = 100

    allOptions :: [GetOpt.OptDescr ()]
    allOptions =
      commonHelpOptions
        ++ concatMap optionFieldToGetOpt optsUngrouped
        ++ concatMap (concatMap optionFieldToGetOpt . snd) optsGrouped

    descColumn :: Int
    descColumn =
      min
        maxFlagColumnWidth
        ( maximum
            ( 0
                : map
                  (length . fst . getOptToColumns)
                  allOptions
            )
        )
        + 2

    (ungroupedRows, ungroupedWarnings) =
      renderOptionRows
        colorizeWarningHeader
        maxFlagColumnWidth
        descColumn
        helpOutputWidth
        (commonHelpOptions ++ concatMap optionFieldToGetOpt optsUngrouped)

    renderGroupToWidth = renderGroup maxFlagColumnWidth descColumn helpOutputWidth
    renderedGroups = map renderGroupToWidth optsGrouped

    groupedRows = concatMap fst renderedGroups

    groupedWarnings = concatMap snd renderedGroups

    warningSection =
      case ungroupedWarnings ++ groupedWarnings of
        [] -> ""
        warnings ->
          "\n"
            <> colorizeWarningHeader "Warnings:"
            <> "\n"
            <> concat ["  - " <> warning <> "\n" | warning <- warnings]

    (optsGrouped, optsUngrouped) =
      groupSequentially (commandOptions buildCommand ShowArgs) groupPredicates

renderGroup :: Int -> Int -> Int -> (OptionGroupKey, [OptionField a]) -> (String, [String])
renderGroup maxFlagColumnWidth descColumn helpOutputWidth (title, options)
  | null options = ("", [])
  | title == InstallLayoutOptions = renderInstallLayoutGroupCompact helpOutputWidth options
  | otherwise =
      let (rows, warnings) =
            renderOptionRows
              colorizeWarningHeader
              maxFlagColumnWidth
              descColumn
              helpOutputWidth
              (concatMap optionFieldToGetOpt options)
       in ( "\n"
              <> colorizeHeader (show title <> ":")
              <> "\n"
              <> rows
          , warnings
          )

renderInstallLayoutGroupCompact :: Int -> [OptionField a] -> (String, [String])
renderInstallLayoutGroupCompact helpOutputWidth options =
  ( "\n"
      <> colorizeHeader (show InstallLayoutOptions <> ":")
      <> "\n"
      <> concat ["  " <> line <> "\n" | line <- wrappedFlagLines]
  , []
  )
  where
    flagColumns = map (fst . getOptToColumns) (concatMap optionFieldToGetOpt options)
    compactFlags = ordNub flagColumns
    flagsLine = intercalate ", " compactFlags
    wrappedFlagLines = wrapDescription (max 40 (helpOutputWidth - 2)) flagsLine

colorizeHeader :: String -> String
colorizeHeader text = "\ESC[32m" <> text <> "\ESC[0m"

colorizeWarningHeader :: String -> String
colorizeWarningHeader text = "\ESC[31m" <> text <> "\ESC[0m"

colorizeUsageHeader :: String -> String
colorizeUsageHeader = T.unpack . T.replace (T.pack "Usage:") (T.pack $ colorizeHeader "Usage:") . T.pack

colorizeExamplesHeader :: String -> String
colorizeExamplesHeader = T.unpack . T.replace (T.pack "Examples:") (T.pack $ colorizeHeader "Examples:") . T.pack
