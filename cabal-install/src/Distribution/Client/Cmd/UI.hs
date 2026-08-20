{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Distribution.Client.Cmd.UI
  ( cmdSpec
  , commandParserByName
  , parseCommandWithOptparseMany
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Data.List (stripPrefix)
import Data.Monoid (Endo (..))
import qualified System.Console.GetOpt as GetOpt

import Distribution.Client.NixStyleOptions (NixStyleFlags (..))
import Distribution.ReadE (runReadE)
import Distribution.Simple.Command
  ( CommandParse (..)
  , CommandSpec (..)
  , CommandType (NormalCommand)
  , CommandUI (..)
  , OptDescr (..)
  , OptionField (..)
  , ShowOrParseArgs (..)
  , commandAddAction
  , commandParseArgs
  )
import Options.Applicative
  ( ParserInfo
  , ParserResult (..)
  , asum
  , defaultPrefs
  , execParserPure
  , flag'
  , footer
  , fullDesc
  , header
  , help
  , helper
  , info
  , long
  , metavar
  , progDesc
  , renderFailure
  , strArgument
  , (<**>)
  )
import qualified Options.Applicative as O

helpDescriptionOrSynopsis :: CommandUI flags -> String
helpDescriptionOrSynopsis x =
  case commandDescription x of
    Nothing -> commandSynopsis x
    Just mkDescription -> mkDescription "cabal"

data CmdItem a
  = CmdItemFlag (Endo (NixStyleFlags a))
  | CmdItemTarget String
  | CmdItemListOptions

data ParsedCommand a = ParsedCommand
  { parsedFlagEdits :: Endo (NixStyleFlags a)
  , parsedTargets :: [String]
  , parsedListOptions :: Bool
  }

-- | Examples text for a command, given the program name and command name.
type Examples =
  String
  -- ^ program name
  -> String
  -- ^ command name
  -> String
  -- ^ examples text

-- | Replacements for v2- prefixed commands, such as;
--
-- * v2-build -> new-build or
-- * v2-build -> build.
type ReplaceCommandAlias =
  String
  -- ^ the new prefix
  -> String
  -- ^ the command
  -> String
  -- ^ the command name with the prefix replaced

-- | Given a v2- prefixed command name, returns a function for replacing that
-- prefix with a new prefix.
replaceCommandAlias :: String -> ReplaceCommandAlias
replaceCommandAlias = replaceText

-- SEE: generic-sop-lens.hs
replaceText :: String -> String -> String -> String
replaceText needle replacement = go
  where
    go [] = []
    go input@(char : rest)
      | Just remainder <- stripPrefix needle input = replacement ++ go remainder
      | otherwise = char : go rest

-- | Puts a prefix before a bare command name.
affixVersionPrefix :: String -> String -> String
affixVersionPrefix = replaceText "v2-"

-- | Removes the v2- prefix from a command name, leaving the bare command name.
stripVersionPrefix :: String -> String
stripVersionPrefix = affixVersionPrefix ""

-- | Assuming a v2- prefix for a command name, for the 'commandName' of the
-- given command, makes a list that includes the bare name, the new- prefixed
-- name, and the v2- prefixed name.
commandNames :: CommandUI flags -> [String]
commandNames command =
  [ stripVersionPrefix name
  , affixVersionPrefix "new-" name
  , name
  ]
  where
    name = commandName command

cmdSpec
  :: CommandUI flags
  -> (flags -> [String] -> action)
  -> [CommandSpec action]
cmdSpec command action =
  [CommandSpec ui (`commandAddAction` action) NormalCommand]
  where
    ui =
      command
        { commandName = stripVersionPrefix (commandName command)
        , commandUsage = stripVersionPrefix . commandUsage command
        , commandDescription = (stripVersionPrefix .) <$> commandDescription command
        , commandNotes = (stripVersionPrefix .) <$> commandNotes command
        }

cmdListOptions :: CommandUI flags -> [String]
cmdListOptions command =
  case commandParseArgs command False ["--list-options"] of
    CommandList opts -> opts
    _ -> []

parseCommand
  :: Examples
  -> CommandUI (NixStyleFlags a)
  -> (NixStyleFlags a -> [String] -> action)
  -> String
  -> [String]
  -> CommandParse action
parseCommand examples cmdui action invokedName cmdArgs =
  case execParserPure defaultPrefs pInfo (normalizeOptArgs optArgShorts cmdArgs) of
    Success parsed ->
      if parsedListOptions parsed
        then CommandList (cmdListOptions cmdui)
        else
          let flags = appEndo (parsedFlagEdits parsed) (commandDefaultFlags cmdui)
           in CommandReadyToGo (action flags (parsedTargets parsed))
    Failure failure ->
      let (msg, exitCode) = renderFailure failure ("cabal " ++ invokedName)
       in if exitCode == ExitSuccess
            then CommandHelp (helpText (replaceCommandAlias (commandName cmdui)) cmdui invokedName)
            else CommandErrors [msg]
    CompletionInvoked _ ->
      CommandErrors ["Shell completion is not supported by this parser path."]
  where
    fields = commandOptions cmdui ParseArgs
    pInfo = parserInfo invokedName examples flagParsers cmdui
    flagParsers = cmdOptionParsers fields
    optArgShorts = optArgShortNames fields

-- | A parser for one or more command names.
data NamedCommandParser action = NamedCommandParser
  { namedCommandNames :: [String]
  -- ^ The command name and its aliases.
  , namedCommandParser :: String -> [String] -> CommandParse action
  }

-- | Wrap a command's optparse parser together with the names it should match.
commandParserByName
  :: Examples
  -> CommandUI (NixStyleFlags flags)
  -> (NixStyleFlags flags -> [String] -> action)
  -> NamedCommandParser action
commandParserByName examples command action =
  NamedCommandParser
    { namedCommandNames = commandNames command
    , namedCommandParser = \name args -> parseCommand examples command action name args
    }

-- | Parse a command using a list of name/parser associations, picking the first
-- match in the list.
parseCommandWithOptparseMany
  :: CommandUI globalFlags
  -> [NamedCommandParser action]
  -> [String]
  -> Maybe (CommandParse (globalFlags, CommandParse action))
parseCommandWithOptparseMany globalCommand commands argv =
  case commandParseArgs globalCommand True argv of
    CommandReadyToGo (mkGlobalFlags, cmdArgs0) -> do
      cmdName : cmdArgs <- pure cmdArgs0
      parser <- find ((cmdName `elem`) . namedCommandNames) commands
      let globalFlags = mkGlobalFlags (commandDefaultFlags globalCommand)
      pure $ CommandReadyToGo (globalFlags, namedCommandParser parser cmdName cmdArgs)
    _ -> Nothing

parserInfo :: String -> Examples -> [O.Parser (CmdItem a)] -> CommandUI flags -> ParserInfo (ParsedCommand a)
parserInfo invokedName examples flagParsers cmdui =
  info
    (parsedCommandParser flagParsers <**> helper)
    ( fullDesc
        <> progDesc (helpDescriptionOrSynopsis cmdui)
        <> header ("cabal " ++ invokedName)
        <> footer (examples "cabal" invokedName)
    )

parsedCommandParser :: [O.Parser (CmdItem a)] -> O.Parser (ParsedCommand a)
parsedCommandParser flagParsers = toParsed <$> many (cmdItemParser flagParsers)
  where
    toParsed items =
      let edits = [e | CmdItemFlag e <- items]
          targets = [t | CmdItemTarget t <- items]
          listOptionsSeen = any isListOptions items
       in ParsedCommand
            { parsedFlagEdits = mconcat edits
            , parsedTargets = targets
            , parsedListOptions = listOptionsSeen
            }

    isListOptions CmdItemListOptions = True
    isListOptions _ = False

cmdItemParser :: [O.Parser (CmdItem a)] -> O.Parser (CmdItem a)
cmdItemParser flags =
  asum
    ( flags
        ++ [ CmdItemListOptions
              <$ flag'
                ()
                (long "list-options" <> help "Print a list of command line flags")
           , CmdItemTarget <$> strArgument (metavar "TARGET")
           ]
    )

cmdOptionParsers :: [OptionField (NixStyleFlags a)] -> [O.Parser (CmdItem a)]
cmdOptionParsers fields = (fmap . fmap) CmdItemFlag (optionFieldFlagParsers fields)

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
  OptArg desc optFlags@(_shortFlags, longFlags) placeHolder reader (_defaultText, defaultFn) _show ->
    -- An /optional-argument/ option (GetOpt's 'OptArg'), such as
    -- @-j[NUM]@ / @--jobs[=NUM]@. optparse-applicative has no native support
    -- for optional option arguments, so we model the two documented forms
    -- explicitly:
    --
    --   * the bare form (@-j@ / @--jobs@) via 'O.flag'', yielding the default;
    --   * the attached-value form (@--jobs=NUM@) via 'O.option' on the /long/
    --     name only.
    --
    -- The attached /short/ form (@-j4@) is normalised to the long @=@ form by
    -- 'normalizeOptArgs' before parsing. The space-separated form (@-j 4@) is
    -- intentionally not supported: a token following a bare option is treated
    -- as a positional argument, matching what the @--help@ output advertises.
    [ Endo
        <$> asum
          ( O.flag' defaultFn (flagMods optFlags <> O.internal)
              : [ O.option
                  (O.eitherReader (runReadE reader))
                  (mconcat (map O.long longFlags) <> O.metavar placeHolder <> O.help desc <> O.internal)
                | not (null longFlags)
                ]
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

-- | Collect, for every /optional-argument/ option ('OptArg') that has a long
-- name, the mapping from each of its short flags to its (first) long flag.
-- Used by 'normalizeOptArgs' to rewrite the attached short form.
optArgShortNames :: [OptionField flags] -> [(Char, String)]
optArgShortNames = concatMap fieldShorts
  where
    fieldShorts (OptionField _ descrs) = concatMap descrShorts descrs
    descrShorts = \case
      OptArg _ (shortFlags, longFlags) _ _ _ _ ->
        case longFlags of
          (lng : _) -> [(c, lng) | c <- shortFlags]
          [] -> []
      _ -> []

-- | Rewrite the attached short form of an optional-argument option (e.g.
-- @-j4@) into the long @=@ form (@--jobs=4@), which optparse-applicative can
-- parse. Bare options (@-j@), everything after a @--@ terminator, and any
-- token whose leading short flag is not a known optional-argument option are
-- left untouched.
normalizeOptArgs :: [(Char, String)] -> [String] -> [String]
normalizeOptArgs shortToLong = go
  where
    go [] = []
    go ("--" : rest) = "--" : rest
    go (tok : rest) = rewrite tok : go rest
    rewrite tok = case tok of
      ('-' : c : more)
        | not (null more)
        , c /= '-'
        , Just lng <- lookup c shortToLong ->
            "--" ++ lng ++ "=" ++ more
      _ -> tok

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
  BoolOpt desc (shortTrue, longTrue) (shortFalse, longFalse) _setFn _getFn
    | null shortFalse && null longFalse ->
        [GetOpt.Option shortTrue longTrue (GetOpt.NoArg ()) desc]
    | null shortTrue && null longTrue ->
        [GetOpt.Option shortFalse longFalse (GetOpt.NoArg ()) desc]
    | otherwise ->
        [ GetOpt.Option shortTrue longTrue (GetOpt.NoArg ()) ("Enable " <> desc)
        , GetOpt.Option shortFalse longFalse (GetOpt.NoArg ()) ("Disable " <> desc)
        ]

renderOptionRows :: Int -> Int -> Int -> [GetOpt.OptDescr ()] -> (String, [String])
renderOptionRows maxFlagColumnWidth descColumn helpOutputWidth options =
  (concatMap fst rendered, concatMap snd rendered)
  where
    indent = " "
    indentWidth = length indent
    descColumnWidth = indentWidth + descColumn
    descriptionMarker = "# "
    markerPadding = replicate (length descriptionMarker) ' '
    descriptionIndent = replicate descColumnWidth ' '
    descriptionWidth = max 20 (helpOutputWidth - descColumnWidth - length descriptionMarker)

    rendered = map renderOption options

    renderOption opt =
      let (flagColumn, description) = getOptToColumns opt
          wrappedDescription = wrapDescription descriptionWidth description
          isStacked = length flagColumn > maxFlagColumnWidth + 1
          renderedRow =
            if isStacked
              then renderStacked flagColumn wrappedDescription
              else renderInline flagColumn wrappedDescription
       in (renderedRow, [])

    renderInline flagColumn descriptionLines =
      let padding = max 1 (descColumn - length flagColumn)
       in case descriptionLines of
            [] -> indent <> flagColumn <> "\n"
            firstLineText : continuation ->
              let firstLine = indent <> flagColumn <> replicate padding ' ' <> descriptionMarker <> firstLineText <> "\n"
                  continuationLines = [descriptionIndent <> markerPadding <> line <> "\n" | line <- continuation]
               in firstLine <> concat continuationLines

    renderStacked flagColumn descriptionLines =
      case descriptionLines of
        [] -> indent <> flagColumn <> "\n"
        firstLineText : continuation ->
          indent
            <> flagColumn
            <> "\n"
            <> descriptionIndent
            <> descriptionMarker
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

getOptToColumns :: GetOpt.OptDescr () -> (String, String)
getOptToColumns (GetOpt.Option shortFlags longFlags argDescr description) =
  (intercalate ", " (renderShortFlags ++ renderLongFlags), description)
  where
    renderShortFlags = map renderShortFlag shortFlags

    renderShortFlag shortFlag =
      case argDescr of
        GetOpt.NoArg _ -> "-" <> [shortFlag]
        -- Required args can be given as "-w PATH" or "-wPATH" (but not "-w=PATH").
        GetOpt.ReqArg _ metaVar ->
          let opt = "-" <> [shortFlag]
           in opt <> " " <> metaVar <> " or " <> opt <> metaVar
        GetOpt.OptArg _ metaVar -> "-" <> [shortFlag] <> "[" <> metaVar <> "]"

    -- Only the first long flag is rendered. Others are often aliases such as
    -- alternative spellings, such as American versus British spelling. Another
    -- reason they're not shown is we don't wrap the flags and risk overflowing
    -- the width available for display.
    renderLongFlags = map renderLongFlag $ take 1 longFlags

    renderLongFlag longFlag =
      case argDescr of
        GetOpt.NoArg _ -> "--" <> longFlag
        GetOpt.ReqArg _ metaVar -> "--" <> longFlag <> "=" <> metaVar
        GetOpt.OptArg _ metaVar -> "--" <> longFlag <> "[=" <> metaVar <> "]"

helpText :: ReplaceCommandAlias -> CommandUI (NixStyleFlags a) -> String -> String -> String
helpText replaceAlias command invokedName pname =
  commandSynopsis command
    <> "\n\n"
    <> replace (commandUsage command pname)
    <> maybeReplace (commandDescription command)
    <> "\n"
    <> "Flags for "
    <> invokedName
    <> ":"
    <> "\n"
    <> rows
    <> maybeReplace (commandNotes command)
  where
    replace = replaceOthers . replaceAlias invokedName
    maybeReplace = maybe "" (('\n' :) . replace . ($ pname))

    -- A command description may contain references to other v2-prefixed
    -- commands, replace those too.
    replaceOthers =
      if
          | "v2-" `isPrefixOf` invokedName -> id
          | "new-" `isPrefixOf` invokedName -> affixVersionPrefix "new-"
          | otherwise -> stripVersionPrefix

    commonHelpOptions :: [GetOpt.OptDescr ()]
    commonHelpOptions =
      [GetOpt.Option ['h'] ["help"] (GetOpt.NoArg ()) "Show this help text"]

    maxFlagColumnWidth :: Int
    maxFlagColumnWidth = 27

    helpOutputWidth :: Int
    helpOutputWidth = 78

    allOptions :: [GetOpt.OptDescr ()]
    allOptions = commonHelpOptions ++ concatMap optionFieldToGetOpt (commandOptions command ShowArgs)

    maxFlagColumns :: Int
    maxFlagColumns = maximum (0 : map (length . fst . getOptToColumns) allOptions)

    descColumn :: Int
    descColumn = min maxFlagColumnWidth maxFlagColumns + 2

    (rows, _) =
      renderOptionRows
        maxFlagColumnWidth
        descColumn
        helpOutputWidth
        (commonHelpOptions ++ concatMap optionFieldToGetOpt (commandOptions command ShowArgs))
