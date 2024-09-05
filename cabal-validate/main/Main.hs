module Main where

import Control.Applicative (Alternative (many, (<|>)), (<**>))
import Control.Monad (forM_, unless)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (DiffTime, secondsToDiffTime)
import Data.Time.Clock.System (getSystemTime, systemToTAITime)
import Data.Time.Clock.TAI (AbsoluteTime, diffAbsoluteTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Version (Version, makeVersion, parseVersion, showVersion)
import GHC.Conc (getNumCapabilities)
import Options.Applicative
   ( FlagFields
   , Mod
   , Parser
   , ParserInfo
   , auto
   , execParser
   , flag
   , flag'
   , fullDesc
   , help
   , helper
   , hidden
   , info
   , long
   , maybeReader
   , option
   , progDesc
   , short
   , strOption
   , switch
   , value
   )
import System.Console.ANSI
   ( Color (Blue, Cyan, Green, Magenta, Red, Yellow)
   , ColorIntensity (Vivid)
   , ConsoleIntensity (BoldIntensity)
   , ConsoleLayer (Foreground)
   , SGR (Reset, SetColor, SetConsoleIntensity)
   , setSGRCode
   )
import System.Console.Terminal.Size (Window (width), size)
import System.Directory (getCurrentDirectory)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitFailure, exitSuccess)
import System.Info (arch, fullCompilerVersion, os)
import Text.ParserCombinators.ReadP (readP_to_S)
import Turtle (proc, procStrict, when, (</>))

-- | Command-line options, resolved with context from the environment.
data ResolvedOpts = ResolvedOpts
   { verbose :: Bool
   , jobs :: Int
   , cwd :: FilePath
   , startTime :: AbsoluteTime
   , compiler :: Compiler
   , extraCompilers :: [FilePath]
   , cabal :: FilePath
   , hackageTests :: HackageTests
   , archPath :: FilePath
   , projectFile :: FilePath
   , targets :: [String]
   , steps :: [Step]
   }
   deriving (Show)

data Compiler = Compiler
   { executable :: FilePath
   , version :: Version
   }
   deriving (Show)

makeCompiler :: FilePath -> IO Compiler
makeCompiler executable = do
   -- TODO: Check the exit code!
   (_exitCode, stdout) <- procStrict (T.pack executable) ["--numeric-version"] ""
   let version = T.unpack $ T.strip stdout
       parsedVersions = readP_to_S parseVersion version
       -- TODO: Check for empty lists, check for non-empty remaining input.
       -- Who needs error messages? Those aren't in the API.
       parsedVersion =
         head
            [ parsed
            | (parsed, rest) <- parsedVersions
            , null rest
            ]
   pure
      Compiler
         { executable = executable
         , version = parsedVersion
         }

baseHc :: ResolvedOpts -> FilePath
baseHc opts = "ghc-" <> showVersion opts.compiler.version

baseBuildDir :: ResolvedOpts -> FilePath
baseBuildDir opts = "dist-newstyle-validate-" <> baseHc opts

buildDir :: ResolvedOpts -> FilePath
buildDir opts =
   opts.cwd
      </> baseBuildDir opts
      </> "build"
      </> opts.archPath
      </> baseHc opts

cabalArgs :: ResolvedOpts -> [Text]
cabalArgs opts =
   [ "--jobs=" <> T.pack (show opts.jobs)
   , "--with-compiler=" <> T.pack (baseHc opts)
   , "--builddir=" <> T.pack (baseBuildDir opts)
   , "--project-file=" <> T.pack opts.projectFile
   ]

cabalTestsuiteBasedir :: ResolvedOpts -> FilePath
cabalTestsuiteBasedir opts =
   buildDir opts
      </> "cabal-testsuite-3"

cabalNewBuildArgs :: ResolvedOpts -> [Text]
cabalNewBuildArgs opts = ["build"] ++ cabalArgs opts

cabalListBinArgs :: ResolvedOpts -> [Text]
cabalListBinArgs opts = ["list-bin"] ++ cabalArgs opts

rtsArgs :: ResolvedOpts -> [Text]
rtsArgs opts =
   case opts.archPath of
      "x86_64-windows" ->
         -- See: https://github.com/haskell/cabal/issues/9571
         if fullCompilerVersion > makeVersion [9, 0, 2]
            then ["+RTS", "--io-manager=native", "-RTS"]
            else []
      _ -> []

resolveOpts :: Opts -> IO ResolvedOpts
resolveOpts opts = do
   let optionals :: Bool -> [a] -> [a]
       optionals True items = items
       optionals False _ = []

       optional :: Bool -> a -> [a]
       optional keep item = optionals keep [item]

       steps =
         if not (null opts.steps)
            then opts.steps
            else
               concat
                  [
                     [ PrintConfig
                     , PrintToolVersions
                     , Build
                     ]
                  , optional opts.doctest Doctest
                  , optional opts.runLibTests LibTests
                  , optional opts.runLibSuite LibSuite
                  , optional (opts.runLibSuite && not (null opts.extraCompilers)) LibSuiteExtras
                  , optional (opts.runCliTests && not opts.libOnly) CliTests
                  , optional (opts.runCliSuite && not opts.libOnly) CliSuite
                  , optionals opts.solverBenchmarks [SolverBenchmarksTests, SolverBenchmarksRun]
                  ]

       targets =
         concat
            [
               [ "Cabal"
               , "Cabal-hooks"
               , "cabal-testsuite"
               , "Cabal-tests"
               , "Cabal-QuickCheck"
               , "Cabal-tree-diff"
               , "Cabal-described"
               ]
            , optionals
               (CliTests `elem` steps)
               [ "cabal-install"
               , "cabal-install-solver"
               , "cabal-benchmarks"
               ]
            , optional opts.solverBenchmarks "solver-benchmarks"
            ]

       archPath =
         let osPath =
               case os of
                  "darwin" -> "osx"
                  "linux" -> "linux"
                  "mingw32" -> "windows"
                  _ -> os -- TODO: Warning?
          in arch <> "-" <> osPath

       projectFile =
         if opts.libOnly
            then "cabal.validate-libonly.project"
            else "cabal.validate.project"

   when opts.listSteps $ do
      -- TODO: This should probably list _all_ available steps, not just the selected ones!
      putStrLn "Targets:"
      forM_ targets $ \target -> do
         putStrLn $ "  " <> target
      putStrLn "Steps:"
      forM_ steps $ \step -> do
         putStrLn $ "  " <> displayStep step
      exitSuccess

   startTime <- getAbsoluteTime
   jobs <- fromMaybe getNumCapabilities (pure <$> opts.jobs)
   cwd <- getCurrentDirectory
   compiler <- makeCompiler opts.compiler

   pure
      ResolvedOpts
         { verbose = opts.verbose
         , jobs = jobs
         , cwd = cwd
         , startTime = startTime
         , compiler = compiler
         , extraCompilers = opts.extraCompilers
         , cabal = opts.cabal
         , archPath = archPath
         , projectFile = projectFile
         , hackageTests = opts.hackageTests
         , targets = targets
         , steps = steps
         }

-- | Command-line options.
data Opts = Opts
   { verbose :: Bool
   , jobs :: Maybe Int
   , compiler :: FilePath
   , cabal :: FilePath
   , extraCompilers :: [FilePath]
   , doctest :: Bool
   , steps :: [Step]
   , listSteps :: Bool
   , libOnly :: Bool
   , runLibTests :: Bool
   , runCliTests :: Bool
   , runLibSuite :: Bool
   , runCliSuite :: Bool
   , solverBenchmarks :: Bool
   , hackageTests :: HackageTests
   }
   deriving (Show)

optsParser :: Parser Opts
optsParser =
   Opts
      <$> ( flag'
               True
               ( short 'v'
                  <> long "verbose"
                  <> help "Always display build and test output"
               )
               <|> flag
                  False
                  False
                  ( short 'q'
                     <> long "quiet"
                     <> help "Silence build and test output"
                  )
          )
      <*> option
         (Just <$> auto)
         ( short 'j'
            <> long "jobs"
            <> help "Passed to `cabal build --jobs`"
            <> value Nothing
         )
      <*> strOption
         ( short 'w'
            <> long "with-compiler"
            -- TODO: For tests? Builds?
            <> help "Use the given compiler instead of `ghc`"
            <> value "ghc"
         )
      <*> strOption
         ( long "with-cabal"
            -- TODO: For builds?
            <> help "Use the given `cabal-install`"
            <> value "cabal"
         )
      <*> many
         ( strOption
            ( long "extra-hc"
               <> help "Extra compilers to run the test suites with"
            )
         )
      <*> boolOption
         False
         "doctest"
         ( -- TODO: Which libraries?
           help "Run doctest on library"
         )
      <*> many
         ( (option (maybeReader parseStep))
            ( long "step"
               <> help "Run only a specific step (can be specified multiple times)"
            )
         )
      <*> switch
         ( long "list-steps"
            <> help "List the available steps and exit"
         )
      <*> ( flag'
               True
               ( long "lib-only"
                  <> help "Test only `Cabal` (the library)"
               )
               <|> flag
                  False
                  False
                  ( long "cli"
                     <> help "Test `cabal-install` (the executable) in addition to `Cabal` (the library)"
                  )
          )
      <*> boolOption
         True
         "run-lib-tests"
         ( -- TODO: Which library?
           help "Run library tests"
         )
      <*> boolOption
         True
         "run-cli-tests"
         ( help "Run client tests"
         )
      <*> boolOption
         False
         "run-lib-suite"
         ( -- TODO: Which library? What does this mean?
           help "Run `cabal-testsuite` with library"
         )
      <*> boolOption
         False
         "run-cli-suite"
         ( -- TODO: What does this mean?
           help "Run `cabal-testsuite` with client"
         )
      <*> boolOption
         False
         "run-solver-benchmarks"
         ( help "Build and trial run `solver-benchmarks`"
         )
      <*> ( flag'
               CompleteHackageTests
               ( long "complete-hackage-tests"
                  <> help "Run `hackage-tests` on complete Hackage data"
               )
               <|> flag
                  NoHackageTests
                  PartialHackageTests
                  ( long "partial-hackage-tests"
                     <> help "Run `hackage-tests` on parts of Hackage data"
                  )
          )

-- | Parse a boolean switch with separate names for the true and false options.
boolOption' :: Bool -> String -> String -> Mod FlagFields Bool -> Parser Bool
boolOption' defaultValue trueName falseName modifiers =
   flag' True (modifiers <> long trueName)
      <|> flag defaultValue False (modifiers <> hidden <> long falseName)

-- | Parse a boolean switch with a `--no-*` flag for setting the option to false.
boolOption :: Bool -> String -> Mod FlagFields Bool -> Parser Bool
boolOption defaultValue trueName modifiers =
   boolOption' defaultValue trueName ("no-" <> trueName) modifiers

fullOptsParser :: ParserInfo Opts
fullOptsParser =
   info
      (optsParser <**> helper)
      ( fullDesc
         <> progDesc "Test suite runner for `Cabal` and `cabal-install` developers"
      )

data HackageTests
   = CompleteHackageTests
   | PartialHackageTests
   | NoHackageTests
   deriving (Show)

data Step
   = PrintConfig
   | PrintToolVersions
   | Build
   | Doctest
   | LibTests
   | LibSuite
   | LibSuiteExtras
   | CliTests
   | CliSuite
   | SolverBenchmarksTests
   | SolverBenchmarksRun
   | TimeSummary
   deriving (Eq, Enum, Bounded, Show)

displayStep :: Step -> String
displayStep step =
   case step of
      PrintConfig -> "print-config"
      PrintToolVersions -> "print-tool-versions"
      Build -> "build"
      Doctest -> "doctest"
      LibTests -> "lib-tests"
      LibSuite -> "lib-suite"
      LibSuiteExtras -> "lib-suite-extras"
      CliTests -> "cli-tests"
      CliSuite -> "cli-suite"
      SolverBenchmarksTests -> "solver-benchmarks-tests"
      SolverBenchmarksRun -> "solver-benchmarks-run"
      TimeSummary -> "time-summary"

nameToStep :: Map String Step
nameToStep =
   Map.fromList
      [ (displayStep step, step)
      | step <- [minBound .. maxBound]
      ]

parseStep :: String -> Maybe Step
parseStep step = Map.lookup step nameToStep

runStep :: ResolvedOpts -> Step -> IO ()
runStep opts step = do
   printHeader (displayStep step)
   case step of
      PrintConfig -> printConfig opts
      PrintToolVersions -> printToolVersions opts
      _ -> error "TODO"
   putStrLn ""

getTerminalWidth :: IO Int
getTerminalWidth = maybe 80 (.width) <$> size @Int

printHeader :: String -> IO ()
printHeader title = do
   columns <- getTerminalWidth
   let left = 3
       right = columns - length title - left - 2
       header =
         setSGRCode [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Cyan]
            <> replicate left '═'
            <> " "
            <> title
            <> " "
            <> replicate right '═'
            <> setSGRCode [Reset]
   putStrLn header

timed :: ResolvedOpts -> Text -> [Text] -> IO ()
timed opts command args = do
   let prettyCommand = command <> " " <> T.unwords args

   startTime <- getAbsoluteTime

   -- TODO: Replace `$HOME` or `opts.cwd` for brevity?
   putStrLn $
      setSGRCode [SetColor Foreground Vivid Blue]
         <> T.unpack ("$ " <> prettyCommand)
         <> setSGRCode [Reset]

   (exitCode, rawOutput) <-
      if opts.verbose
         then (\exitCode -> (exitCode, "")) <$> proc command args ""
         else procStrict command args ""

   endTime <- getAbsoluteTime

   let duration = diffAbsoluteTime endTime startTime
       totalDuration = diffAbsoluteTime endTime opts.startTime

       output = T.strip rawOutput
       linesLimit = 50
       outputLines = T.lines output
       hiddenLines = length outputLines - linesLimit
       tailLines = drop hiddenLines outputLines

       finishedMessage =
         T.unpack prettyCommand
            <> " ("
            <> formatDiffTime duration
            <> " this step, "
            <> formatDiffTime totalDuration
            <> " cumulative)"

   case exitCode of
      ExitSuccess -> do
         unless opts.verbose $ do
            if hiddenLines <= 0
               then putStrLn (T.unpack output)
               else
                  putStrLn $
                     "("
                        <> show hiddenLines
                        <> " lines hidden, use `--verbose` to show)\n"
                        <> "...\n"
                        <> T.unpack (T.unlines tailLines)

         putStrLn $
            setSGRCode [SetColor Foreground Vivid Green]
               <> "Finished: "
               <> finishedMessage
               <> setSGRCode [Reset]
      ExitFailure exitCode' -> do
         unless opts.verbose $ do
            putStrLn (T.unpack output)

         putStrLn $
            setSGRCode [SetColor Foreground Vivid Red]
               <> "Failed ("
               <> show exitCode'
               <> "): "
               <> finishedMessage
               <> setSGRCode [Reset]

         -- TODO: `--keep-going` mode.
         exitFailure

   pure ()

printConfig :: ResolvedOpts -> IO ()
printConfig opts = do
   putStrLn $
      "compiler:          "
         <> opts.compiler.executable
         <> "\ncabal-install:     "
         <> opts.cabal
         <> "\njobs:              "
         <> show opts.jobs
         <> "\nsteps:             "
         <> unwords (map displayStep opts.steps)
         <> "\nHackage tests:     "
         <> show opts.hackageTests
         <> "\nverbose:           "
         <> show opts.verbose
         <> "\nextra compilers:   "
         <> unwords opts.extraCompilers
         <> "\nextra RTS options: "
         <> unwords (map T.unpack (rtsArgs opts))

printToolVersions :: ResolvedOpts -> IO ()
printToolVersions opts = do
   timed opts (T.pack (baseHc opts)) ["--version"]
   timed opts (T.pack opts.cabal) ["--version"]

   forM_ opts.extraCompilers $ \compiler -> do
      timed opts (T.pack compiler) ["--version"]

getAbsoluteTime :: IO AbsoluteTime
getAbsoluteTime = systemToTAITime <$> getSystemTime

formatDiffTime :: DiffTime -> String
formatDiffTime delta =
   let minute = secondsToDiffTime 60
       hour = 60 * minute
    in if delta >= hour
         then formatTime defaultTimeLocale "%h:%02M:%02ES" delta
         else
            if delta >= minute
               then formatTime defaultTimeLocale "%m:%2ES" delta
               else formatTime defaultTimeLocale "%2Es" delta

main :: IO ()
main = do
   opts <- execParser fullOptsParser
   resolvedOpts <- resolveOpts opts
   mainInner resolvedOpts

mainInner :: ResolvedOpts -> IO ()
mainInner opts =
   forM_ opts.steps $ \step -> do
      runStep opts step
