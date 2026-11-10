-- | The code generator of the @cabal-tests@ test-suite.
--
-- Cabal runs a code generator as
--
-- > script-env-gen TARGETDIR [SOURCEDIR ...] -- [GHCOPTION ...]
--
-- where the GHC options are those it builds the test-suite with. We want the
-- options that say which packages the test-suite depends on and which package
-- databases they are in. @cabal-tests@ runs the test scripts with those, see
-- "Test.Cabal.Script", so we generate a module for it that has them.
module Main (main) where

import Data.List (stripPrefix)
import System.Directory (canonicalizePath, createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath (takeFileName, (</>))

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> die "script-env-gen: expected a target directory"
    targetDir : rest -> do
      let ghcOptions = drop 1 $ dropWhile (/= "--") rest
      -- The paths are relative to the package directory, which is the
      -- working directory of the generator. The tests run elsewhere.
      -- Leave out the package database for the test-suite's own package.
      packageDbs <-
        traverse canonicalizePath . filter ((/= "package.conf.inplace") . takeFileName) $
          optionValues "-package-db" ghcOptions
      let packageIds = optionValues "-package-id" ghcOptions
      let moduleDir = targetDir </> "Test" </> "Cabal"
      createDirectoryIfMissing True moduleDir
      writeFile (moduleDir </> "ScriptEnv0.hs") . unlines $
        [ "module Test.Cabal.ScriptEnv0 where"
        , ""
        , "-- | The package databases that cabal-tests was built with."
        , "scriptPackageDbs :: [FilePath]"
        , "scriptPackageDbs = " ++ show packageDbs
        , ""
        , "-- | The ids of the packages that cabal-tests depends on."
        , "scriptPackageIds :: [String]"
        , "scriptPackageIds = " ++ show packageIds
        ]
      -- Cabal adds the modules we name to the test-suite.
      putStrLn "Test.Cabal.ScriptEnv0"

-- | The values of an option, given as @-option value@ or @-option=value@.
optionValues :: String -> [String] -> [String]
optionValues option = go
  where
    go [] = []
    go (o : v : rest) | o == option = v : go rest
    go (o : rest) = maybe id (:) (stripPrefix (option ++ "=") o) (go rest)
