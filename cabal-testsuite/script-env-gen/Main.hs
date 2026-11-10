-- | The code generator of the @test-runtime-deps@ test-suite.
--
-- Cabal runs a code generator as
--
-- > script-env-gen TARGETDIR [SOURCEDIR ...] -- [GHCOPTION ...]
--
-- where the GHC options are those it builds the test-suite with. We generate
-- no modules. We only want the options that say which packages the test-suite
-- depends on and which package databases they are in, and save those to a file
-- in the target directory for @cabal-tests@ to read, see
-- "Test.Cabal.Script".
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
      createDirectoryIfMissing True targetDir
      writeFile (targetDir </> "script-env") $ show (packageDbs, packageIds) ++ "\n"

-- | The values of an option, given as @-option value@ or @-option=value@.
optionValues :: String -> [String] -> [String]
optionValues option = go
  where
    go [] = []
    go (o : v : rest) | o == option = v : go rest
    go (o : rest) = maybe id (:) (stripPrefix (option ++ "=") o) (go rest)
