{-# LANGUAGE DataKinds #-}

-- | Functions for interrogating the current working directory
module Test.Cabal.Workdir where

import Distribution.Simple.Configure
import Distribution.Simple.Setup
import Distribution.Utils.Path
  ( Dist
  , FileOrDir (..)
  , Pkg
  , SymbolicPath
  , getSymbolicPath
  , makeSymbolicPath
  )

import System.Directory
import System.Environment (getExecutablePath)
import System.FilePath

-- | Guess what the dist directory of the package of a running executable is,
-- by using the conventional layout of built executables
-- in relation to the top of a dist directory.  Will not work
-- if the executable has been installed somewhere else.
guessDistDir :: IO (SymbolicPath Pkg (Dir Dist))
guessDistDir = do
  exe_path <- canonicalizePath =<< getExecutablePath
  -- The executable is at build/exe/exe in the dist directory of its component.
  let dist0 = packageDistDir . takeDirectory . takeDirectory $ takeDirectory exe_path
  b <- doesFileExist (dist0 </> "setup-config")
  if b
    then do
      cwd <- getCurrentDirectory
      return . makeSymbolicPath $ makeRelative (normalise cwd) dist0
    else do
      d <- getSymbolicPath <$> findDistPrefOrDefault NoFlag
      makeSymbolicPath <$> canonicalizePath d

-- Note [Dist directories]
-- ~~~~~~~~~~~~~~~~~~~~~~~
-- With @PKG@ for the directory that cabal-install gives a package under its
-- build directory, the dist directory of a component is:
--
-- > PKG/KIND/NAME[/VARIANT]
-- > PKG[/VARIANT]
--
-- The first is for a component other than the main library, with a @KIND@ of
-- @x@ for an executable or @t@ for a test-suite. The second is for the main
-- library, or for every component of a package that is configured as a whole.
-- @VARIANT@ is @noopt@ or @opt@ for builds with @-O0@ or @-O2@.
--
-- We call the second of these the dist directory of the package.

-- | The dist directory of the package, given the dist directory of one of its
-- executables, see Note [Dist directories].
packageDistDir :: FilePath -> FilePath
packageDistDir dist =
  case reverse (splitDirectories dist') of
    _name : "x" : pkg -> joinPath (reverse pkg ++ variant)
    _ -> dist
  where
    (dist', variant) = splitVariant dist

-- | The dist directory of a component of the given kind and name, given the
-- dist directory of the package, see Note [Dist directories].
componentDistDir :: FilePath -> String -> String -> IO FilePath
componentDistDir dist kind name = do
  let (pkg, variant) = splitVariant dist
      dist' = joinPath ([pkg, kind, name] ++ variant)
  b <- doesDirectoryExist dist'
  return $ if b then dist' else dist

-- | Split the variant, if any, from the end of a dist directory.
splitVariant :: FilePath -> (FilePath, [FilePath])
splitVariant dist0
  | variant `elem` ["noopt", "opt"] = (takeDirectory dist, [variant])
  | otherwise = (dist, [])
  where
    dist = dropTrailingPathSeparator dist0
    variant = takeFileName dist

-- | The path of an executable of this package, given the dist directory of
-- the package.
distExePath :: FilePath -> String -> IO FilePath
distExePath dist exe = do
  dist' <- componentDistDir dist "x" exe
  return $ dist' </> "build" </> exe </> exe
