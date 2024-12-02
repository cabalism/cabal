{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      :  Distribution.PackageDescription.Check.Common
-- Copyright   :  Francesco Ariis 2022
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Common types/functions to various check modules which are *no* part of
-- Distribution.PackageDescription.Check.Monad.
module Distribution.PackageDescription.Check.Common
  ( AssocDep
  , CabalField
  , PathKind (..)
  , checkCustomField
  , partitionDeps
  , checkPVP
  , checkPVPs
  , withoutUpperBound
  , leqUpperBound
  , trailingZeroUpperBound
  , gtLowerBound
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Compat.NonEmptySet (toNonEmpty)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Check.Monad
import Distribution.Utils.Generic (isAscii)
import Distribution.Version

import Control.Monad

-- Type of FilePath.
data PathKind
  = PathKindFile
  | PathKindDirectory
  | PathKindGlob
  deriving (Eq)

-- | .cabal field we are referring to. As now it is just a synonym to help
-- reading the code, in the future it might take advantage of typification
-- in Cabal-syntax.
type CabalField = String

checkCustomField :: Monad m => (String, String) -> CheckM m ()
checkCustomField (n, _) =
  checkP
    (any (not . isAscii) n)
    (PackageDistInexcusable $ NonASCIICustomField [n])

-- ------------------------------------------------------------
-- PVP types/functions
-- ------------------------------------------------------------

-- A library name / dependencies association list. Ultimately to be
-- fed to PVP check.
type AssocDep = (UnqualComponentName, [Dependency])

-- Convenience function to partition important dependencies by name. To
-- be used together with checkPVP. Important: usually “base” or “Cabal”,
-- as the error is slightly different.
-- Note that `partitionDeps` will also filter out dependencies which are
-- already present in a inherithed fashion (e.g. an exe which imports the
-- main library will not need to specify upper bounds on shared dependencies,
-- hence we do not return those).
--
partitionDeps
  :: Monad m
  => [AssocDep] -- Possibly inherited dependencies, i.e.
  -- dependencies from internal/main libs.
  -> [UnqualComponentName] -- List of package names ("base", "Cabal"…)
  -> [Dependency] -- Dependencies to check.
  -> CheckM m ([Dependency], [Dependency])
partitionDeps ads ns ds = do
  -- Shared dependencies from “intra .cabal” libraries.
  let
    -- names of our dependencies
    dqs = map unqualName ds
    -- shared targets that match
    fads = filter (flip elem dqs . fst) ads
    -- the names of such targets
    inNam = nub $ map fst fads :: [UnqualComponentName]
    -- the dependencies of such targets
    inDep = concatMap snd fads :: [Dependency]

  -- We exclude from checks:
  -- 1. dependencies which are shared with main library / a
  --    sublibrary; and of course
  -- 2. the names of main library / sub libraries themselves.
  --
  -- So in myPackage.cabal
  -- library
  --      build-depends: text < 5
  -- ⁝
  --      build-depends: myPackage,        ← no warning, internal
  --                     text,             ← no warning, inherited
  --                     monadacme         ← warning!
  let fFun d =
        notElem (unqualName d) inNam
          && notElem
            (unqualName d)
            (map unqualName inDep)
      ds' = filter fFun ds

  return $ partition (flip elem ns . unqualName) ds'
  where
    -- Return *sublibrary* name if exists (internal),
    -- otherwise package name.
    unqualName :: Dependency -> UnqualComponentName
    unqualName (Dependency n _ nel) =
      case head (toNonEmpty nel) of
        (LSubLibName ln) -> ln
        _ -> packageNameToUnqualComponentName n

-- PVP dependency check (one warning message per dependency, usually
-- for important dependencies like base).
checkPVP
  :: Monad m
  => (Dependency -> Bool)
  -> (String -> PackageCheck) -- Warn message dependend on name
  -- (e.g. "base", "Cabal").
  -> [Dependency]
  -> CheckM m ()
checkPVP p ckf ds = do
  let ods = filter p ds
  mapM_ (tellP . ckf . unPackageName . depPkgName) ods

-- PVP dependency check for a list of dependencies. Some code duplication
-- is sadly needed to provide more ergonimic error messages.
checkPVPs
  :: Monad m
  => (Dependency -> Bool)
  -> ( [String]
       -> PackageCheck -- Grouped error message, depends on a
       -- set of names.
     )
  -> [Dependency] -- Deps to analyse.
  -> CheckM m ()
checkPVPs p cf ds
  | null ns = return ()
  | otherwise = tellP (cf ns)
  where
    ods = filter p ds
    ns = map (unPackageName . depPkgName) ods

-- | Is the version range without an upper bound?
withoutUpperBound :: Dependency -> Bool
withoutUpperBound (Dependency _ ver _) = not . hasUpperBound $ ver

-- | Is the upper bound version range LEQ (less or equal, <=)?
leqUpperBound :: Dependency -> Bool
leqUpperBound (Dependency _ ver _) = isLEQUpperBound ver

-- | Does the upper bound version range have a trailing zero?
trailingZeroUpperBound :: Dependency -> Bool
trailingZeroUpperBound (Dependency _ ver _) = isTrailingZeroUpperBound ver

-- | Is the lower bound version range GT (greater than, >)?
gtLowerBound :: Dependency -> Bool
gtLowerBound (Dependency _ ver _) = isGTLowerBound ver

pattern IsLEQUpperBound, IsGTLowerBound, IsTrailingZeroUpperBound :: VersionRangeF a
pattern IsLEQUpperBound <- OrEarlierVersionF _
pattern IsGTLowerBound <- LaterVersionF _
pattern IsTrailingZeroUpperBound <- (upperTrailingZero -> True)

upperTrailingZero :: VersionRangeF a -> Bool
upperTrailingZero (OrEarlierVersionF x) = trailingZero x
upperTrailingZero (EarlierVersionF x) = trailingZero x
upperTrailingZero _ = False

trailingZero :: Version -> Bool
trailingZero (versionNumbers -> vs)
  | [0] <- vs = False
  | 0 : _ <- reverse vs = True
  | otherwise = False

isLEQUpperBound :: VersionRange -> Bool
isLEQUpperBound (projectVersionRange -> v)
  | IsLEQUpperBound <- v = True
  | IntersectVersionRangesF x y <- v = isLEQUpperBound x || isLEQUpperBound y
  | UnionVersionRangesF x y <- v = isLEQUpperBound x || isLEQUpperBound y
  | otherwise = False

isGTLowerBound :: VersionRange -> Bool
isGTLowerBound (projectVersionRange -> v)
  | IsGTLowerBound <- v = True
  | IntersectVersionRangesF x y <- v = isGTLowerBound x || isGTLowerBound y
  | UnionVersionRangesF x y <- v = isGTLowerBound x || isGTLowerBound y
  | otherwise = False

isTrailingZeroUpperBound :: VersionRange -> Bool
isTrailingZeroUpperBound (projectVersionRange -> v)
  | IsTrailingZeroUpperBound <- v = True
  | IntersectVersionRangesF x y <- v = isTrailingZeroUpperBound x || isTrailingZeroUpperBound y
  | UnionVersionRangesF x y <- v = isTrailingZeroUpperBound x || isTrailingZeroUpperBound y
  | otherwise = False
