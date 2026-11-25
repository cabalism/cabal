-- |
-- Module      :  Distribution.Solver.Types.PkgConfigDb
-- Copyright   :  (c) Iñaki García Etxebarria 2016
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- The packages available to pkg-config.
module Distribution.Solver.Types.PkgConfigDb
    ( PkgConfigDb (..)
    , pkgConfigDbFromList
    , pkgConfigPkgIsPresent
    , pkgConfigDbPkgVersion
    ) where

import Distribution.Solver.Compat.Prelude
import Prelude ()

import qualified Data.Map                 as M

import Distribution.Package                     (PkgconfigName, mkPkgconfigName)
import Distribution.Parsec
import Distribution.Types.PkgconfigVersion
import Distribution.Types.PkgconfigVersionRange

-- | The list of packages installed in the system visible to
-- @pkg-config@.
--
-- If an entry is `Nothing`, this means that the package seems to be present,
-- but we don't know the exact version (because parsing of the version number
-- failed).
newtype PkgConfigDb = PkgConfigDb (M.Map PkgconfigName (Maybe PkgconfigVersion))
     deriving (Show, Generic)

instance Binary PkgConfigDb
instance Structured PkgConfigDb

-- | Create a `PkgConfigDb` from a list of @(packageName, version)@ pairs.
pkgConfigDbFromList :: [(String, String)] -> PkgConfigDb
pkgConfigDbFromList pairs = (PkgConfigDb . M.fromList . map convert) pairs
    where
      convert :: (String, String) -> (PkgconfigName, Maybe PkgconfigVersion)
      convert (n,vs) = (mkPkgconfigName n, simpleParsec vs)

-- | Check whether a given package range is satisfiable in the given
-- @pkg-config@ database.
pkgConfigPkgIsPresent :: PkgConfigDb -> PkgconfigName -> PkgconfigVersionRange -> Bool
pkgConfigPkgIsPresent (PkgConfigDb db) pn vr =
    case M.lookup pn db of
      Nothing       -> False    -- Package not present in the DB.
      Just Nothing  -> True     -- Package present, but version unknown.
      Just (Just v) -> withinPkgconfigVersionRange v vr

-- | Query the version of a package in the @pkg-config@ database.
-- @Nothing@ indicates the package is not in the database, while
-- @Just Nothing@ indicates that the package is in the database,
-- but its version is not known.
pkgConfigDbPkgVersion :: PkgConfigDb -> PkgconfigName -> Maybe (Maybe PkgconfigVersion)
pkgConfigDbPkgVersion (PkgConfigDb db) pn = M.lookup pn db
