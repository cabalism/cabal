{-# LANGUAGE OverloadedStrings #-}

module Distribution.Types.LibraryStanza
  ( LibraryStanza (..)
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Parsec
import Distribution.Pretty

import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp

-- | An optional stanza a library component can belong to.
--
-- A library may belong to several: helpers shared by a package's test-suites
-- and its benchmarks belong to both, and are requested when either is. A
-- library belonging to none -- the usual case, and the default -- is always
-- requested.
--
-- @since 3.19.0.0
data LibraryStanza
  = -- | Requested when test-suites are.
    LibraryStanzaTest
  | -- | Requested when benchmarks are.
    LibraryStanzaBench
  deriving (Generic, Show, Read, Eq, Ord, Enum, Bounded, Data)

instance Pretty LibraryStanza where
  pretty LibraryStanzaTest = Disp.text "test"
  pretty LibraryStanzaBench = Disp.text "bench"

instance Parsec LibraryStanza where
  parsec = do
    name <- P.munch1 isAlpha
    case name of
      "test" -> return LibraryStanzaTest
      "bench" -> return LibraryStanzaBench
      _ -> fail $ "Unknown stanza: " ++ name

instance Binary LibraryStanza
instance Structured LibraryStanza
instance NFData LibraryStanza
