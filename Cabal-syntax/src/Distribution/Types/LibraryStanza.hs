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

-- | Which optional stanza, if any, a library component belongs to.
--
-- A library in an optional stanza is requested only when that stanza is
-- requested, exactly as a test-suite is requested only under
-- @--enable-tests@. This lets a package share modules between its
-- test-suites without those modules' dependencies being resolved for
-- builds that do not want the tests.
--
-- @since 3.19.0.0
data LibraryStanza
  = -- | An ordinary library, always requested. The default.
    LibraryStanzaAlways
  | -- | Requested only when test-suites are.
    LibraryStanzaTest
  | -- | Requested only when benchmarks are.
    LibraryStanzaBench
  deriving (Generic, Show, Read, Eq, Ord, Data)

instance Pretty LibraryStanza where
  pretty LibraryStanzaAlways = Disp.text "always"
  pretty LibraryStanzaTest = Disp.text "test"
  pretty LibraryStanzaBench = Disp.text "bench"

instance Parsec LibraryStanza where
  parsec = do
    name <- P.munch1 isAlpha
    case name of
      "always" -> return LibraryStanzaAlways
      "test" -> return LibraryStanzaTest
      "bench" -> return LibraryStanzaBench
      _ -> fail $ "Unknown stanza: " ++ name

instance Binary LibraryStanza
instance Structured LibraryStanza
instance NFData LibraryStanza

-- | 'LibraryStanzaAlways' is the identity; combining two different optional
-- stanzas is not meaningful, so the left one wins.
instance Semigroup LibraryStanza where
  LibraryStanzaAlways <> b = b
  a <> _ = a

instance Monoid LibraryStanza where
  mempty = LibraryStanzaAlways
