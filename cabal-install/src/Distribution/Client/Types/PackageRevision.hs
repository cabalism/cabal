{-# LANGUAGE OverloadedStrings #-}

-- | Pinning a specific @.cabal@ file revision of a package version.
--
-- Package repositories such as Hackage allow the @.cabal@ file of a
-- released package version to be edited after the fact. Every such edit
-- is a /revision/, numbered from @1@ (the original upload being revision
-- @0@) and recorded in the @x-revision@ field of the revised @.cabal@
-- file. A 'PackageRevision' pins one package version to one of its
-- revisions, either by number or by the SHA-256 hash of the @.cabal@
-- file text.
module Distribution.Client.Types.PackageRevision
  ( RevisionPin (..)
  , PackageRevision (..)
  , RevisionPins
  , packageRevisionsMap
  , packageDescriptionRevision
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Control.Monad ((<=<))
import Data.Char (isHexDigit)
import Distribution.Client.HashValue (HashValue, parseHashValue, showHashValue)
import Distribution.Package (PackageId, PackageIdentifier (..))
import Distribution.PackageDescription (PackageDescription (..))
import Distribution.Version (nullVersion)

import qualified Data.Map.Strict as Map
import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp

-- | Which revision of a package version to use.
data RevisionPin
  = -- | The revision whose @x-revision@ field is this number, with the
    -- original upload (no @x-revision@ field) being revision @0@.
    RevisionNumber Int
  | -- | The revision whose @.cabal@ file text has this SHA-256 hash.
    RevisionHash HashValue
  deriving (Eq, Show, Generic)

instance Binary RevisionPin
instance Structured RevisionPin
instance NFData RevisionPin

instance Pretty RevisionPin where
  pretty (RevisionNumber n) = Disp.text "rev:" <<>> Disp.int n
  pretty (RevisionHash h) = Disp.text "sha256:" <<>> Disp.text (showHashValue h)

instance Parsec RevisionPin where
  parsec = parseNumber <|> parseHash
    where
      parseNumber = do
        _ <- P.string "rev:"
        RevisionNumber <$> P.integral
      parseHash = do
        _ <- P.string "sha256:"
        hex <- P.munch1 isHexDigit
        case parseHashValue hex of
          Just h -> return (RevisionHash h)
          Nothing -> fail $ "invalid sha256 hash: " ++ hex

-- | A package version pinned to one of its revisions, written in the
-- same notation as Stack uses: @pkg-1.2.3\@rev:2@ or
-- @pkg-1.2.3\@sha256:HEX@.
data PackageRevision = PackageRevision PackageId RevisionPin
  deriving (Eq, Show, Generic)

instance Binary PackageRevision
instance Structured PackageRevision
instance NFData PackageRevision

instance Pretty PackageRevision where
  pretty (PackageRevision pkgid pin) = pretty pkgid <<>> Disp.char '@' <<>> pretty pin

-- |
--
-- >>> simpleParsec "foo-1.2.3@rev:2" :: Maybe PackageRevision
-- Just (PackageRevision (PackageIdentifier {pkgName = PackageName "foo", pkgVersion = mkVersion [1,2,3]}) (RevisionNumber 2))
--
-- >>> simpleParsec "foo-1.2.3" :: Maybe PackageRevision
-- Nothing
instance Parsec PackageRevision where
  parsec = do
    pkgid <- parsec
    when (pkgVersion pkgid == nullVersion) $
      fail "expected a package version to pin the revision of"
    _ <- P.char '@'
    PackageRevision pkgid <$> parsec

-- | Revision pins keyed by package version.
type RevisionPins = Map PackageId RevisionPin

-- | Collect revision pins into a map, rejecting conflicting pins for the
-- same package version. Repeating an identical pin is fine.
packageRevisionsMap :: [PackageRevision] -> Either (PackageId, RevisionPin, RevisionPin) RevisionPins
packageRevisionsMap = foldM insert Map.empty
  where
    insert m (PackageRevision pkgid pin) =
      case Map.lookup pkgid m of
        Just pin' | pin' /= pin -> Left (pkgid, pin', pin)
        _ -> Right (Map.insert pkgid pin m)

-- | The revision number recorded in a package description's @x-revision@
-- field, or @0@ when there is none.
--
-- The @x-revision@ field is a feature of package repositories (not of
-- Cabal itself): repositories such as Hackage add it to the @.cabal@ file
-- text of every revision they publish.
packageDescriptionRevision :: PackageDescription -> Int
packageDescriptionRevision =
  fromMaybe 0 . (readMaybe <=< lookup "x-revision") . customFieldsPD
