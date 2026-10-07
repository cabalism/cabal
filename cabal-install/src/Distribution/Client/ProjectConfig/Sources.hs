{-# LANGUAGE LambdaCase #-}

-- | Choosing between several sources for one package.
--
-- A project may name the same package more than once: a
-- @source-repository-package@ in @cabal.project@ and another in
-- @cabal.project.local@, a @packages@ directory in the root and a repository in
-- an import, or two directories holding the same package. Cabal only learns
-- which package a source provides after fetching and reading it, so every
-- source is read first. Then, for each package name, the source listed at the
-- strongest position wins: the position of a project file is its layer and
-- import depth, as for @override-constraints@
-- ("Distribution.Client.ProjectConfig.Override"). Identical sources listed
-- twice merge silently. Two different sources at the same position are an
-- error. The solver never sees the sources that lost.
module Distribution.Client.ProjectConfig.Sources
  ( provenancePosition
  , SourceNote (..)
  , SourceError (..)
  , resolveDuplicateSourcePackages

    -- * Reporting
  , reportSourceNotes
  , sourceErrorMsg
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Data.Foldable (minimumBy)
import qualified Data.List.NonEmpty as NE

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Distribution.Client.ProjectConfig.Override (Layer (..), Position (..), pathPosition)
import Distribution.Client.ProjectConfig.Types (ProjectConfigProvenance (..))
import Distribution.Client.Types.PackageLocation (UnresolvedSourcePackage)
import Distribution.Client.Types.PackageSpecifier (PackageSpecifier (..))
import Distribution.Package (PackageName, packageName)
import Distribution.Simple.Utils (notice)
import Distribution.Solver.Types.ProjectConfigPath (docProjectConfigPath, isTopLevelConfigPath)
import Distribution.Solver.Types.SourcePackage (SourcePackage (..))
import Text.PrettyPrint (Doc, nest, render, text, vcat, ($$), ($+$))

-- | The position of a package entry, from the project file that lists it. The
-- implicit project, used when there is no @cabal.project@, counts as a root
-- project file.
provenancePosition :: ProjectConfigProvenance -> Position
provenancePosition = \case
  Implicit -> Position LayerProject 0
  Explicit path -> pathPosition path

-- | A source package with the project files that listed it.
type Tagged = (UnresolvedSourcePackage, [ProjectConfigProvenance])

-- | What happened where a package had several sources.
data SourceNote
  = -- | The first source was used and the others were dropped.
    SourceReplaced PackageName Tagged [Tagged]
  deriving (Eq, Show)

-- | Why the sources could not be resolved.
data SourceError
  = -- | Different sources for one package at the same position.
    SourceConflict PackageName [Tagged]
  deriving (Eq, Show)

-- | Keeps one source per package name. Packages named by @extra-packages@ are
-- not sources and pass through untouched, as does a source with no provenance.
-- The order of the result follows the input.
resolveDuplicateSourcePackages
  :: [(PackageSpecifier UnresolvedSourcePackage, [ProjectConfigProvenance])]
  -> Either SourceError ([PackageSpecifier UnresolvedSourcePackage], [SourceNote])
resolveDuplicateSourcePackages specifiers = do
  (dropped, notes) <- foldM resolveGroup (Set.empty, []) (Map.elems groups)
  pure ([spec | (i, (spec, _)) <- indexed, i `Set.notMember` dropped], reverse notes)
  where
    indexed :: [(Int, (PackageSpecifier UnresolvedSourcePackage, [ProjectConfigProvenance]))]
    indexed = zip [0 ..] specifiers

    -- Source packages by name, in input order.
    groups :: Map.Map PackageName [(Int, Tagged)]
    groups =
      Map.fromListWith
        (flip (++))
        [ (packageName pkg, [(i, (pkg, provenances))])
        | (i, (SpecificSourcePackage pkg, provenances)) <- indexed
        ]

    resolveGroup
      :: (Set.Set Int, [SourceNote])
      -> [(Int, Tagged)]
      -> Either SourceError (Set.Set Int, [SourceNote])
    resolveGroup acc [] = pure acc
    resolveGroup (dropped, notes) group@((_, (pkg0, _)) : _) =
      case [(minimum (map provenancePosition provenances), entry) | entry@(_, (_, provenances@(_ : _))) <- merged] of
        [] -> pure (droppedMerged, notes)
        positioned ->
          let best = minimum (map fst positioned)
              (winners, losers) = partition ((== best) . fst) positioned
           in case winners of
                [(_, (_, winner))] ->
                  pure
                    ( foldr (Set.insert . fst . snd) droppedMerged losers
                    , [SourceReplaced name winner (map (snd . snd) losers) | not (null losers)] ++ notes
                    )
                _ -> Left (SourceConflict name (map (snd . snd) winners))
      where
        name = packageName pkg0

        -- Identical sources listed more than once are one source with the
        -- provenances of all its listings. Only the first listing is kept.
        merged :: [(Int, Tagged)]
        merged = foldl' mergeInto [] group

        mergeInto acc entry@(_, (pkg, provenances)) =
          case break (\(_, (pkg', _)) -> srcpkgSource pkg' == srcpkgSource pkg) acc of
            (before, (j, (pkg', provenances')) : after) -> before ++ (j, (pkg', provenances' ++ provenances)) : after
            (_, []) -> acc ++ [entry]

        droppedMerged = foldr Set.insert dropped [i | (i, _) <- group, i `notElem` map fst merged]

-- | Reports, at normal verbosity, each package whose other sources were dropped.
reportSourceNotes :: Verbosity -> [SourceNote] -> IO ()
reportSourceNotes verbosity notes =
  unless (null notes) . notice verbosity . render . vcat $
    [ text "cabal project has multiple sources for"
      <+> (pretty name <> text ":")
      $+$ nest 2 (text "using" <+> docTagged winner)
      $+$ nest 2 (vcat [text "ignoring" <+> docTagged loser | loser <- losers])
    | SourceReplaced name winner losers <- notes
    ]

-- | The message for a 'SourceError'.
sourceErrorMsg :: SourceError -> String
sourceErrorMsg = \case
  SourceConflict name sources ->
    render $
      text "cabal project has different sources for"
        <+> pretty name
        <+> text "at the same position:"
        $+$ nest 2 (vcat (map docTagged sources))
        $+$ text "Remove all but one, or list the one to use in a project file that outranks these, such as cabal.project.local."

-- | A source and the strongest of the project files that listed it. A root
-- project file is named on the same line; an import chain follows on its own
-- lines.
docTagged :: Tagged -> Doc
docTagged (pkg, provenances) = case NE.nonEmpty provenances of
  Nothing -> pretty (srcpkgSource pkg)
  Just ps -> case minimumBy (comparing provenancePosition) ps of
    Implicit -> pretty (srcpkgSource pkg) <+> text "from the implicit project"
    Explicit path
      | isTopLevelConfigPath path -> pretty (srcpkgSource pkg) <+> text "from" <+> docProjectConfigPath path
      | otherwise -> (pretty (srcpkgSource pkg) <+> text "from") $$ nest 2 (docProjectConfigPath path)
