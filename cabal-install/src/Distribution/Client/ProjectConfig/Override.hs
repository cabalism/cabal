{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

-- | Resolving @override-constraints@ against plain @constraints@ before the
-- solver runs.
--
-- Plain constraints are additive: the solver intersects them. An override
-- replaces other constraints of the same kind on the same package, at a weaker
-- position, and plain constraints at the same position. A position is a
-- configuration layer and an import depth within it. Two different overrides
-- at the same position are an error, as is an override whose scope is narrower
-- than a constraint it would otherwise replace.
module Distribution.Client.ProjectConfig.Override
  ( -- * Positions
    Layer (..)
  , Position (..)
  , constraintPosition

    -- * Resolution
  , OverrideNote (..)
  , OverrideError (..)
  , applyOverrideConstraints

    -- * Reporting
  , reportOverrideNotes
  , overrideErrorMsg
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Distribution.Client.Targets
  ( UserConstraint (..)
  , UserConstraintScope (..)
  , UserQualifier (..)
  , userConstraintPackageName
  )
import Distribution.Simple.Utils (info, notice, warn)
import Distribution.Solver.Types.ConstraintSource (ConstraintSource (..))
import Distribution.Solver.Types.PackageConstraint (PackageProperty (..))
import Distribution.Solver.Types.ProjectConfigPath
  ( ProjectConfigPath (..)
  , docProjectConfigPath
  , projectConfigPathRoot
  )
import Distribution.Types.Flag (FlagName, mkFlagAssignment, unFlagAssignment)
import Distribution.Types.PackageName (PackageName)
import Distribution.Version (simplifyVersionRange)
import System.FilePath (takeExtension)
import Text.PrettyPrint (Doc, nest, render, text, vcat, ($$), ($+$))

-- | Configuration layers, strongest first.
data Layer
  = -- | The command line, including user targets.
    LayerCommandLine
  | -- | @cabal.project.local@ and its imports.
    LayerLocal
  | -- | @cabal.project@, @cabal.project.freeze@ and their imports.
    LayerProject
  | -- | The global config file.
    LayerGlobal
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Where a constraint sits. The derived 'Ord' compares the layer first and
-- then the depth, so a smaller position is a stronger one.
data Position = Position
  { positionLayer :: Layer
  , positionDepth :: Int
  -- ^ The number of imports between the layer's root file and the file with
  -- the constraint.
  }
  deriving (Eq, Ord, Show)

-- | The position of a constraint, or 'Nothing' for constraints outside the
-- position system: those cabal adds itself, and those with no known source.
-- These are never replaced and never replace anything.
constraintPosition :: ConstraintSource -> Maybe Position
constraintPosition = \case
  ConstraintSourceCommandlineFlag -> Just (Position LayerCommandLine 0)
  ConstraintSourceUserTarget -> Just (Position LayerCommandLine 0)
  ConstraintSourceProjectConfig path@(ProjectConfigPath p) ->
    let layer = if takeExtension (projectConfigPathRoot path) == ".local" then LayerLocal else LayerProject
     in Just (Position layer (NE.length p - 1))
  ConstraintSourceMainConfig _ -> Just (Position LayerGlobal 0)
  ConstraintSourceUserConfig _ -> Just (Position LayerGlobal 0)
  _ -> Nothing

type Labeled = (UserConstraint, ConstraintSource)

-- | What an override replaces. Version ranges, @installed@ and @source@ are one
-- kind; each flag is its own kind. Stanza constraints only ever enable a stanza
-- and have no kind.
data Kind = KindVersion | KindFlag FlagName
  deriving (Eq, Ord, Show)

-- | A constraint of one kind. A multi-flag constraint is one atom per flag.
data Atom = Atom
  { atomId :: Int
  , atomOrigin :: Int
  -- ^ The index of the constraint this atom came from.
  , atomIsOverride :: Bool
  , atomKind :: Kind
  , atomConstraint :: UserConstraint
  , atomSource :: ConstraintSource
  , atomPosition :: Maybe Position
  }

-- | Splits a constraint into atoms, or 'Nothing' if it has no kind.
atomize :: UserConstraint -> Maybe [(Kind, UserConstraint)]
atomize c@(UserConstraint scope prop) = case prop of
  PackagePropertyStanzas _ -> Nothing
  PackagePropertyFlags flags ->
    Just
      [ (KindFlag name, UserConstraint scope (PackagePropertyFlags (mkFlagAssignment [(name, value)])))
      | (name, value) <- unFlagAssignment flags
      ]
  _ -> Just [(KindVersion, c)]

-- | Whether every package instance the second scope applies to, the first
-- applies to as well.
scopeContains :: UserConstraintScope -> UserConstraintScope -> Bool
scopeContains outer inner = case outer of
  UserAnyQualifier p -> p == userConstraintScopeName inner
  UserAnySetupQualifier p -> case inner of
    UserAnySetupQualifier q -> p == q
    UserQualified (UserQualSetup _) q -> p == q
    _ -> False
  _ -> outer == inner

scopesOverlap :: UserConstraintScope -> UserConstraintScope -> Bool
scopesOverlap a b = scopeContains a b || scopeContains b a

userConstraintScopeName :: UserConstraintScope -> PackageName
userConstraintScopeName = \case
  UserQualified _ p -> p
  UserAnySetupQualifier p -> p
  UserAnyQualifier p -> p

-- | Two constraints are the same when they are equal after simplifying version
-- ranges, regardless of where they came from.
sameConstraint :: UserConstraint -> UserConstraint -> Bool
sameConstraint (UserConstraint sa pa) (UserConstraint sb pb) = sa == sb && simplify pa == simplify pb
  where
    simplify (PackagePropertyVersion vr) = PackagePropertyVersion (simplifyVersionRange vr)
    simplify p = p

-- | What happened to an override.
data OverrideNote
  = -- | The override replaced the constraint.
    OverrideReplaced Labeled Labeled
  | -- | The override replaced nothing.
    OverrideUnused Labeled
  deriving (Eq, Show)

-- | Why overrides could not be resolved.
data OverrideError
  = -- | Two different overrides of the same kind at the same position.
    OverrideConflict Labeled Labeled
  | -- | An override whose scope is narrower than a constraint it would
    -- otherwise replace.
    OverrideTooNarrow Labeled Labeled
  deriving (Eq, Show)

-- | Resolves overrides against plain constraints. Returns the constraints to
-- give the solver, and notes on what the overrides did.
applyOverrideConstraints :: [Labeled] -> [Labeled] -> Either OverrideError ([Labeled], [OverrideNote])
applyOverrideConstraints plain overrides = do
  (removed, replacements) <- foldM step (duplicates, []) positions
  let survivors = [a | a <- atoms, atomId a `Set.notMember` removed]
      byOrigin = Map.fromListWith (flip (++)) [(atomOrigin a, [a]) | a <- survivors]
      replacers = Set.fromList [atomId o | (o, _) <- replacements]
      -- An override that replaced nothing, unless it was itself replaced by a
      -- stronger one or merged into an identical one.
      unused =
        [ OverrideUnused (labeled a)
        | a <- atoms
        , atomIsOverride a
        , atomId a `Set.notMember` removed
        , atomId a `Set.notMember` replacers
        ]
      notes = [OverrideReplaced (labeled o) (labeled c) | (o, c) <- replacements] ++ unused
  pure (concat [rebuild i lc byOrigin | (i, (lc, _)) <- originals], notes)
  where
    originals :: [(Int, (Labeled, Bool))]
    originals = zip [0 ..] (map (,False) plain ++ map (,True) overrides)

    atoms :: [Atom]
    atoms =
      zipWith (\i (kind, c, src, isOverride, origin) -> Atom i origin isOverride kind c src (constraintPosition src)) [0 ..] $
        [ (kind, c, src, isOverride, origin)
        | (origin, ((constraint, src), isOverride)) <- originals
        , Just kinds <- [atomize constraint]
        , (kind, c) <- kinds
        ]

    labeled :: Atom -> Labeled
    labeled a = (atomConstraint a, atomSource a)

    atomCounts :: Map.Map Int Int
    atomCounts = Map.fromListWith (+) [(atomOrigin a, 1) | a <- atoms]

    groups :: [[Atom]]
    groups = Map.elems $ Map.fromListWith (flip (++)) [((userConstraintPackageName (atomConstraint a), atomKind a), [a]) | a <- atoms]

    -- Identical overrides at the same position merge: all but the first are
    -- dropped up front, without being reported.
    duplicates :: Set.Set Int
    duplicates =
      Set.fromList
        [ atomId b
        | g <- groups
        , (a, b) <- pairs [o | o <- g, atomIsOverride o]
        , atomPosition a == atomPosition b
        , sameConstraint (atomConstraint a) (atomConstraint b)
        ]

    positions :: [Position]
    positions = sort . nub $ [p | a <- atoms, atomIsOverride a, Just p <- [atomPosition a]]

    step :: (Set.Set Int, [(Atom, Atom)]) -> Position -> Either OverrideError (Set.Set Int, [(Atom, Atom)])
    step (removed0, replacements0) p = foldM group (removed0, replacements0) groups
      where
        group (removed, replacements) g = do
          let live a = atomId a `Set.notMember` removed
              overs = [a | a <- g, atomIsOverride a, atomPosition a == Just p, live a]
          -- R3: different overrides at this position must not overlap.
          sequence_
            [ Left (OverrideConflict (labeled a) (labeled b))
            | (a, b) <- pairs overs
            , scopesOverlap (scope a) (scope b)
            , not (sameConstraint (atomConstraint a) (atomConstraint b))
            ]
          -- R2: each override replaces what it contains at weaker positions,
          -- and plain constraints at this position.
          foldM
            ( \(removed', replacements') o ->
                foldM
                  ( \(rm, rs) c ->
                      if scopeContains (scope o) (scope c)
                        then Right (Set.insert (atomId c) rm, (o, c) : rs)
                        else
                          if scopesOverlap (scope o) (scope c)
                            then Left (OverrideTooNarrow (labeled o) (labeled c))
                            else Right (rm, rs)
                  )
                  (removed', replacements')
                  [ c
                  | c <- g
                  , atomId c `Set.notMember` removed'
                  , atomId c /= atomId o
                  , Just pc <- [atomPosition c]
                  , pc > p || (pc == p && not (atomIsOverride c))
                  ]
            )
            (removed, replacements)
            overs

    scope :: Atom -> UserConstraintScope
    scope a = let UserConstraint s _ = atomConstraint a in s

    -- Puts a constraint back together from its surviving atoms.
    rebuild :: Int -> Labeled -> Map.Map Int [Atom] -> [Labeled]
    rebuild i lc@(UserConstraint s _, src) byOrigin = case atomize (fst lc) of
      Nothing -> [lc]
      Just _ -> case Map.findWithDefault [] i byOrigin of
        [] -> []
        as
          | length as == Map.findWithDefault 0 i atomCounts -> [lc]
          | otherwise ->
              [
                ( UserConstraint s . PackagePropertyFlags . mkFlagAssignment $
                    [ (name, value)
                    | a <- as
                    , UserConstraint _ (PackagePropertyFlags flags) <- [atomConstraint a]
                    , (name, value) <- unFlagAssignment flags
                    ]
                , src
                )
              ]

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = map (x,) xs ++ pairs xs

-- | Reports what the overrides did. Replacements are listed at @-v2@, except
-- that replacing a constraint from a freeze file is reported at normal
-- verbosity, since a freeze file is meant to be authoritative. An override that
-- replaces a constraint in its own file, or that replaces nothing, is a
-- warning.
reportOverrideNotes :: Verbosity -> [OverrideNote] -> IO ()
reportOverrideNotes verbosity notes = do
  let replaced = [(o, c) | OverrideReplaced o c <- notes]
      unused = [o | OverrideUnused o <- notes]
      sameFile = [(o, c) | (o, c) <- replaced, snd o == snd c]
      fromFreeze = [(o, c) | (o, c) <- replaced, snd o /= snd c, isFreeze (snd c)]
  unless (null replaced) . info verbosity . render $
    text "override-constraints replaced these constraints:" $+$ nest 2 (vcat (map docReplacement replaced))
  unless (null fromFreeze) . notice verbosity . render $
    text "override-constraints replaced these constraints from a freeze file:" $+$ nest 2 (vcat (map docReplacement fromFreeze))
  unless (null sameFile) . warn verbosity . render $
    text "override-constraints replaced these constraints from the same file, which is probably a mistake:"
      $+$ nest 2 (vcat (map docReplacement sameFile))
  unless (null unused) . warn verbosity . render $
    text "override-constraints replaced nothing for:" $+$ nest 2 (vcat (map docLabeled unused))
  where
    isFreeze = \case
      ConstraintSourceProjectConfig path -> takeExtension (projectConfigPathRoot path) == ".freeze"
      _ -> False

docReplacement :: (Labeled, Labeled) -> Doc
docReplacement (o, c) = docLabeled o $+$ nest 2 (text "replaced" <+> docLabeled c)

docLabeled :: Labeled -> Doc
docLabeled (c, src) = case src of
  ConstraintSourceProjectConfig path -> (pretty c <+> text "from") $$ nest 2 (docProjectConfigPath path)
  other -> pretty c <+> text "from" <+> pretty other

-- | The message for an 'OverrideError'.
overrideErrorMsg :: OverrideError -> String
overrideErrorMsg =
  render . \case
    OverrideConflict a b ->
      text "conflicting override-constraints at the same position:"
        $+$ nest 2 (docLabeled a $+$ docLabeled b)
    OverrideTooNarrow o c ->
      text "an override-constraint is narrower than a constraint it would replace:"
        $+$ nest 2 (docLabeled o $+$ nest 2 (text "is narrower than" <+> docLabeled c))
        $+$ text "Widen the override's scope, for example with the any. prefix, to replace every instance of the package."
