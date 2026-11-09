-- | This module implements a view of a 'VersionRange' as a finite
-- list of separated version intervals.
--
-- In conversion from and to 'VersionRange' it makes some effort to
-- preserve the caret operator @^>=x.y@.  This constraint a priori
-- specifies the same interval as @==x.y.*@, but indicates that newer
-- versions could be acceptable (@allow-newer: ^@).
module Distribution.Types.VersionInterval
  ( -- * Version intervals
    VersionIntervals
  , unVersionIntervals
  , mkVersionIntervals

    -- * Conversions
  , toVersionIntervals
  , fromVersionIntervals

    -- ** Normalisation
  , normaliseVersionRange2

    -- * Relaxation
  , relaxLastInterval
  , relaxHeadInterval

    -- * Version intervals view
  , asVersionIntervals
  , VersionInterval (..)
  , LowerBound (..)
  , UpperBound (..)
  , Bound (..)

    -- * Invariants
  , invariantVersionIntervals
  ) where

import Control.Applicative (liftA2)
import Control.Exception (assert)
import Distribution.Compat.Prelude hiding (Applicative (..))
import Prelude ()

import Distribution.Types.Version
import Distribution.Types.VersionRange.Internal

-- To test this module, and to run version range normalisation benchmarks:
--
-- cabal run Cabal-tests:unit-tests -- -p Distribution.Version
-- cabal run cabal-benchmarks -- -o bench.html normaliseVersionRange
--
-- The @{-\@ ... \@-}@ comments are LiquidHaskell specifications.  They are
-- checked by building the Cabal-liquid package, see @cabal.liquid.project@,
-- and are inert otherwise.  They prove, for the list of intervals built by
-- 'stage2and3', the two clauses of 'invariantVersionIntervals' that do not
-- depend on the versions being valid: each interval is non-empty and
-- consecutive intervals do not touch.

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

-- | A complementary representation of a 'VersionRange'. Instead of a boolean
-- version predicate it uses an increasing sequence of non-overlapping,
-- non-empty intervals.
--
-- The key point is that this representation gives a canonical representation
-- for the semantics of 'VersionRange's. This makes it easier to check things
-- like whether a version range is empty, covers all versions, or requires a
-- certain minimum or maximum version. It also makes it easy to check equality
-- or containment. It also makes it easier to identify \'simple\' version
-- predicates for translation into foreign packaging systems that do not
-- support complex version range expressions.
newtype VersionIntervals = VersionIntervals [VersionInterval]
  deriving (Eq, Show)

-- | Inspect the list of version intervals.
unVersionIntervals :: VersionIntervals -> [VersionInterval]
unVersionIntervals (VersionIntervals is) = is

-- | Directly construct a 'VersionIntervals' from a list of intervals.
mkVersionIntervals :: [VersionInterval] -> Maybe VersionIntervals
mkVersionIntervals intervals
  | invariantVersionIntervals (VersionIntervals intervals) = Just . VersionIntervals $ intervals
  | otherwise = Nothing

data VersionInterval = VersionInterval !LowerBound !UpperBound deriving (Eq, Show)
data LowerBound = LowerBound !Version !Bound deriving (Eq, Show)
data UpperBound = NoUpperBound | UpperBound !Version !Bound deriving (Eq, Show)
data Bound = ExclusiveBound | InclusiveBound deriving (Eq, Show)

{-@ data VersionInterval = VersionInterval { viLower :: LowerBound, viUpper :: UpperBound } @-}
{-@ data LowerBound = LowerBound { lbVersion :: Version, lbBound :: Bound } @-}
{-@ data UpperBound = NoUpperBound | UpperBound { ubVersion :: Version, ubBound :: Bound } @-}

{-@ measure isBounded :: UpperBound -> Bool
      isBounded NoUpperBound = False
      isBounded (UpperBound v b) = True
  @-}

-- The lower bound @L@ is at or before the upper bound @U@: the interval
-- @[L, U]@ is non-empty, see 'nonEmptyVI'.
{-@ predicate NonEmptyBounds L U =
      not (isBounded U)
        || lbVersion L < ubVersion U
        || (lbVersion L == ubVersion U
              && lbBound L == InclusiveBound
              && ubBound U == InclusiveBound)
  @-}
{-@ predicate NonEmptyVI I = NonEmptyBounds (viLower I) (viUpper I) @-}

-- The upper bound @U@ is strictly before the lower bound @L@, see
-- 'doesNotTouch'.
{-@ predicate DoesNotTouch U L =
      isBounded U
        && (ubVersion U < lbVersion L
              || (ubVersion U == lbVersion L
                    && ubBound U == ExclusiveBound
                    && lbBound L == ExclusiveBound))
  @-}
{-@ predicate Separated X Y = DoesNotTouch (viUpper X) (viLower Y) @-}

-- The lower bound @A@ is at or after the lower bound @B@.
{-@ predicate LowerGE A B =
      lbVersion B < lbVersion A
        || (lbVersion B == lbVersion A
              && (lbBound A == lbBound B || lbBound B == InclusiveBound))
  @-}

-- The upper bound @V@ is at or after the upper bound @U@.
{-@ predicate UpperGE V U =
      not (isBounded V)
        || (isBounded U
              && (ubVersion U < ubVersion V
                    || (ubVersion U == ubVersion V
                          && (ubBound U == ubBound V || ubBound V == InclusiveBound))))
  @-}

{-@ type NonEmptyInterval = {i:VersionInterval | NonEmptyVI i} @-}

-- Non-empty intervals where no interval touches any interval before it.
{-@ type SeparatedIntervals = [NonEmptyInterval]<{\x y -> Separated x y}> @-}

{-@ zeroLowerBound :: {l:LowerBound | lbVersion l == 0 && lbBound l == InclusiveBound} @-}
zeroLowerBound :: LowerBound
zeroLowerBound = LowerBound version0 InclusiveBound

{-@ isVersion0 :: v:Version -> {b:Bool | b <=> v == 0} @-}
isVersion0 :: Version -> Bool
isVersion0 = (==) version0

-------------------------------------------------------------------------------
-- Stage1
-------------------------------------------------------------------------------

-- Every interval built from a leaf is non-empty because the versions in a
-- 'VersionRange' are valid, an assumption recorded in Cabal-liquid.
{-@ stage1 :: VersionRange -> [NonEmptyInterval] @-}
stage1 :: VersionRange -> [VersionInterval]
stage1 = cataVersionRange alg
  where
    -- version range leaves transform into singleton intervals
    alg (ThisVersionF v) = [VersionInterval (LowerBound v InclusiveBound) (UpperBound v InclusiveBound)]
    alg (LaterVersionF v) = [VersionInterval (LowerBound v ExclusiveBound) NoUpperBound]
    alg (OrLaterVersionF v) = [VersionInterval (LowerBound v InclusiveBound) NoUpperBound]
    alg (EarlierVersionF v)
      | isVersion0 v = []
      | otherwise = [VersionInterval zeroLowerBound (UpperBound v ExclusiveBound)]
    alg (OrEarlierVersionF v) = [VersionInterval zeroLowerBound (UpperBound v InclusiveBound)]
    -- \^>= version-range's upper bound should be MajorBound
    alg (MajorBoundVersionF v) = [VersionInterval (LowerBound v InclusiveBound) (UpperBound (majorUpperBound v) ExclusiveBound)]
    -- union: just merge the version intervals
    alg (UnionVersionRangesF v1 v2) = v1 ++ v2
    -- intersection: pairwise intersect. Strip empty intervals. Sort to restore the invariant.
    alg (IntersectVersionRangesF v1 v2) = mapMaybe nonEmptyInterval $ liftA2 intersectInterval (stage2and3 v1) (stage2and3 v2)

-- Check that interval is non-empty
{-@ nonEmptyInterval :: VersionInterval -> Maybe NonEmptyInterval @-}
nonEmptyInterval :: VersionInterval -> Maybe VersionInterval
nonEmptyInterval i | nonEmptyVI i = Just i
nonEmptyInterval _ = Nothing

-------------------------------------------------------------------------------
-- Stage2
-------------------------------------------------------------------------------

{-@ stage2 :: [NonEmptyInterval] -> [NonEmptyInterval] @-}
stage2 :: [VersionInterval] -> [VersionInterval]
stage2 = sortBy lowerboundCmp

lowerboundCmp :: VersionInterval -> VersionInterval -> Ordering
lowerboundCmp (VersionInterval (LowerBound v vb) _) (VersionInterval (LowerBound u ub) _) =
  compare v u <> compareBound vb ub
  where
    compareBound :: Bound -> Bound -> Ordering
    compareBound InclusiveBound InclusiveBound = EQ
    compareBound InclusiveBound ExclusiveBound = LT
    compareBound ExclusiveBound InclusiveBound = GT
    compareBound ExclusiveBound ExclusiveBound = EQ

-------------------------------------------------------------------------------
-- Postprocess
-------------------------------------------------------------------------------

-- Post-processing takes a list of ordered version intervals,
-- but possibly overlapping, and creates 'VersionIntervals'.
{-@ postprocess :: [NonEmptyInterval] -> VersionIntervals @-}
postprocess :: [VersionInterval] -> VersionIntervals
postprocess = checkInvariant . VersionIntervals . stage2and3

{-@ stage2and3 :: [NonEmptyInterval] -> SeparatedIntervals @-}
stage2and3 :: [VersionInterval] -> [VersionInterval]
stage2and3 = stage3 . stage2

{-@ stage3 :: [NonEmptyInterval] -> SeparatedIntervals @-}
stage3 :: [VersionInterval] -> [VersionInterval]
stage3 [] = []
stage3 (VersionInterval lb ub : rest) = stage3go lb ub rest

-- The intervals emitted all start at or after @lb@, which is what makes the
-- interval emitted before them separated from each of them.
{-@ stage3go
      :: lb:LowerBound
      -> ub:{UpperBound | NonEmptyBounds lb ub}
      -> xs:[NonEmptyInterval]
      -> {v:[{i:NonEmptyInterval | LowerGE (viLower i) lb}]<{\x y -> Separated x y}> | len v > 0}
      / [len xs]
  @-}
stage3go :: LowerBound -> UpperBound -> [VersionInterval] -> [VersionInterval]
stage3go !lb NoUpperBound _ = [VersionInterval lb NoUpperBound]
stage3go !lb !ub [] = [VersionInterval lb ub]
stage3go !lb !ub (VersionInterval lb' ub' : rest')
  | doesNotTouch ub lb' = VersionInterval lb ub : stage3go lb' ub' rest'
  | otherwise = stage3go lb (unionUpper ub ub') rest'

-------------------------------------------------------------------------------
-- Intersections
-------------------------------------------------------------------------------

intersectInterval :: VersionInterval -> VersionInterval -> VersionInterval
intersectInterval (VersionInterval lv uv) (VersionInterval lu uu) =
  VersionInterval (intersectLower lv lu) (intersectUpper uv uu)

intersectLower :: LowerBound -> LowerBound -> LowerBound
intersectLower (LowerBound v vb) (LowerBound u ub) = case compare v u of
  EQ -> LowerBound v (intersectBound vb ub)
  LT -> LowerBound u ub
  GT -> LowerBound v vb

intersectUpper :: UpperBound -> UpperBound -> UpperBound
intersectUpper NoUpperBound b = b
intersectUpper b NoUpperBound = b
intersectUpper (UpperBound v vb) (UpperBound u ub) = case compare v u of
  EQ -> UpperBound v (intersectBound vb ub)
  LT -> UpperBound v vb
  GT -> UpperBound u ub

intersectBound :: Bound -> Bound -> Bound
intersectBound InclusiveBound InclusiveBound = InclusiveBound
intersectBound _ _ = ExclusiveBound

-------------------------------------------------------------------------------
-- Unions
-------------------------------------------------------------------------------

{-@ unionUpper :: a:UpperBound -> b:UpperBound -> {v:UpperBound | UpperGE v a && UpperGE v b} @-}
unionUpper :: UpperBound -> UpperBound -> UpperBound
unionUpper NoUpperBound _ = NoUpperBound
unionUpper _ NoUpperBound = NoUpperBound
unionUpper (UpperBound v vb) (UpperBound u ub) = case compare v u of
  EQ -> UpperBound v (unionBound vb ub)
  LT -> UpperBound u ub
  GT -> UpperBound v vb

{-@ unionBound :: a:Bound -> b:Bound -> {v:Bound | v == InclusiveBound || (v == a && v == b)} @-}
unionBound :: Bound -> Bound -> Bound
unionBound ExclusiveBound ExclusiveBound = ExclusiveBound
unionBound _ _ = InclusiveBound

-------------------------------------------------------------------------------
-- VersionRange
-------------------------------------------------------------------------------

-- | View a 'VersionRange' as a union of intervals.
--
-- This provides a canonical view of the semantics of a 'VersionRange' as
-- opposed to the syntax of the expression used to define it. For the syntactic
-- view use 'foldVersionRange'.
--
-- Each interval is non-empty. The sequence is in increasing order and no
-- intervals overlap or touch. Therefore only the first and last can be
-- unbounded. The sequence can be empty if the range is empty
-- (e.g. a range expression like @\< 1 && > 2@).
--
-- Other checks are trivial to implement using this view. For example:
--
-- > isNoVersion vr | [] <- asVersionIntervals vr = True
-- >                | otherwise                   = False
--
-- > isSpecificVersion vr
-- >    | [(LowerBound v  InclusiveBound
-- >       ,UpperBound v' InclusiveBound)] <- asVersionIntervals vr
-- >    , v == v'   = Just v
-- >    | otherwise = Nothing
asVersionIntervals :: VersionRange -> [VersionInterval]
asVersionIntervals = unVersionIntervals . toVersionIntervals

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

-- | Check an upper bound does not intersect, or even touch a lower bound:
--
-- @
--   ---|      or  ---)     but not  ---]     or  ---)     or  ---]
--       |---         (---              (---         [---         [---
-- @
{-@ doesNotTouch :: u:UpperBound -> l:LowerBound -> {v:Bool | v <=> DoesNotTouch u l} @-}
doesNotTouch :: UpperBound -> LowerBound -> Bool
doesNotTouch NoUpperBound _ = False
doesNotTouch (UpperBound u ub) (LowerBound l lb) =
  (u < l) || (u == l && ub == ExclusiveBound && lb == ExclusiveBound)

-------------------------------------------------------------------------------
-- Invariants
-------------------------------------------------------------------------------

-- | 'VersionIntervals' invariant:
--
-- * all intervals are valid (lower bound is less then upper bound, i.e. non-empty)
-- * intervals doesn't touch each other (distinct)
invariantVersionIntervals :: VersionIntervals -> Bool
invariantVersionIntervals (VersionIntervals intervals) =
  all validInterval intervals
    && all doesNotTouch' adjacentIntervals
  where
    doesNotTouch' :: (VersionInterval, VersionInterval) -> Bool
    doesNotTouch' (VersionInterval _ u, VersionInterval l' _) = doesNotTouch u l'

    adjacentIntervals :: [(VersionInterval, VersionInterval)]
    adjacentIntervals = case intervals of
      [] -> []
      (_ : tl) -> zip intervals tl

-- LiquidHaskell's specification of 'assert' asks for a proof of its
-- condition.  The invariant is proven on the list of intervals instead, see
-- 'stage2and3', so this runtime check is left out of verification.
{-@ ignore checkInvariant @-}
checkInvariant :: VersionIntervals -> VersionIntervals
checkInvariant is = assert (invariantVersionIntervals is) is
{-# INLINE checkInvariant #-}

validInterval :: VersionInterval -> Bool
validInterval i@(VersionInterval l u) = validLower l && validUpper u && nonEmptyVI i
  where
    validLower (LowerBound v _) = validVersion v
    validUpper NoUpperBound = True
    validUpper (UpperBound v _) = validVersion v

-- Check an interval is non-empty
--
{-@ nonEmptyVI :: i:VersionInterval -> {v:Bool | v <=> NonEmptyVI i} @-}
nonEmptyVI :: VersionInterval -> Bool
nonEmptyVI (VersionInterval _ NoUpperBound) = True
nonEmptyVI (VersionInterval (LowerBound l lb) (UpperBound u ub)) =
  (l < u) || (l == u && lb == InclusiveBound && ub == InclusiveBound)

-------------------------------------------------------------------------------
-- Conversions
-------------------------------------------------------------------------------

-- | Convert a 'VersionRange' to a sequence of version intervals.
toVersionIntervals :: VersionRange -> VersionIntervals
toVersionIntervals = postprocess . stage1

-- | Convert a 'VersionIntervals' value back into a 'VersionRange' expression
-- representing the version intervals.
fromVersionIntervals :: VersionIntervals -> VersionRange
fromVersionIntervals (VersionIntervals []) = noVersion
fromVersionIntervals (VersionIntervals (x : xs)) = foldr1 unionVersionRanges (fmap intervalToVersionRange (x :| xs))

intervalToVersionRange :: VersionInterval -> VersionRange
intervalToVersionRange (VersionInterval (LowerBound v vb) upper') = case upper' of
  NoUpperBound ->
    lowerBound
  UpperBound u ub
    | vb == InclusiveBound
    , ub == InclusiveBound
    , v == u ->
        thisVersion v
  UpperBound u ub -> withLowerBound (makeUpperBound u ub)
  where
    lowerBound :: VersionRange
    lowerBound = case vb of
      InclusiveBound -> orLaterVersion v
      ExclusiveBound -> laterVersion v

    withLowerBound :: VersionRange -> VersionRange
    withLowerBound vr
      | isVersion0 v, vb == InclusiveBound = vr
      | otherwise = intersectVersionRanges lowerBound vr

    makeUpperBound :: Version -> Bound -> VersionRange
    makeUpperBound u InclusiveBound = orEarlierVersion u
    makeUpperBound u ExclusiveBound = earlierVersion u

-------------------------------------------------------------------------------
-- Normalisation
-------------------------------------------------------------------------------

-- | Since @Cabal-3.6@ this function.. TODO
normaliseVersionRange2 :: VersionRange -> VersionRange
normaliseVersionRange2 = fromVersionIntervals . toVersionIntervals

-------------------------------------------------------------------------------
-- Relaxation
-------------------------------------------------------------------------------

relaxLastInterval :: VersionIntervals -> VersionIntervals
relaxLastInterval (VersionIntervals xs) = VersionIntervals (relaxLastInterval' xs)
  where
    relaxLastInterval' [] = []
    relaxLastInterval' [VersionInterval l _] = [VersionInterval l NoUpperBound]
    relaxLastInterval' (i : is) = i : relaxLastInterval' is

relaxHeadInterval :: VersionIntervals -> VersionIntervals
relaxHeadInterval (VersionIntervals xs) = VersionIntervals (relaxHeadInterval' xs)
  where
    relaxHeadInterval' [] = []
    relaxHeadInterval' (VersionInterval _ u : is) = VersionInterval zeroLowerBound u : is
