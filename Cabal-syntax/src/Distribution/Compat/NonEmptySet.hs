module Distribution.Compat.NonEmptySet
  ( NonEmptySet

    -- * Construction
  , singleton

    -- * Insertion
  , insert

    -- * Deletion
  , delete
  , filter

    -- * Conversions
  , toNonEmpty
  , fromNonEmpty
  , toList
  , toSet

    -- * Query
  , member

    -- * Map
  , map
  ) where

import Prelude (Bool (..), Eq, Maybe (..), Ord (..), Read, Show (..), String, error, otherwise, return, showParen, showString, ($), (++), (.))

import Control.Applicative (empty)
import Control.DeepSeq (NFData (..))
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..))
-- Imported for LiquidHaskell's isJust and fromJust measures, which it only
-- loads for modules that import Data.Maybe.
import Data.Maybe ()
import Data.Semigroup (Semigroup (..))

import qualified Data.Foldable as F
import qualified Data.Set as Set

import Distribution.Compat.Binary (Binary (..))
import Distribution.Utils.Structured

-- | @since 3.4.0.0
newtype NonEmptySet a = NES (Set.Set a)
  deriving (Eq, Ord, Data, Read)

-- The @{-\@ ... \@-}@ comments are LiquidHaskell specifications, checked by
-- building the Cabal-liquid package, see @cabal.liquid.project@, and inert
-- otherwise.  The set inside is never empty, so 'toNonEmpty' cannot reach
-- 'panic', and each operation is described by the set it returns.  In the
-- logic a 'NonEmptySet' is the set it wraps, as a newtype is a cast in GHC
-- Core, so the specifications apply set operations to it directly.
{-@ data NonEmptySet a = NES { nesSet :: {s:Set.Set a | not (Set_emp s)} } @-}
{-@ invariant {v:NonEmptySet a | not (Set_emp v)} @-}

-- LiquidHaskell has no specification for 'Set.map'.
{-@ assume Set.map :: Ord b => (a -> b) -> s:Set.Set a -> {v:Set.Set b | Set_emp v <=> Set_emp s} @-}

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance Show a => Show (NonEmptySet a) where
  showsPrec d s =
    showParen (d > 10) $
      showString "fromNonEmpty "
        . showsPrec 11 (toNonEmpty s)

instance Binary a => Binary (NonEmptySet a) where
  put (NES s) = put s
  get = do
    xs <- get
    if Set.null xs
      then empty
      else return (NES xs)

instance Structured a => Structured (NonEmptySet a) where
  structure = containerStructure

instance NFData a => NFData (NonEmptySet a) where
  rnf (NES x) = rnf x

-- | Note: there aren't @Monoid@ instance.
instance Ord a => Semigroup (NonEmptySet a) where
  NES x <> NES y = NES (Set.union x y)

instance F.Foldable NonEmptySet where
  foldMap f (NES s) = F.foldMap f s
  foldr f z (NES s) = F.foldr f z s
  toList = toList
  null _ = False
  length (NES s) = F.length s

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

{-@ singleton :: x:a -> {v:NonEmptySet a | v == Set_sng x} @-}
singleton :: a -> NonEmptySet a
singleton = NES . Set.singleton

-------------------------------------------------------------------------------
-- Insertion
-------------------------------------------------------------------------------

{-@ insert :: Ord a => x:a -> s:NonEmptySet a -> {v:NonEmptySet a | v == Set_cup s (Set_sng x)} @-}
insert :: Ord a => a -> NonEmptySet a -> NonEmptySet a
insert x (NES xs) = NES (Set.insert x xs)

-------------------------------------------------------------------------------
-- Deletion
-------------------------------------------------------------------------------

-- | The set without the element, or 'Nothing' when that would be empty.
{-@ delete
      :: Ord a
      => x:a
      -> s:NonEmptySet a
      -> {v:Maybe (NonEmptySet a) |
            (isJust v <=> not (Set_emp (Set_dif s (Set_sng x))))
              && (isJust v => fromJust v == Set_dif s (Set_sng x))}
  @-}
delete :: Ord a => a -> NonEmptySet a -> Maybe (NonEmptySet a)
delete x (NES xs)
  | Set.null res = Nothing
  | otherwise = Just (NES res)
  where
    res = Set.delete x xs

-- Assumed: LiquidHaskell has no specification for 'Set.filter', and one
-- written here for it would be taken for this module's 'filter'.
{-@ assume filter :: (a -> Bool) -> s:NonEmptySet a -> {v:Set.Set a | Set_sub v s} @-}
filter :: (a -> Bool) -> NonEmptySet a -> Set.Set a
filter predicate (NES set) = Set.filter predicate set

-------------------------------------------------------------------------------
-- Conversions
-------------------------------------------------------------------------------

fromNonEmpty :: Ord a => NonEmpty a -> NonEmptySet a
fromNonEmpty (x :| xs) = NES (Set.fromList (x : xs))

-- The empty case cannot happen, which LiquidHaskell checks through the
-- precondition of 'panic'.
toNonEmpty :: NonEmptySet a -> NonEmpty a
toNonEmpty (NES s) = case Set.toList s of
  [] -> panic "toNonEmpty"
  x : xs -> x :| xs

-- Going through 'toSet' rather than matching on 'NES' lets LiquidHaskell
-- relate the list to the set; the pattern match is a cast it sees through.
{-@ toList :: s:NonEmptySet a -> {xs:[a] | Set.listElts xs == s} @-}
toList :: NonEmptySet a -> [a]
toList = Set.toList . toSet

{-@ toSet :: s:NonEmptySet a -> {v:Set.Set a | v == s && not (Set_emp v)} @-}
toSet :: NonEmptySet a -> Set.Set a
toSet (NES s) = s

-------------------------------------------------------------------------------
-- Query
-------------------------------------------------------------------------------

{-@ member :: Ord a => x:a -> s:NonEmptySet a -> {v:Bool | v <=> Set_mem x s} @-}
member :: Ord a => a -> NonEmptySet a -> Bool
member x (NES xs) = Set.member x xs

-------------------------------------------------------------------------------
-- Map
-------------------------------------------------------------------------------

map
  :: Ord b
  => (a -> b)
  -> NonEmptySet a
  -> NonEmptySet b
map f (NES x) = NES (Set.map f x)

-------------------------------------------------------------------------------
-- Internal
-------------------------------------------------------------------------------

{-@ panic :: {msg:String | false} -> a @-}
panic :: String -> a
panic msg = error $ "NonEmptySet invariant violated: " ++ msg
