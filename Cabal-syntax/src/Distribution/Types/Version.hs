module Distribution.Types.Version
  ( -- * Package versions
    Version
  , mkVersion
  , mkVersion'
  , versionNumbers
  , nullVersion
  , alterVersion
  , version0

    -- * Internal
  , validVersion
  , versionDigitParser
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Parsec
import Distribution.Pretty

import qualified Data.Version as Base
import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp
import qualified Text.Read as Read

-- | A 'Version' represents the version of a software entity.
--
-- Instances of 'Eq' and 'Ord' are provided, which gives exact
-- equality and lexicographic ordering of the version number
-- components (i.e. 2.1 > 2.0, 1.2.3 > 1.2.2, etc.).
--
-- This type is opaque and distinct from the 'Base.Version' type in
-- "Data.Version" since @Cabal-2.0@. The difference extends to the
-- 'Binary' instance using a different (and more compact) encoding.
--
-- @since 2.0.0.2
data Version
  = PV0 {-# UNPACK #-} !Word64
  | PV1 !Int [Int]
  -- NOTE: If a version fits into the packed Word64
  -- representation (i.e. at most four version components
  -- which all fall into the [0..0xfffe] range), then PV0
  -- MUST be used. This is essential for the 'Eq' instance
  -- to work.
  deriving (Data, Eq, Generic)

-- The @{-\@ ... \@-}@ comments are LiquidHaskell specifications, checked by
-- building the Cabal-liquid package, see @cabal.liquid.project@, and inert
-- otherwise.  They state the representation invariant in the NOTE above and
-- prove that 'mkVersion' and 'versionNumbers' maintain it and are inverses.
--
-- LiquidHaskell has no model of "Data.Bits", and cannot be given one for
-- methods polymorphic in 'Bits' because it only allows arithmetic on type
-- variables constrained by numeric classes.  The four 16-bit fields of a
-- packed word are therefore uninterpreted functions in the logic, @fld1@ to
-- @fld4@ from the most significant end, and the functions that use bit
-- operations, 'inWord16VerRep1' and friends, 'mkWord64VerRep' and
-- 'word16Field', are given @assume@ specifications saying what they do to
-- the fields.  Those are trusted.  Everything built on them is proved.
--
-- The components of a version are described by position, 'vlen' and 'vf1'
-- to 'vf4', because the logic has no syntax for building a list from an
-- element and a tail.  Together with 'vrest' for the tail of a 'PV1' they
-- determine the list, so the specifications of 'mkVersion' and
-- 'versionNumbers' say that one is the inverse of the other.
--
-- A 'PV0' word holds up to four components, each stored plus one in a 16-bit
-- field from the most significant end, so a zero field marks the end.  The
-- fields after the first zero field are zero too.  A 'PV1' holds components
-- that do not fit that encoding.
{-@ data Version
      = PV0 { pv0Word :: {w:Word64 | WellFormedWord w} }
      | PV1 { pv1Head :: Int, pv1Tail :: {ns:[Int] | not (FitsWord pv1Head ns)} }
  @-}

-- The 16-bit fields of a packed word, from the most significant end.  Each
-- holds a component plus one, or zero for no component.
{-@ measure fld1 :: Word64 -> Int @-}
{-@ measure fld2 :: Word64 -> Int @-}
{-@ measure fld3 :: Word64 -> Int @-}
{-@ measure fld4 :: Word64 -> Int @-}
{-@ invariant {w:Word64 |
      0 <= fld1 w && fld1 w <= 65535
        && 0 <= fld2 w && fld2 w <= 65535
        && 0 <= fld3 w && fld3 w <= 65535
        && 0 <= fld4 w && fld4 w <= 65535
        && (w == 0 => fld1 w == 0 && fld2 w == 0 && fld3 w == 0 && fld4 w == 0)}
  @-}

-- A component that fits a 16-bit field once incremented.
{-@ predicate InField X = 0 <= X && X <= 65534 @-}

-- Components that fit the packed representation: at most four, each in a
-- field.
{-@ predicate FitsWord N NS =
      InField N
        && len NS <= 3
        && (len NS >= 1 => InField (head NS))
        && (len NS >= 2 => InField (head (tail NS)))
        && (len NS >= 3 => InField (head (tail (tail NS))))
  @-}

-- The number of components, as 'versionNumbers' computes it: a 'PV0' word
-- ends at its first zero field.
{-@ measure vlen :: Version -> Int
      vlen (PV1 n ns) = 1 + len ns
      vlen (PV0 w) =
        if fld1 w == 0 then 0
        else if fld2 w == 0 then 1
        else if fld3 w == 0 then 2
        else if fld4 w == 0 then 3
        else 4
  @-}

-- The first four components, meaningful up to 'vlen'.
{-@ measure vf1 :: Version -> Int
      vf1 (PV1 n ns) = n
      vf1 (PV0 w) = fld1 w - 1
  @-}
{-@ measure vf2 :: Version -> Int
      vf2 (PV1 n ns) = head ns
      vf2 (PV0 w) = fld2 w - 1
  @-}
{-@ measure vf3 :: Version -> Int
      vf3 (PV1 n ns) = head (tail ns)
      vf3 (PV0 w) = fld3 w - 1
  @-}
{-@ measure vf4 :: Version -> Int
      vf4 (PV1 n ns) = head (tail (tail ns))
      vf4 (PV0 w) = fld4 w - 1
  @-}

-- The components after the first of a 'PV1'.
{-@ measure isPV1 :: Version -> Bool
      isPV1 (PV0 w) = False
      isPV1 (PV1 n ns) = True
  @-}
{-@ measure vrest :: Version -> [Int]
      vrest (PV1 n ns) = ns
  @-}

-- A version whose components are the given list, position by position.
{-@ predicate HasComponents V NS =
      vlen V == len NS
        && (len NS >= 1 => vf1 V == head NS)
        && (len NS >= 2 => vf2 V == head (tail NS))
        && (len NS >= 3 => vf3 V == head (tail (tail NS)))
        && (len NS >= 4 => vf4 V == head (tail (tail (tail NS))))
        && (isPV1 V => vrest V == tail NS)
  @-}

{-@ predicate WellFormedWord W =
      (fld1 W == 0 => fld2 W == 0)
        && (fld2 W == 0 => fld3 W == 0)
        && (fld3 W == 0 => fld4 W == 0)
  @-}

instance Ord Version where
  compare (PV0 x) (PV0 y) = compare x y
  compare (PV1 x xs) (PV1 y ys) = case compare x y of
    EQ -> compare xs ys
    c -> c
  compare (PV0 w) (PV1 y ys) = case compare x y of
    EQ -> compare [x2, x3, x4] ys
    c -> c
    where
      x = word16Field 48 w
      x2 = word16Field 32 w
      x3 = word16Field 16 w
      x4 = word16Field 0 w
  compare (PV1 x xs) (PV0 w) = case compare x y of
    EQ -> compare xs [y2, y3, y4]
    c -> c
    where
      y = word16Field 48 w
      y2 = word16Field 32 w
      y3 = word16Field 16 w
      y4 = word16Field 0 w

instance Show Version where
  showsPrec d v =
    showParen (d > 10) $
      showString "mkVersion "
        . showsPrec 11 (versionNumbers v)

instance Read Version where
  readPrec = Read.parens $ do
    Read.Ident "mkVersion" <- Read.lexP
    v <- Read.step Read.readPrec
    return (mkVersion v)

instance Binary Version
instance Structured Version

instance NFData Version where
  rnf (PV0 _) = ()
  rnf (PV1 _ ns) = rnf ns

instance Pretty Version where
  pretty ver =
    Disp.hcat
      ( Disp.punctuate
          (Disp.char '.')
          (map Disp.int $ versionNumbers ver)
      )

instance Parsec Version where
  parsec = (mkVersion . toList <$> P.sepByNonEmpty versionDigitParser (P.char '.')) <* tags
    where
      tags = do
        ts <- many $ P.char '-' *> some (P.satisfy isAlphaNum)
        case ts of
          [] -> pure ()
          (_ : _) -> parsecWarning PWTVersionTag "version with tags"

-- | An integral without leading zeroes.
--
-- @since 3.0
versionDigitParser :: CabalParsing m => m Int
versionDigitParser = (some d >>= toNumber) P.<?> "version digit (integral without leading zeroes)"
  where
    toNumber :: CabalParsing m => [Int] -> m Int
    toNumber [0] = return 0
    toNumber (0 : _) = P.unexpected "Version digit with leading zero"
    toNumber xs
      -- 10^9 = 1000000000
      -- 2^30 = 1073741824
      --
      -- GHC Int is at least 32 bits, so 2^31-1 is the 'maxBound'.
      | length xs > 9 = P.unexpected "At most 9 numbers are allowed per version number part"
      | otherwise = return $ foldl' (\a b -> a * 10 + b) 0 xs

    d :: P.CharParsing m => m Int
    d = f <$> P.satisfyRange '0' '9'
    f c = ord c - ord '0'

-- | Construct 'Version' from list of version number components.
--
-- For instance, @mkVersion [3,2,1]@ constructs a 'Version'
-- representing the version @3.2.1@.
--
-- All version components must be non-negative. @mkVersion []@
-- currently represents the special /null/ version; see also 'nullVersion'.
--
-- @since 2.0.0.2
{-@ mkVersion :: ns:[Int] -> {v:Version | HasComponents v ns} @-}
mkVersion :: [Int] -> Version
-- TODO: add validity check; disallow 'mkVersion []' (we have
-- 'nullVersion' for that)
mkVersion [] = nullVersion
mkVersion [v1]
  | inWord16VerRep1 v1 = PV0 (mkWord64VerRep1 v1)
  | otherwise = PV1 v1 []
mkVersion (v1 : vs@[v2])
  | inWord16VerRep2 v1 v2 = PV0 (mkWord64VerRep2 v1 v2)
  | otherwise = PV1 v1 vs
mkVersion (v1 : vs@[v2, v3])
  | inWord16VerRep3 v1 v2 v3 = PV0 (mkWord64VerRep3 v1 v2 v3)
  | otherwise = PV1 v1 vs
mkVersion (v1 : vs@[v2, v3, v4])
  | inWord16VerRep4 v1 v2 v3 v4 = PV0 (mkWord64VerRep4 v1 v2 v3 v4)
  | otherwise = PV1 v1 vs
mkVersion (v1 : vs) = PV1 v1 vs

-- Whether the components fit the packed representation.  Or-ing each
-- component with its successor checks that both are below 2^16, so the
-- component is in [0, 0xfffe].
{-@ assume inWord16VerRep1 :: x1:Int -> {v:Bool | v <=> InField x1} @-}
inWord16VerRep1 :: Int -> Bool
inWord16VerRep1 x1 = inWord16 (x1 .|. (x1 + 1))

{-@ assume inWord16VerRep2 :: x1:Int -> x2:Int -> {v:Bool | v <=> InField x1 && InField x2} @-}
inWord16VerRep2 :: Int -> Int -> Bool
inWord16VerRep2 x1 x2 =
  inWord16
    ( x1
        .|. (x1 + 1)
        .|. x2
        .|. (x2 + 1)
    )

{-@ assume inWord16VerRep3 :: x1:Int -> x2:Int -> x3:Int -> {v:Bool | v <=> InField x1 && InField x2 && InField x3} @-}
inWord16VerRep3 :: Int -> Int -> Int -> Bool
inWord16VerRep3 x1 x2 x3 =
  inWord16
    ( x1
        .|. (x1 + 1)
        .|. x2
        .|. (x2 + 1)
        .|. x3
        .|. (x3 + 1)
    )

{-@ assume inWord16VerRep4 :: x1:Int -> x2:Int -> x3:Int -> x4:Int -> {v:Bool | v <=> InField x1 && InField x2 && InField x3 && InField x4} @-}
inWord16VerRep4 :: Int -> Int -> Int -> Int -> Bool
inWord16VerRep4 x1 x2 x3 x4 =
  inWord16
    ( x1
        .|. (x1 + 1)
        .|. x2
        .|. (x2 + 1)
        .|. x3
        .|. (x3 + 1)
        .|. x4
        .|. (x4 + 1)
    )

-- The packed representation of components that fit, see 'inWord16VerRep1'
-- and friends.
{-@ mkWord64VerRep1 :: y1:{Int | InField y1} -> {w:Word64 | fld1 w == y1 + 1 && fld2 w == 0 && fld3 w == 0 && fld4 w == 0} @-}
mkWord64VerRep1 :: Int -> Word64
mkWord64VerRep1 y1 = mkWord64VerRep (y1 + 1) 0 0 0

{-@ mkWord64VerRep2 :: y1:{Int | InField y1} -> y2:{Int | InField y2} -> {w:Word64 | fld1 w == y1 + 1 && fld2 w == y2 + 1 && fld3 w == 0 && fld4 w == 0} @-}
mkWord64VerRep2 :: Int -> Int -> Word64
mkWord64VerRep2 y1 y2 = mkWord64VerRep (y1 + 1) (y2 + 1) 0 0

{-@ mkWord64VerRep3 :: y1:{Int | InField y1} -> y2:{Int | InField y2} -> y3:{Int | InField y3} -> {w:Word64 | fld1 w == y1 + 1 && fld2 w == y2 + 1 && fld3 w == y3 + 1 && fld4 w == 0} @-}
mkWord64VerRep3 :: Int -> Int -> Int -> Word64
mkWord64VerRep3 y1 y2 y3 = mkWord64VerRep (y1 + 1) (y2 + 1) (y3 + 1) 0

{-@ mkWord64VerRep4 :: y1:{Int | InField y1} -> y2:{Int | InField y2} -> y3:{Int | InField y3} -> y4:{Int | InField y4} -> {w:Word64 | fld1 w == y1 + 1 && fld2 w == y2 + 1 && fld3 w == y3 + 1 && fld4 w == y4 + 1} @-}
mkWord64VerRep4 :: Int -> Int -> Int -> Int -> Word64
mkWord64VerRep4 y1 y2 y3 y4 = mkWord64VerRep (y1 + 1) (y2 + 1) (y3 + 1) (y4 + 1)

-- | Version 0. A lower bound of 'Version'.
--
-- @since 2.2
{-@ version0 :: {v:Version | vlen v == 1 && vf1 v == 0} @-}
version0 :: Version
version0 = mkVersion [0]

-- Each component goes into a 16-bit field, the first component in the most
-- significant one.
{-@ assume mkWord64VerRep
      :: v1:{Int | 0 <= v1 && v1 <= 65535}
      -> v2:{Int | 0 <= v2 && v2 <= 65535}
      -> v3:{Int | 0 <= v3 && v3 <= 65535}
      -> v4:{Int | 0 <= v4 && v4 <= 65535}
      -> {w:Word64 | fld1 w == v1 && fld2 w == v2 && fld3 w == v3 && fld4 w == v4}
  @-}
{-# INLINE mkWord64VerRep #-}
mkWord64VerRep :: Int -> Int -> Int -> Int -> Word64
mkWord64VerRep v1 v2 v3 v4 =
  (fromIntegral v1 `shiftL` 48)
    .|. (fromIntegral v2 `shiftL` 32)
    .|. (fromIntegral v3 `shiftL` 16)
    .|. fromIntegral v4

{-# INLINE inWord16 #-}
inWord16 :: Int -> Bool
inWord16 x = (fromIntegral x :: Word) <= 0xffff

-- | Variant of 'mkVersion' which converts a "Data.Version"
-- 'Base.Version' into Cabal's 'Version' type.
--
-- @since 2.0.0.2
mkVersion' :: Base.Version -> Version
mkVersion' = mkVersion . Base.versionBranch

-- | Unpack 'Version' into list of version number components.
--
-- This is the inverse to 'mkVersion', so the following holds:
--
-- > (versionNumbers . mkVersion) vs == vs
--
-- @since 2.0.0.2
{-@ versionNumbers :: v:Version -> {ns:[Int] | HasComponents v ns} @-}
versionNumbers :: Version -> [Int]
versionNumbers (PV1 n ns) = n : ns
versionNumbers (PV0 w)
  | v1 < 0 = []
  | v2 < 0 = [v1]
  | v3 < 0 = [v1, v2]
  | v4 < 0 = [v1, v2, v3]
  | otherwise = [v1, v2, v3, v4]
  where
    v1 = word16Field 48 w
    v2 = word16Field 32 w
    v3 = word16Field 16 w
    v4 = word16Field 0 w

-- The component stored in the 16-bit field starting at the given bit of a
-- packed word, or -1 for an empty field, see 'mkWord64VerRep'.
{-@ assume word16Field
      :: s:{Int | s == 0 || s == 16 || s == 32 || s == 48}
      -> w:Word64
      -> {v:Int | (s == 48 => v == fld1 w - 1)
               && (s == 32 => v == fld2 w - 1)
               && (s == 16 => v == fld3 w - 1)
               && (s == 0 => v == fld4 w - 1)}
  @-}
{-# INLINE word16Field #-}
word16Field :: Int -> Word64 -> Int
word16Field s w = fromIntegral ((w `shiftR` s) .&. 0xffff) - 1

-- | Constant representing the special /null/ 'Version'
--
-- The 'nullVersion' compares (via 'Ord') as less than every proper
-- 'Version' value.
--
-- @since 2.0.0.2
{-@ nullVersion :: {v:Version | vlen v == 0 && not (isPV1 v)} @-}
nullVersion :: Version
-- TODO: at some point, 'mkVersion' may disallow creating /null/
-- 'Version's
nullVersion = PV0 0

-- | Apply function to list of version number components
--
-- > alterVersion f == mkVersion . f . versionNumbers
--
-- @since 2.0.0.2
alterVersion :: ([Int] -> [Int]) -> Version -> Version
alterVersion f = mkVersion . f . versionNumbers

-- internal helper
validVersion :: Version -> Bool
validVersion v = v /= nullVersion && all (>= 0) (versionNumbers v)
