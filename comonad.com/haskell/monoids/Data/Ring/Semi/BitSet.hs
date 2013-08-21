{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, DeriveDataTypeable, BangPatterns, PatternGuards, TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Ring.Semi.BitSet
-- Copyright   :  (c) Edward Kmett 2009. 
--                Based on Data.BitSet (c) Denis Bueno 2008-2009
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  portable (instances use MPTCs)
--
-- Replacement for "Data.BitSet" extended to handle enumerations where fromEnum
-- can return negative values, support efficient intersection and union
-- and allow complementing of the set with respect to the bounds of the
-- enumeration. Treated as a Boolean semiring over `.&.`/`.|.`. To get a
-- 'Boolean' 'Ring', use @'Boolean' ('BitSet' a)@.
--
-------------------------------------------------------------------------------

module Data.Ring.Semi.BitSet
    ( module Data.Monoid.Reducer
    , module Data.Ring
    -- * BitSet
    , BitSet
    -- * Manipulation
    , empty
    , singleton
    , full
    , union
    , intersection
    , complement
    , insert
    , delete
    , (\\)
    , fromList
    , fromDistinctAscList
    -- * Acessors
    , member
    , null
    , size
    , isComplemented
    , toInteger
    ) where

import Prelude hiding ( null, exponent, toInteger, foldl, foldr, foldl1, foldr1 )
import Data.Bits
import Data.Foldable hiding ( toList )
import Data.Data
import Data.Ring.Semi.Natural
import Data.Ring
import Data.Monoid.Reducer
import Data.Generator
import Data.Ring.Module
import Text.Read
import Text.Show

-- | Set operations optimized for tightly grouped sets or nearly universal sets with a close by group of elements missing.
--   Stores itself like an arbitrary precision floating point number, tracking the least valued member of the set and an
--   Integer comprised of the members. 
data BitSet a = BS 
        { _countAtLeast  :: {-# UNPACK #-} !Int       -- ^ A conservative upper bound on the element count.
                                                      --   If negative, we are complemented with respect to the universe
        , _countAtMost   :: {-# UNPACK #-} !Int       -- ^ A conservative lower bound on the element count.
                                                      --   If negative, we are complemented with respect to the universe
        , _count         ::                 Int       -- ^ Lazy element count used when the above two disagree. O(1) environment size
        , exponent       :: {-# UNPACK #-} !Int       -- ^ Low water mark. index of the least element potentially in the set.
        , _hwm           :: {-# UNPACK #-} !Int       -- ^ High water mark. index of the greatest element potentially in the set.
        , mantissa       :: {-# UNPACK #-} !Integer   -- ^ the set of bits starting from the exponent.
                                                      --   if negative, then we are complmenented with respect to universe
        , _universe      ::                 (Int,Int) -- ^ invariant: whenever mantissa < 0, universe = (fromEnum minBound,fromEnum maxBound)
        , _fromEnum      ::                 Int -> a  -- ^ self-contained extraction behavior, enables Foldable
        } deriving (Typeable)

-- | omit reflection to preserve abstraction
instance (Enum a, Data a) => Data (BitSet a) where
    gfoldl f z im = z fromList `f` toList im
    toConstr _ = error "toConstr"
    gunfold _ _ = error "gunfold"
    dataTypeOf _ = mkNorepType "Data.Ring.Semi.BitSet.BitSet"
    dataCast1 f = gcast1 f 

-- | Internal smart constructor. Forces count whenever it is pigeonholed.
bs :: Enum a => Int -> Int -> Int -> Int -> Int -> Integer -> (Int,Int) -> BitSet a
bs !a !b c !l !h !m u | a == b    = BS a a a l h m u toEnum
                      | otherwise = BS a b c l h m u toEnum
{-# INLINE bs #-}

-- | /O(d)/ where /d/ is absolute deviation in the output of fromEnum over the set
toList :: BitSet a -> [a]
toList (BS _ _ _ l h m u f) 
    | m < 0 = map f [ul..max (pred l) ul] ++ toList' l (map f [min (succ h) uh..uh])
    | otherwise = toList' 0 []
    where
        ~(ul,uh) = u
        toList' !n t 
            | n > h = t
            | testBit m (n - l) = f n : toList' (n+1) t
            | otherwise         = toList' (n+1) t
{-# INLINE toList #-}

-- | /O(1)/ The empty set. Permits /O(1)/ null and size.
empty :: Enum a => BitSet a
empty = BS 0 0 0 0 0 0 undefined toEnum
{-# INLINE empty #-}

-- | /O(1)/ Construct a @BitSet@ with a single element. Permits /O(1)/ null and size
singleton :: Enum a => a -> BitSet a 
singleton x = BS 1 1 1 e e 1 undefined toEnum where e = fromEnum x
{-# INLINE singleton #-}

-- | /O(1)/ amortized cost. Is the 'BitSet' empty? May be faster than checking if @'size' == 0@.
null :: BitSet a -> Bool
null (BS a b c _ _ _ _ _) 
    | a > 0     = False
    | b == 0    = True
    | otherwise = c == 0 
{-# INLINE null #-}

-- | /O(1)/ amortized cost. The number of elements in the bit set.
size :: BitSet a -> Int
size (BS a b c _ _ m (ul,uh) _) 
    | a == b, m >= 0 = a
    | a == b         = uh - ul - a 
    | m >= 0         = c
    | otherwise      = uh - ul - c 
{-# INLINE size #-}

-- | /O(d)/ A 'BitSet' containing every member of the enumeration of @a@.
full :: (Enum a, Bounded a) => BitSet a
full = complement' empty 
{-# INLINE full #-}


-- | /O(d)/ unsafe internal method: complement a set that has already been complemented at least once.
recomplement :: BitSet a -> BitSet a 
recomplement (BS a b c l h m u f) = BS (complement b) (complement a) (complement c) l h (complement m) u f
{-# INLINE recomplement #-}

-- | /O(d)/ unsafe internal method: complement a set that has already been complemented at least once.
pseudoComplement :: BitSet a -> (Int,Int) -> BitSet a 
pseudoComplement (BS a b c l h m _ f) u = BS (complement b) (complement a) (complement c) l h (complement m) u f
{-# INLINE pseudoComplement #-}

-- | /O(d * n)/ Make a 'BitSet' from a list of items.
fromList :: Enum a => [a] -> BitSet a
fromList = foldr insert empty 
{-# INLINE fromList #-}

-- | /O(d * n)/ Make a 'BitSet' from a distinct ascending list of items
fromDistinctAscList :: Enum a => [a] -> BitSet a 
fromDistinctAscList [] = empty
fromDistinctAscList (c:cs) = fromDistinctAscList' cs 1 0 1 
    where
        l = fromEnum c
        fromDistinctAscList' :: Enum a => [a] -> Int -> Int -> Integer -> BitSet a
        fromDistinctAscList' [] !n !h !m  = BS n n n l h m undefined toEnum
        fromDistinctAscList' (c':cs') !n _ !m = 
            let h' = fromEnum c' in 
            fromDistinctAscList' cs' (n+1) h' (setBit m (h' - l))
{-# INLINE fromDistinctAscList #-}

-- | /O(d)/ Insert a single element of type @a@ into the 'BitSet'. Preserves order of 'null' and 'size'
insert :: Enum a => a -> BitSet a -> BitSet a
insert x r@(BS a b c l h m u _)  
    | m < 0, e < l = r 
    | m < 0, e > h = r
    | b == 0       = singleton x
    | a == -1      = r
    | e < l        = bs (a+1) (b+1) (c+1) e h (shiftL m (l - e) .|. 1) u
    | e > h        = bs (a+1) (b+1) (c+1) l p (setBit m p) u
    | testBit m p  = r 
    | otherwise    = bs (a+1) (b+1) (c+1) l h (setBit m p) u
    where 
        e = fromEnum x
        p = e - l 
{-# INLINE insert #-}

-- | /O(d)/ Delete a single item from the 'BitSet'. Preserves order of 'null' and 'size'
delete :: Enum a => a -> BitSet a -> BitSet a
delete x r@(BS a b c l h m u _) 
    | m < 0, e < l = bs (a+1) (b+1) (c+1) e h (shiftL m (l - e) .&. complement 1) u
    | m < 0, e > h = bs (a+1) (b+1) (c+1) l p (clearBit m p) u
    | b == 0       = r
    | a == -1      = pseudoComplement (singleton x) u
    | e < l        = r
    | e > h        = r
    | testBit m p  = bs (a-1) (b-1) (c-1) l h (clearBit m p) u
    | otherwise    = r
    where 
        e = fromEnum x
        p = e - l
{-# INLINE delete #-}

-- | /O(1)/ Test for membership in a 'BitSet'
member :: Enum a => a -> BitSet a -> Bool
member x (BS _ _ _ l h m _ _) 
    | e < l     = m < 0 
    | e > h     = m > 0
    | otherwise = testBit m (e - l)
    where 
        e = fromEnum x
{-# INLINE member #-}

-- | /O(d)/ convert to an Integer representation. Discards negative elements
toInteger :: BitSet a -> Integer
toInteger x = mantissa x `shift` exponent x
{-# INLINE toInteger #-}

-- | /O(d)/.
union :: Enum a => BitSet a -> BitSet a -> BitSet a 
union x@(BS a b c l h m u f) y@(BS a' b' c' l' h' m' u' _)
    | l' < l        = union y x                                                         -- ensure left side has lower exponent
    | b == 0        = y                                                                 -- fast empty union
    | b' == 0       = x                                                                 -- fast empty union
    | a == -1       = entire u                                                          -- fast full union, recomplement obligation met by negative size
    | a' == -1      = entire u'                                                         -- fast full union, recomplement obligation met by negative size
    | m < 0, m' < 0 = recomplement (intersection (recomplement x) (recomplement y))     -- appeal to intersection, recomplement obligation met by 2s complement
    | m' < 0        = recomplement (diff (recomplement y) x u')                         -- union with complement, recomplement obligation met by 2s complement
    | m < 0         = recomplement (diff (recomplement x) y u)                          -- union with complement, recomplement obligation met by 2s complement
    | h < l'        = bs (a + a') (b + b') (c + c') l h' m'' u                          -- disjoint positive ranges
    | otherwise     = bs (a `max` a') (b + b') (recount m'') l (h `max` h') m'' u       -- overlapped positives
    where 
        m'' = m .|. shiftL m' (l' - l)
        entire u'' = BS (-1) (-1) (-1) 0 0 (-1) u'' f

-- | /O(1)/ Check to see if we are represented as a complemented 'BitSet'. 
isComplemented :: Enum a => BitSet a -> Bool
isComplemented = (<0) . mantissa 
{-# INLINE isComplemented #-}

-- | /O(d)/ 
intersection :: Enum a => BitSet a -> BitSet a -> BitSet a 
intersection x@(BS a b _ l h m u _) y@(BS a' b' _ l' h' m' u' _)
    | l' < l = intersection y x                                 
    | b == 0 = empty
    | b' == 0 = empty
    | a == -1 = y
    | a' == -1 = x
    | m < 0, m' < 0 = recomplement (union (recomplement x) (recomplement y))
    | m' < 0 = diff x (recomplement y) u'
    | m < 0 = diff y (recomplement x) u
    | h < l' = empty 
    | otherwise = bs 0 (b `min` b') (recount m'') l'' (h `min` h') m'' u
    where
        l'' = max l l'
        m'' = shift m (l'' - l) .&. shift m' (l'' - l')

-- | Unsafe internal method for computing differences in a known universe of discourse.
--
-- Preconditions:
--
-- (1) @m >= 0@
-- 2   @m' >= 0@
-- 3   @a /= -1@
-- 4   @a' /= -1@
-- 5   @b /= 0@
-- 6   @b' /= 0@
-- 7   @u''@ is a previously obtained copy of @(fromEnum minBound, fromEnum maxBound)@
--
diff :: Enum a => BitSet a -> BitSet a -> (Int,Int) -> BitSet a 
diff x@(BS a _ _ l h m _ _) (BS _ b' _ l' h' m' _ _) u''
    | h < l' = x
    | h' < l = x
    | otherwise = bs (max (a - b') 0) a (recount m'') l h m'' u''
    where 
        m'' = m .&. shift (complement m') (l' - l)
{-# INLINE diff #-}

-- | /O(d)/ Remove all elements present in the second bitset from the first
difference :: Enum a => BitSet a -> BitSet a -> BitSet a 
difference x@(BS a b _ _ _ m u _)  y@(BS a' b' _ _ _ m' _ _) 
   | a == -1       = pseudoComplement y u
   | a' == -1      = empty
   | b == 0        = empty
   | b' == 0       = x
   | m < 0, m' < 0 = diff (recomplement y) (recomplement x) u
   | m < 0         = pseudoComplement (recomplement x `union` y) u
   | m' < 0        = x `union` recomplement y 
   | otherwise     = diff x y u
    
-- | /O(d)/ Infix 'difference'
(\\) :: Enum a => BitSet a -> BitSet a -> BitSet a 
(\\) = difference
{-# INLINE (\\) #-}

instance Eq (BitSet a) where
    x@(BS _ _ _ l _ m u _) == y@(BS _ _ _ l' _ m' _ _)
        | signum m == signum m' = shift m (l - l'') == shift m' (l' - l'') 
        | m' < 0                = y == x
        | otherwise             = mask .&. shift m (l - ul) == shift m' (l - ul)
        where 
            l'' = min l l'
            mask = setBit 0 (uh - ul + 1) - 1
            ul = fst u
            uh = snd u

instance (Enum a, Bounded a) => Bounded (BitSet a) where
    minBound = empty
    maxBound = result where
        result = BS n n n l h m (l,h) toEnum
        n = h - l + 1
        l = fromEnum (minBound `asArgTypeOf` result)
        h = fromEnum (maxBound `asArgTypeOf` result)
        m = setBit 0 n - 1

-- | Utility function to avoid requiring ScopedTypeVariables
asArgTypeOf :: a -> f a -> a
asArgTypeOf = const
{-# INLINE asArgTypeOf #-}

-- | /O(d)/
recount :: Integer -> Int
recount !n 
    | n < 0     = complement (recount (complement n))
    | otherwise = recount' 0 0 
    where
        h = hwm n
        recount' !i !c
            | i > h = c
            | otherwise = recount' (i+1) (if testBit n i then c+1 else c)

-- | /O(d)/. Computes the equivalent of (truncate . logBase 2 . abs) extended with 0 at 0
hwm :: Integer -> Int
hwm !n 
    | n < 0 = hwm (-n)
    | n > 1 = scan p (2*p) 
    | otherwise = 0
    where
        p = probe 1
        -- incrementally compute 2^(2^(i+1)) until it exceeds n
        probe :: Int -> Int
        probe !i
            | bit (2*i) > n = i
            | otherwise     = probe (2*i)

        -- then scan the powers for the highest set bit
        scan :: Int -> Int -> Int
        scan !l !h
            | l == h        = l
            | bit (m+1) > n = scan l m
            | otherwise     = scan (m+1) h
            where 
                m = l + (h - l) `div` 2
 
instance Show a => Show (BitSet a) where
   showsPrec d x@(BS _ _ _ _ _ m u _)
        | m < 0     = showParen (d > 10) $ showString "pseudoComplement " . showsPrec 11 (recomplement x) . showString " " . showsPrec 11 u
        | otherwise = showParen (d > 10) $ showString "fromDistinctAscList " . showsPrec 11 (toList x)

instance (Enum a, Read a) => Read (BitSet a) where
    readPrec = parens $ complemented +++ normal where
        complemented = prec 10 $ do 
                Ident "pseudoComplement" <- lexP
                x <- step readPrec
                pseudoComplement x `fmap` step readPrec
        normal = prec 10 $ do
                Ident "fromDistinctAscList" <- lexP
                fromDistinctAscList `fmap` step readPrec

-- note that operations on values generated by toEnum are pretty slow because the bounds are suboptimal
instance (Enum a, Bounded a) => Enum (BitSet a) where
    fromEnum b@(BS _ _ _ l _ m _ _) = fromInteger (shiftL m (l - l'))
        where 
            l' = fromEnum (minBound `asArgTypeOf` b)
    toEnum i = result 
        where
            result = BS a i (recount m) l h m undefined toEnum -- n <= 2^n, so i serves as a valid upper bound
            l = fromEnum (minBound `asArgTypeOf` result)
            h = fromEnum (maxBound `asArgTypeOf` result)
            m = fromIntegral i
            a | m /= 0 = 1 -- allow a fast null check, but not much else
              | otherwise = 0

instance Foldable BitSet where
    fold = fold . toList
    foldMap f = foldMap f . toList
    foldr f z = foldr f z . toList
    foldl f z = foldl f z . toList
    foldr1 f = foldr1 f . toList
    foldl1 f = foldl1 f . toList
        
instance Enum a => Monoid (BitSet a) where
    mempty = empty
    mappend = union

instance Enum a => Reducer a (BitSet a) where
    unit = singleton
    snoc = flip insert
    cons = insert

instance (Bounded a, Enum a) => Multiplicative (BitSet a) where
    one = full
    times = intersection

instance (Bounded a, Enum a) => Ringoid (BitSet a)
instance (Bounded a, Enum a) => LeftSemiNearRing (BitSet a)
instance (Bounded a, Enum a) => RightSemiNearRing (BitSet a)
instance (Bounded a, Enum a) => SemiRing (BitSet a)

-- idempotent monoid
instance Enum a => Module Natural (BitSet a)
instance Enum a => LeftModule Natural (BitSet a) where
    0 *. _ = empty
    _ *. m = m
instance Enum a => RightModule Natural (BitSet a) where
    _ .* 0 = empty
    m .* _ = m
instance Enum a => Bimodule Natural (BitSet a)
instance (Bounded a, Enum a) => Algebra Natural (BitSet a)

instance (Bounded a, Enum a) => Module (BitSet a) (BitSet a)
instance (Bounded a, Enum a) => LeftModule (BitSet a) (BitSet a) where (*.) = times
instance (Bounded a, Enum a) => RightModule (BitSet a) (BitSet a) where (.*) = times
instance (Bounded a, Enum a) => Bimodule (BitSet a) (BitSet a)
instance (Bounded a, Enum a) => Algebra (BitSet a) (BitSet a)
    
instance Generator (BitSet a) where
    type Elem (BitSet a) = a
    mapReduce f = mapReduce f . toList

instance (Show a, Bounded a, Enum a) => Num (BitSet a) where
    (+) = union
    (-) = difference
    (*) = intersection
    fromInteger m = r where
        r = BS c c c 0 (hwm m) m u toEnum where
        c = recount m
        u = (fromEnum (minBound `asArgTypeOf` r), fromEnum (maxBound `asArgTypeOf` r))
    abs b | mantissa b < 0 = recomplement b
          | otherwise = b
    signum = error "BitSet.signum undefined"

instance (Show a, Bounded a, Enum a) => Bits (BitSet a) where
    (.&.) = intersection
    (.|.) = union
    a `xor` b = (a .|. b) .&. complement (a .&. b)

    -- | /O(d)/ Complements a 'BitSet' with respect to the bounds of @a@. Preserves order of 'null' and 'size'
    complement r@(BS a b c l h m _ _) = BS (complement b) (complement a) (complement c) l h (complement m) u toEnum where
        u = (fromEnum (minBound `asArgTypeOf` r), fromEnum (maxBound `asArgTypeOf` r))
    {-# INLINE complement #-}
    {-
    shift (BS a b c l h m _ f) n = BS a b c ((l + r) `max` uh) ((h + r) `max` uh) m (ul,uh) toEnum) where
        ul = fromEnum (minBound `asArgTypeOf` r)
        uh = fromEnum (maxBound `asArgTypeOf` r)
    -}
    shift = error "BitSet.shift undefined"
    rotate = error "BitSet.rotate undefined"
    bit = singleton . toEnum
    setBit s b = s `union` singleton (toEnum b)
    clearBit s b = s `difference` singleton (toEnum b)
    complementBit s b = s `xor` singleton (toEnum b)
    testBit s b = member (toEnum b) s 
    bitSize r = fromEnum (maxBound `asArgTypeOf` r) - fromEnum (minBound `asArgTypeOf` r)
    isSigned _ = True

complement' :: (Bounded a, Enum a) => BitSet a -> BitSet a
complement' r@(BS a b c l h m _ _) = BS (complement b) (complement a) (complement c) l h (complement m) u toEnum where
    u = (fromEnum (minBound `asArgTypeOf` r), fromEnum (maxBound `asArgTypeOf` r))
