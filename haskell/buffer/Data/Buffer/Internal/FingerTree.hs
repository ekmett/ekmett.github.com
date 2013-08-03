{-# LANGUAGE CPP #-}
-- We cannot actually specify all the language pragmas, see ghc ticket #
-- If we could, these are what they would be:
{- LANGUAGE MagicHash, UnboxedTuples, NamedFieldPuns, BangPatterns, RecordWildCards -}
{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module      : Data.Buffer.Internal.FingerTree
-- Copyright   : (c) Edward Kmett 2010
--               (c) Ross Paterson 2005
-- License     : BSD-style
-- Maintainer  : ekmett@gmail.com
-- Stability   : experimental
-- Portability : portable
-- 
-- General purpose finite sequences.
-- Apart from being finite and having strict operations, sequences
-- also differ from lists in supporting a wider variety of operations
-- efficiently.
--
-- An amortized running time is given for each operation, with /n/ referring
-- to the length of the sequence and /i/ being the integral index used by
-- some operations.  These bounds hold even in a persistent (shared) setting.
--
-- The implementation uses 2-3 finger trees annotated with sizes,
-- as described in section 4.2 of
--
--    * Ralf Hinze and Ross Paterson,
--  \"Finger trees: a simple general-purpose data structure\",
--  /Journal of Functional Programming/ 16:2 (2006) pp 197-217.
--  <http://www.soi.city.ac.uk/~ross/papers/FingerTree.html>
--
-- /Note/: Many of these operations have the same names as similar
-- operations on lists in the "Prelude".  The ambiguity may be resolved
-- using either qualification or the @hiding@ clause.
--
-- This module provides potentially unsafe access to the guts of the Rope
-- implementation
-----------------------------------------------------------------------------

#include "measure.h"

module Data.Buffer.Internal.FingerTree (
    FingerTree(..),
    Node(..),
    Digit(..),
    -- * Construction
    empty,        -- :: FingerTree a
    singleton,    -- :: Measured a => a -> FingerTree a
    cons,         -- :: Measured a => a -> FingerTree a -> FingerTree a
    snoc,         -- :: Measured a => FingerTree a -> a -> FingerTree a
    last,         -- :: Measured a => FingerTree a -> a
    append,       -- :: Measured a => FingerTree a -> FingerTree a -> FingerTree a
    fromList,     -- :: Measured a => [a] -> FingerTree a

    -- * Partial functions
    head,         -- :: FingerTree a -> a
    last,         -- :: FingerTree a -> a
    any,          -- :: (a -> Bool) -> FingerTree a -> Bool
    all,          -- :: (a -> Bool) -> FingerTree a -> Bool

    -- ** Queries
    null,         -- :: FingerTree a -> Bool
    viewl,        -- :: Measured a => FingerTree a -> ViewL (FingerTree a) a
    viewr         -- :: Measured a => FingerTree a -> ViewR (FingerTree a) a
    ) where

import Prelude hiding (
    null, length, take, drop, splitAt, foldl, foldl1, foldr, foldr1,
    reverse, head, last)
import qualified Data.List (foldl')
import Control.Applicative (Applicative(..), (<$>))
import Control.Monad (MonadPlus(..))
import Data.Monoid (Monoid(..))
import Data.Foldable
import Data.Traversable
#ifndef __GLASGOW_HASKELL__
import Data.Typeable (Typeable, typeOf, typeOfDefault)
#endif
import Data.Typeable (TyCon, Typeable1(..), mkTyCon, mkTyConApp )

#ifdef __GLASGOW_HASKELL__
import Text.Read (Lexeme(Ident), lexP, parens, prec,
    readPrec, readListPrec, readListPrecDefault)
import Data.Data (Data(..), DataType, Constr, Fixity(..),
                             mkConstr, mkDataType, constrIndex, gcast1)
#endif

#if TESTING
import Control.Monad (liftM, liftM3, liftM4)
import Test.QuickCheck
#endif

infixr 5 `consTree`
infixl 5 `snocTree`

infixr 5 `append`
infixr 5 `cons`, :<
infixl 5 `snoc`, :>

class Valid a where
    valid :: a -> Bool

data FingerTree a
    = Empty
    | Single a
    | Deep {-# UNPACK #-} !Int {-# UNPACK #-} !Int !(Digit a) (FingerTree (Node a)) !(Digit a)
#if TESTING
    deriving Show
#endif

instance Monoid (FingerTree a) where
    mempty = empty
    mappend = append

instance Measured a => Measured (FingerTree a) where
    {-# SPECIALIZE instance Measured (FingerTree Buffer) #-}
    {-# SPECIALIZE instance Measured (FingerTree (Node a)) #-}
    measureU Empty = 0
    measureU (Single x) = measureU x
    measureU (Deep v w _ _ _) = PACK(v, w)

instance Sized a => Sized (FingerTree a) where
    {-# SPECIALIZE instance Sized (FingerTree Buffer) #-}
    {-# SPECIALIZE instance Sized (FingerTree (Node a)) #-}
    sizeU Empty = 0
    sizeU (Single x) = sizeU x
    sizeU (Deep v _ _ _ _) = UNBOX(v)

instance Foldable FingerTree where
    foldr _ z Empty = z
    foldr f z (Single x) = x `f` z
    foldr f z (Deep _ _ pr m sf) =
        foldr f (foldr (flip (foldr f)) (foldr f z sf) m) pr

    foldl _ z Empty = z
    foldl f z (Single x) = z `f` x
    foldl f z (Deep _ _ pr m sf) =
        foldl f (foldl (foldl f) (foldl f z pr) m) sf

    foldr1 _ Empty = error "foldr1: empty sequence"
    foldr1 _ (Single x) = x
    foldr1 f (Deep _ _ pr m sf) =
        foldr f (foldr (flip (foldr f)) (foldr1 f sf) m) pr

    foldl1 _ Empty = error "foldl1: empty sequence"
    foldl1 _ (Single x) = x
    foldl1 f (Deep _ _ pr m sf) =
        foldl f (foldl (foldl f) (foldl1 f pr) m) sf

    foldMap f Empty = mempty
    foldMap f (Single x) = f x 
    foldMap f (Deep _ _ pr m sf) = foldMap f pr `mappend` foldMap (foldMap f) m `mappend` foldMap f sf

-- any and all are both commutative monoids, try to shortcut using digits first on both sides

{-# SPECIALIZE any :: (Buffer -> Bool) -> FingerTree Buffer -> Bool #-}
{-# SPECIALIZE any :: (Node a -> Bool) -> FingerTree (Node a) -> Bool #-}
any :: (a -> Bool) -> FingerTree a -> Bool
any _ Empty = False
any f (Single x) = f x
any f (Deep _ _ pr m sf) = anyDigit f pr || anyDigit f sf || any (any f) m where
    anyDigit f (One a) = f a
    anyDigit f (Two a b) = f a && f b
    anyDigit f (Three a b c) = f a && f b && f c
    anyDigit f (Four a b c d) = f a && f b && f c && f d

{-# SPECIALIZE all :: (Buffer -> Bool) -> FingerTree Buffer -> Bool #-}
{-# SPECIALIZE all :: (Node a -> Bool) -> FingerTree (Node a) -> Bool #-}
all :: (a -> Bool) -> FingerTree a -> Bool
all _ Empty = True
all f (Single x) = f x
all f (Deep _ _ pr m sf) = allDigit f pr && allDigit f sf && all (all f) m where
    allDigit f (One a) = f a
    allDigit f (Two a b) = f a && f b
    allDigit f (Three a b c) = f a && f b && f c
    allDigit f (Four a b c d) = f a && f b && f c && f d

-- | Like 'traverse', but with a more constrained type.
traverse' :: (Measured a1, Measured a2, Applicative f) =>
    (a1 -> f a2) -> FingerTree a1 -> f (FingerTree a2)
traverse' = traverseTree

traverseTree :: (Measured a2, Applicative f) =>
    (a1 -> f a2) -> FingerTree a1 -> f (FingerTree a2)
traverseTree _ Empty = pure Empty
traverseTree f (Single x) = Single <$> f x
traverseTree f (Deep _ _ pr m sf) =
    deep <$> traverseDigit f pr <*> traverseTree (traverseNode f) m <*> traverseDigit f sf

traverseNode :: (Measured a2, Applicative f) =>
    (a1 -> f a2) -> Node a1 -> f (Node a2)
traverseNode f (Node2 _ _ a b) = node2 <$> f a <*> f b
traverseNode f (Node3 _ _ a b c) = node3 <$> f a <*> f b <*> f c

traverseDigit :: (Applicative f) => (a -> f b) -> Digit a -> f (Digit b)
traverseDigit f (One a) = One <$> f a
traverseDigit f (Two a b) = Two <$> f a <*> f b
traverseDigit f (Three a b c) = Three <$> f a <*> f b <*> f c
traverseDigit f (Four a b c d) = Four <$> f a <*> f b <*> f c <*> f d

{-# INLINE deep #-}
{-# SPECIALIZE deep :: Digit Buffer -> FingerTree (Node Buffer) -> Digit Buffer -> FingerTree Buffer #-}
{-# SPECIALIZE deep :: Digit (Node a) -> FingerTree (Node (Node a)) -> Digit (Node a) -> FingerTree (Node a) #-}
deep :: Measured a => Digit a -> FingerTree (Node a) -> Digit a -> FingerTree a
deep pr m sf = Deep (spr + sm + ssf) (epr + em + esf) pr m sf
    where MEASURED(pr,m,sf)

-- Digits

data Digit a
    = One a
    | Two a a
    | Three a a a
    | Four a a a a
#if TESTING
    deriving Show
#endif

instance Foldable Digit where
    foldr f z (One a) = a `f` z
    foldr f z (Two a b) = a `f` (b `f` z)
    foldr f z (Three a b c) = a `f` (b `f` (c `f` z))
    foldr f z (Four a b c d) = a `f` (b `f` (c `f` (d `f` z)))

    foldl f z (One a) = z `f` a
    foldl f z (Two a b) = (z `f` a) `f` b
    foldl f z (Three a b c) = ((z `f` a) `f` b) `f` c
    foldl f z (Four a b c d) = (((z `f` a) `f` b) `f` c) `f` d

    foldr1 _ (One a) = a
    foldr1 f (Two a b) = a `f` b
    foldr1 f (Three a b c) = a `f` (b `f` c)
    foldr1 f (Four a b c d) = a `f` (b `f` (c `f` d))

    foldl1 _ (One a) = a
    foldl1 f (Two a b) = a `f` b
    foldl1 f (Three a b c) = (a `f` b) `f` c
    foldl1 f (Four a b c d) = ((a `f` b) `f` c) `f` d

    foldMap f (One a) = f a
    foldMap f (Two a b) = f a `mappend` f b
    foldMap f (Three a b c) = f a `mappend` f b `mappend` f c
    foldMap f (Four a b c d) = (f a `mappend` f b) `mappend` (f c `mappend` f d)
    
instance Functor Digit where
    fmap = fmapDefault

instance Traversable Digit where
    traverse f (One a) = One <$> f a
    traverse f (Two a b) = Two <$> f a <*> f b
    traverse f (Three a b c) = Three <$> f a <*> f b <*> f c
    traverse f (Four a b c d) = Four <$> f a <*> f b <*> f c <*> f d

instance Measured a => Measured (Digit a) where
    {-# SPECIALIZE instance Measured (Digit Buffer) #-}
    {-# SPECIALIZE instance Measured (Digit (Node a)) #-}
    measureU (One a) = measureU a
    measureU (Two a b) = PACK(sa + sb, ea + eb)
        where MEASURED(a,b)
    measureU (Two a b) = PACK(sa + sb + sc, ea + eb + ec)
        where MEASURED(a,b,c)
    measureU (Two a b) = PACK(sa + sb + sc + sd, ea + eb + ec + ed)
        where MEASURED(a,b,c,d)

instance Sized a => Sized (Digit a) where
    {-# SPECIALIZE instance Sized (Digit Buffer) #-}
    {-# SPECIALIZE instance Sized (Digit (Node a)) #-}
    sizeU (One a) = sizeU a
    sizeU (Two a b) = sizeU a PLUS sizeU b
    sizeU (Three a b c) = sizeU a PLUS sizeU b PLUS sizeU c
    sizeU (Four a b c d) = sizeU a PLUS sizeU b PLUS sizeU c PLUS sizeU d

{-# SPECIALIZE digitToTree :: Digit Buffer -> FingerTree Buffer #-}
{-# SPECIALIZE digitToTree :: Digit (Node a) -> FingerTree (Node a) #-}
digitToTree :: Measured a => Digit a -> FingerTree a
digitToTree (One a) = Single a
digitToTree (Two a b) = deep (One a) Empty (One b)
digitToTree (Three a b c) = deep (Two a b) Empty (One c)
digitToTree (Four a b c d) = deep (Two a b) Empty (Two c d)

-- Nodes

data Node a
    = Node2 {-# UNPACK #-} !Int {-# UNPACK #-} !Int a a
    | Node3 {-# UNPACK #-} !Int {-# UNPACK #-} !Int a a a
#if TESTING
    deriving Show
#endif

instance Foldable Node where
    foldr f z (Node2 _ _ a b) = a `f` (b `f` z)
    foldr f z (Node3 _ _ a b c) = a `f` (b `f` (c `f` z))

    foldl f z (Node2 _ _ a b) = (z `f` a) `f` b
    foldl f z (Node3 _ _ a b c) = ((z `f` a) `f` b) `f` c

    foldMap f (Node2 _ _ a b) = f a `mappend` f b
    foldMap f (Node2 _ _ a b c) = f a `mappend` f b `mappend` f c

instance Measured (Node a) where
    measureU (Node2 v e _ _) = PACK(v, e)
    measureU (Node3 v e _ _) = PACK(v, e)

instance Sized (Node a) where
    sizeU (Node2 v _ _ _)  = UNBOX(v)
    sizeU (Node3 v _ _ _ _) = UNBOX(v)

{-# INLINE node2 #-}
{-# SPECIALIZE node2 :: Elem a -> Elem a -> Node Buffer #-}
{-# SPECIALIZE node2 :: Node a -> Node a -> Node (Node a) #-}
node2     :: Measured a => a -> a -> Node a
node2 a b =  Node2 (sa + sb) (xa + xb) a b
    where MEASURED(a,b)

{-# INLINE node3 #-}
{-# SPECIALIZE node3 :: Elem a -> Elem a -> Elem a -> Node Buffer #-}
{-# SPECIALIZE node3 :: Node a -> Node a -> Node a -> Node (Node a) #-}
node3       :: Measured a => a -> a -> a -> Node a
node3 a b c =  Node3 (sa + sb + sc) (xa + xb + xc) a b c
    where MEASURED(a,b,c)

nodeToDigit :: Node a -> Digit a
nodeToDigit (Node2 _ _ a b) = Two a b
nodeToDigit (Node3 _ _ a b c) = Three a b c

-- Elements

instance Measured Buffer where
    measureU (PS _ _ l e) = PACK(l, e)

instance Sized Buffer where
    sizeU (PS _ _ l _) = UNBOX(l)

------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------

-- | /O(1)/. The empty sequence.
empty :: FingerTree a 
empty =  Empty

-- | /O(1)/. A singleton sequence.
singleton :: a -> FingerTree a
singleton =  Single

-- | /O(1)/. Add an element to the left end of a sequence.
-- Mnemonic: a triangle with the single element at the pointy end.
{-# SPECIALIZE cons :: Buffer -> FingerTree Buffer -> FingerTree Buffer #-}
{-# SPECIALIZE cons :: Node a -> FingerTree (Node a) -> FingerTree (Node a) #-}
cons    :: Measured a => a -> FingerTree a -> FingerTree a
cons a Empty = Single a
cons a (Single b) = deep (One a) Empty (One b)
cons a (Deep s x (Four b c d e) m sf) = m `seq` 
    Deep (sa + s) (xa + x) (Two a b) (node3 c d e `cons` m) sf
    where MEASURED(a)
cons a (Deep s x (Three b c d) m sf) = 
    Deep (sa + s) (xa + x) (Four a b c d) m sf
    where MEASURED(a)
cons a (Deep s x (Two b c) m sf) = 
    Deep (sa + s) (xa + x) (Three a b c) m sf
    where MEASURED(a)
cons a (Deep s x (One b) m sf) = 
    Deep (sa + s) (xa + x) (Two a b) m sf
    where MEASURED(a)
    
-- | /O(1)/. Add an element to the right end of a sequence.
-- Mnemonic: a triangle with the single element at the pointy end.
{-# SPECIALIZE snoc :: FingerTree Buffer -> Buffer -> FingerTree Buffer #-}
{-# SPECIALIZE snoc :: FingerTree (Node a) -> Node a -> FingerTree (Node a) #-}
snoc :: Measured a => FingerTree a -> a -> FingerTree a
snoc Empty a    =  Single a
snoc (Single a) b   =  deep (One a) Empty (One b)
snoc (Deep s x pr m (Four a b c d)) e = m `seq`
    Deep (s + se) (x + ee) pr (m `snoc` node3 a b c) (Two d e)
    where MEASUED(e)
snoc (Deep s x pr m (Three a b c)) d =
    Deep (s + sd) (x + xd) pr m (Four a b c d)
    where MEASURED(d)
snoc (Deep s x pr m (Two a b)) c =
    Deep (s + sc) (x + xc) pr m (Three a b c)
    where MEASURED(c)
snoc (Deep s x pr m (One a)) b =
    Deep (s + sb) (x + xb) pr m (Two a b)
    where MEASURED(b)

-- | /O(log(min(n1,n2)))/. Concatenate two sequences.
-- The appendTree/addDigits gunk below is machine generated
append :: FingerTree Buffer -> FingerTree Buffer -> FingerTree Buffer
append Empty xs =
    xs
append xs Empty =
    xs
append (Single x) xs =
    x `consTree` xs
append xs (Single x) =
    xs `snocTree` x
append (Deep s1 x1 pr1 m1 sf1) (Deep s2 x2 pr2 m2 sf2) =
    Deep (s1 + s2) (x1 + x2) pr1 (addDigits0 m1 sf1 pr2 m2) sf2

addDigits0 :: FingerTree (Node Buffer) -> Digit Buffer -> Digit Buffer -> FingerTree (Node Buffer) -> FingerTree (Node Buffer)
addDigits0 m1 (One a) (One b) m2 =
    appendTree1 m1 (node2 a b) m2
addDigits0 m1 (One a) (Two b c) m2 =
    appendTree1 m1 (node3 a b c) m2
addDigits0 m1 (One a) (Three b c d) m2 =
    appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits0 m1 (One a) (Four b c d e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits0 m1 (Two a b) (One c) m2 =
    appendTree1 m1 (node3 a b c) m2
addDigits0 m1 (Two a b) (Two c d) m2 =
    appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits0 m1 (Two a b) (Three c d e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits0 m1 (Two a b) (Four c d e f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits0 m1 (Three a b c) (One d) m2 =
    appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits0 m1 (Three a b c) (Two d e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits0 m1 (Three a b c) (Three d e f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits0 m1 (Three a b c) (Four d e f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits0 m1 (Four a b c d) (One e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits0 m1 (Four a b c d) (Two e f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits0 m1 (Four a b c d) (Three e f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits0 m1 (Four a b c d) (Four e f g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2

appendTree1 :: FingerTree (Node a) -> Node a -> FingerTree (Node a) -> FingerTree (Node a)
appendTree1 Empty a xs =
    a `consTree` xs
appendTree1 xs a Empty =
    xs `snocTree` a
appendTree1 (Single x) a xs =
    x `consTree` a `consTree` xs
appendTree1 xs a (Single x) =
    xs `snocTree` a `snocTree` x
appendTree1 (Deep s1 x1 pr1 m1 sf1) a (Deep s2 x2 pr2 m2 sf2) =
    Deep (s1 + sa + s2) (x1 + xa + x2) pr1 (addDigits1 m1 sf1 a pr2 m2) sf2
    where MEASURED(a)

addDigits1 :: FingerTree (Node (Node a)) -> Digit (Node a) -> Node a -> Digit (Node a) -> FingerTree (Node (Node a)) -> FingerTree (Node (Node a))
addDigits1 m1 (One a) b (One c) m2 =
    appendTree1 m1 (node3 a b c) m2
addDigits1 m1 (One a) b (Two c d) m2 =
    appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits1 m1 (One a) b (Three c d e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits1 m1 (One a) b (Four c d e f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Two a b) c (One d) m2 =
    appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits1 m1 (Two a b) c (Two d e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits1 m1 (Two a b) c (Three d e f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Two a b) c (Four d e f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits1 m1 (Three a b c) d (One e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits1 m1 (Three a b c) d (Two e f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Three a b c) d (Three e f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits1 m1 (Three a b c) d (Four e f g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits1 m1 (Four a b c d) e (One f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Four a b c d) e (Two f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits1 m1 (Four a b c d) e (Three f g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits1 m1 (Four a b c d) e (Four f g h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2

appendTree2 :: FingerTree (Node a) -> Node a -> Node a -> FingerTree (Node a) -> FingerTree (Node a)
appendTree2 Empty a b xs =
    a `consTree` b `consTree` xs
appendTree2 xs a b Empty =
    xs `snocTree` a `snocTree` b
appendTree2 (Single x) a b xs =
    x `consTree` a `consTree` b `consTree` xs
appendTree2 xs a b (Single x) =
    xs `snocTree` a `snocTree` b `snocTree` x
appendTree2 (Deep s1 e1 pr1 m1 sf1) a b (Deep s2 e2 pr2 m2 sf2) =
    Deep (s1 + sa + sb + s2) (e1 + ea + eb + e2) pr1 (addDigits2 m1 sf1 a b pr2 m2) sf2
    where MEASURED(a,b)

addDigits2 :: FingerTree (Node (Node a)) -> Digit (Node a) -> Node a -> Node a -> Digit (Node a) -> FingerTree (Node (Node a)) -> FingerTree (Node (Node a))
addDigits2 m1 (One a) b c (One d) m2 =
    appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits2 m1 (One a) b c (Two d e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits2 m1 (One a) b c (Three d e f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits2 m1 (One a) b c (Four d e f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits2 m1 (Two a b) c d (One e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits2 m1 (Two a b) c d (Two e f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits2 m1 (Two a b) c d (Three e f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits2 m1 (Two a b) c d (Four e f g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits2 m1 (Three a b c) d e (One f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits2 m1 (Three a b c) d e (Two f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits2 m1 (Three a b c) d e (Three f g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits2 m1 (Three a b c) d e (Four f g h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits2 m1 (Four a b c d) e f (One g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits2 m1 (Four a b c d) e f (Two g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits2 m1 (Four a b c d) e f (Three g h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits2 m1 (Four a b c d) e f (Four g h i j) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2

appendTree3 :: FingerTree (Node a) -> Node a -> Node a -> Node a -> FingerTree (Node a) -> FingerTree (Node a)
appendTree3 Empty a b c xs =
    a `consTree` b `consTree` c `consTree` xs
appendTree3 xs a b c Empty =
    xs `snocTree` a `snocTree` b `snocTree` c
appendTree3 (Single x) a b c xs =
    x `consTree` a `consTree` b `consTree` c `consTree` xs
appendTree3 xs a b c (Single x) =
    xs `snocTree` a `snocTree` b `snocTree` c `snocTree` x
appendTree3 (Deep s1 x1 pr1 m1 sf1) a b c (Deep s2 x2 pr2 m2 sf2) =
    Deep (s1 + sa + sb + sc + s2) (x1 + xa + xb + xc + x2) pr1 (addDigits3 m1 sf1 a b c pr2 m2) sf2
    where MEASURED(a,b,c)

addDigits3 :: FingerTree (Node (Node a)) -> Digit (Node a) -> Node a -> Node a -> Node a -> Digit (Node a) -> FingerTree (Node (Node a)) -> FingerTree (Node (Node a))
addDigits3 m1 (One a) b c d (One e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits3 m1 (One a) b c d (Two e f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits3 m1 (One a) b c d (Three e f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits3 m1 (One a) b c d (Four e f g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits3 m1 (Two a b) c d e (One f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits3 m1 (Two a b) c d e (Two f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits3 m1 (Two a b) c d e (Three f g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits3 m1 (Two a b) c d e (Four f g h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits3 m1 (Three a b c) d e f (One g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits3 m1 (Three a b c) d e f (Two g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits3 m1 (Three a b c) d e f (Three g h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits3 m1 (Three a b c) d e f (Four g h i j) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits3 m1 (Four a b c d) e f g (One h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits3 m1 (Four a b c d) e f g (Two h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits3 m1 (Four a b c d) e f g (Three h i j) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits3 m1 (Four a b c d) e f g (Four h i j k) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node2 j k) m2

appendTree4 :: FingerTree (Node a) -> Node a -> Node a -> Node a -> Node a -> FingerTree (Node a) -> FingerTree (Node a)
appendTree4 Empty a b c d xs =
    a `consTree` b `consTree` c `consTree` d `consTree` xs
appendTree4 xs a b c d Empty =
    xs `snocTree` a `snocTree` b `snocTree` c `snocTree` d
appendTree4 (Single x) a b c d xs =
    x `consTree` a `consTree` b `consTree` c `consTree` d `consTree` xs
appendTree4 xs a b c d (Single x) =
    xs `snocTree` a `snocTree` b `snocTree` c `snocTree` d `snocTree` x
appendTree4 (Deep s1 x1 pr1 m1 sf1) a b c d (Deep s2 x2 pr2 m2 sf2) =
    Deep (s1 + sa + sb + sc + sd + s2) (x1 + xa + xb + xc + xd + x2) pr1 (addDigits4 m1 sf1 a b c d pr2 m2) sf2
    where MEASURED(a,b,c,d)

addDigits4 :: FingerTree (Node (Node a)) -> Digit (Node a) -> Node a -> Node a -> Node a -> Node a -> Digit (Node a) -> FingerTree (Node (Node a)) -> FingerTree (Node (Node a))
addDigits4 m1 (One a) b c d e (One f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits4 m1 (One a) b c d e (Two f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits4 m1 (One a) b c d e (Three f g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits4 m1 (One a) b c d e (Four f g h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Two a b) c d e f (One g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits4 m1 (Two a b) c d e f (Two g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits4 m1 (Two a b) c d e f (Three g h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Two a b) c d e f (Four g h i j) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits4 m1 (Three a b c) d e f g (One h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits4 m1 (Three a b c) d e f g (Two h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Three a b c) d e f g (Three h i j) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits4 m1 (Three a b c) d e f g (Four h i j k) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node2 j k) m2
addDigits4 m1 (Four a b c d) e f g h (One i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Four a b c d) e f g h (Two i j) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits4 m1 (Four a b c d) e f g h (Three i j k) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node2 j k) m2
addDigits4 m1 (Four a b c d) e f g h (Four i j k l) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node3 j k l) m2

------------------------------------------------------------------------
-- Deconstruction
------------------------------------------------------------------------

-- | /O(1)/. Is this the empty sequence?
null       :: FingerTree a-> Bool
null Empty = True
null _     =  False

-- Views

head :: FingerTree a -> a
head Empty = errorEmptyList "head"
head (Single a) = a 
head (Deep _ _ pr _ _) = case pr of 
        One a -> a
        Two a _ -> a
        Three a _ _ -> a
        Four a _ _ _ -> a

last :: FingerTree a -> a
last Empty = errorEmptyList "last"
last (Single a) = a 
last (Deep _ _ _ _ sf) = case sf of 
        One a -> a
        Two _ b -> b
        Three _ _ c -> c
        Four _ _ _ d -> d

-- | /O(1)/. Analyse the left end of a sequence.
{-# SPECIALIZE viewl :: FingerTree Buffer -> ViewL (FingerTree Buffer) Buffer #-}
{-# SPECIALIZE viewl :: FingerTree (Node a) -> ViewL (FingerTree (Node a) (Node a) #-}
viewl   :: Measured a => FingerTree a -> ViewL (FingerTree a) a
viewl Empty             = EmptyL 
viewl (Single a)        = a :< Empty
viewl (Deep s e (One a) m sf) = a :< case viewl m of
    Nothing2    -> digitToTree sf
    b :< m'  -> Deep (s - sa) (e - ea) (nodeToDigit b) m' sf)
    where MEASURED(a)
viewl (Deep s e (Two a b) m sf) =
    a :< Deep (s - sa) (e - ea) (One b) m sf
    where MEASURED(a)
viewl (Deep s e (Three a b c) m sf) =
    a :< Deep (s - sa) (e - ea) (Two b c) m sf
    where MEASURED(a)
viewl (Deep s e (Four a b c d) m sf) =
    a :< Deep (s - sa) (e - ea) (Three b c d) m sf
    where MEASURED(a)

-- | /O(1)/. Analyse the right end of a sequence.
{-# SPECIALIZE viewrTree :: FingerTree Buffer -> ViewR (FingerTree Buffer) Buffer #-}
{-# SPECIALIZE viewrTree :: FingerTree (Node a) -> ViewR (FingerTree (Node a)) (Node a) #-}
viewr :: Measured a => FingerTree a -> ViewR (FingerTree a) a
viewr Empty = EmptyR
viewr (Single z) = Empty :> z
viewr (Deep s e pr m (One z)) = (case viewr m of
    EmptyR   -> digitToTree pr
    m' :> y  -> Deep (s - sz) (e - ez) pr m' (nodeToDigit y)) :> z
    where MEASURED(z)
viewr (Deep s e pr m (Two y z)) =
    Deep (s - sz) (e - ez) pr m (One y) :> z
    where MEASURED(z)
viewr (Deep s e pr m (Three x y z)) =
    Deep (s - sz) (e - ez) pr m (Two x y) :> z
    where MEASURED(z)
viewr (Deep s e pr m (Four w x y z)) =
    Deep (s - sz) (e - ez) pr m (Three w x y) :> z
    where MEASURED(z)

{-# SPECIALIZE deepL :: Maybe (Digit Buffer) -> FingerTree (Node Buffer) -> Digit Buffer -> FingerTree Buffer #-}
{-# SPECIALIZE deepL :: Maybe (Digit (Node a)) -> FingerTree (Node (Node a)) -> Digit (Node a) -> FingerTree (Node a) #-}
deepL :: Measured a => Maybe (Digit a) -> FingerTree (Node a) -> Digit a -> FingerTree a
deepL Nothing m sf  = case viewlTree m of
    EmptyL -> digitToTree sf
    a :< m' -> Deep (sm + ssf) (em + esf) (nodeToDigit a) m' sf
    where MEASURED(m,sf)
deepL (Just pr) m sf = deep pr m sf

{-# SPECIALIZE deepR :: Digit Buffer -> FingerTree (Node Buffer) -> Maybe (Digit Buffer) -> FingerTree Buffer #-}
{-# SPECIALIZE deepR :: Digit (Node a) -> FingerTree (Node (Node a)) -> Maybe (Digit (Node a)) -> FingerTree (Node a) #-}
deepR :: Measured a => Digit a -> FingerTree (Node a) -> Maybe (Digit a) -> FingerTree a
deepR pr m Nothing  = case viewrTree m of
    EmptyR -> digitToTree pr
    m' :< a -> Deep (spr + sm) (epr + em) pr m' (nodeToDigit a)
    where MEASURED(pr,m)
deepR pr m (Just sf) = deep pr m sf

------------------------------------------------------------------------
-- Lists
------------------------------------------------------------------------

-- | /O(n)/. Create a sequence from a finite list of elements.
-- There is a function 'toList' in the opposite direction for all
-- instances of the 'Foldable' class, including 'Rope'.
fromList :: Measured a => [a] -> FingerTree a
fromList = foldr cons empty

#if TESTING

------------------------------------------------------------------------
-- QuickCheck
------------------------------------------------------------------------

instance (Arbitrary a, Measured a) => Arbitrary (FingerTree a) where
    arbitrary = liftM Roperbitrary
    coarbitrary (Rope x) = coarbitrary x

instance Arbitrary a => Arbitrary Buffer where
    arbitrary = liftM Elem arbitrary
    coarbitrary (Elem x) = coarbitrary x

instance (Arbitrary a, Measured a) => Arbitrary (FingerTree a) where
    arbitrary = sized arb
      where arb :: (Arbitrary a, Measured a) => Int -> Gen (FingerTree a)
        arb 0 = return Empty
        arb 1 = liftM Single arbitrary
        arb n = liftM3 deep arbitrary (arb (n `div` 2)) arbitrary

    coarbitrary Empty = variant 0
    coarbitrary (Single x) = variant 1 . coarbitrary x
    coarbitrary (Deep _ _ pr m sf) =
        variant 2 . coarbitrary pr . coarbitrary m . coarbitrary sf

instance (Arbitrary a, Measured a) => Arbitrary (Node a) where
    arbitrary = oneof [
            liftM2 node2 arbitrary arbitrary,
            liftM3 node3 arbitrary arbitrary arbitrary]

    coarbitrary (Node2 _ _ a b) = variant 0 . coarbitrary a . coarbitrary b
    coarbitrary (Node3 _ _ a b c) =
        variant 1 . coarbitrary a . coarbitrary b . coarbitrary c

instance Arbitrary a => Arbitrary (Digit a) where
    arbitrary = oneof [
            liftM One arbitrary,
            liftM2 Two arbitrary arbitrary,
            liftM3 Three arbitrary arbitrary arbitrary,
            liftM4 Four arbitrary arbitrary arbitrary arbitrary]

    coarbitrary (One a) = variant 0 . coarbitrary a
    coarbitrary (Two a b) = variant 1 . coarbitrary a . coarbitrary b
    coarbitrary (Three a b c) =
        variant 2 . coarbitrary a . coarbitrary b . coarbitrary c
    coarbitrary (Four a b c d) =
        variant 3 . coarbitrary a . coarbitrary b . coarbitrary c . coarbitrary d
#endif

------------------------------------------------------------------------
-- Valid trees
------------------------------------------------------------------------

instance Valid Buffer where
    valid _ = True

instance (Measured a, Valid a) => Valid (FingerTree a) where
    valid Empty = True
    valid (Single x) = valid x
    valid (Deep s e pr m sf) =
        s == spr + sm + ssf && e == epr + em + esf && e >= 0 && e <= s && valid pr && valid m && valid sf
        where MEASURED(pr,m,sf)

instance (Measured a, Valid a) => Valid (Node a) where
    valid (Node2 s e a b) = s == s == sa + sb && e == ea + eb && e >= 0 && e <= s && valid a && valid b
        where MEASURED(a,b)
    valid (Node3 s e a b c) = s == sa+ sb + sc && e == ea + eb + ec && e >= 0 && e <= s && valid a && valid b && valid c

instance Valid a => Valid (Digit a) where
    valid (One a) = valid a
    valid (Two a b) = valid a && valid b
    valid (Three a b c) = valid a && valid b && valid c
    valid (Four a b c d) = valid a && valid b && valid c && valid d

