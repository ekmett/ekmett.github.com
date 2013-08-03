{-# LANGUAGE UndecidableInstances, TypeOperators, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Ring.Semi.Natural
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (type families, MPTCs)
--
-- Monoids for non-negative integers ('Natural') and ints ('Nat')
--
-- The naturals form a module over any of our monoids.
-----------------------------------------------------------------------------

module Data.Ring.Semi.Natural
    ( module Data.Ring
    , Natural
    , toNatural
    , fromNatural
    ) where

import Prelude hiding (id,(.))
import Numeric (readDec, showInt)
import Control.Applicative
import Control.Monad
import Data.Ring
import qualified Data.Monoid.Combinators as Monoid
-- import Data.Word
import Data.Monoid.Monad
import Data.Monoid.Applicative
import Data.Monoid.Multiplicative
import Data.Monoid.Categorical
import Data.Monoid.Self
import Data.Monoid.Lexical.SourcePosition
import Data.Monoid.Lexical.UTF8.Decoder
import Data.Generator.Free

#ifdef M_CONTAINERS
-- used with Seq
import Data.Generator.Compressive.RLE
import Data.Sequence (Seq)
#endif

#ifdef X_OverloadedStrings
import Data.Monoid.FromString
#endif

toNatural :: Integer -> Natural
toNatural = fromInteger

fromNatural :: Ringoid r => Natural -> r
fromNatural = Monoid.replicate one . getNatural

newtype Natural = Natural { getNatural :: Integer } 
    deriving (Eq,Ord)

instance Read Natural where
    readsPrec = const readDec

instance Show Natural where
    showsPrec = const showInt

instance Num Natural where
    Natural a + Natural b = Natural (a + b)
    Natural a - Natural b = fromInteger (a - b) 
    Natural a * Natural b = Natural (a * b)
    abs = id
    signum = Natural . signum . getNatural
    fromInteger x | x < 0     = error "Natural < 0"
                  | otherwise = Natural x
    negate 0 = 0 
    negate _ = error "Natural < 0"

instance Enum Natural where
    succ (Natural n) = Natural (n + 1)
    pred (Natural 0) = error "Natural < 0"
    pred (Natural n) = Natural (n - 1)
    toEnum n | n < 0 = error "Natural < 0"
    toEnum n = Natural (fromIntegral n)
    fromEnum = fromIntegral
    enumFrom (Natural n) = Natural `map` enumFrom n
    enumFromThen (Natural n) (Natural np) 
        | np < n = Natural `map` enumFromThenTo n np 0
        | otherwise = Natural `map` enumFromThen n np
    enumFromTo (Natural n) (Natural m) = Natural `map` enumFromTo n m
    enumFromThenTo (Natural n) (Natural m) (Natural o) = Natural `map` enumFromThenTo n m o

instance Real Natural where
    toRational = toRational . getNatural

instance Integral Natural where
    toInteger = getNatural
    Natural a `quot` Natural b = Natural (a `quot` b)
    Natural a `rem` Natural b = Natural (a `rem` b)
    Natural a `div` Natural b = Natural (a `div` b)
    Natural a `mod` Natural b = Natural (a `mod` b)
    Natural a `quotRem` Natural b = (Natural q,Natural r) where ~(q,r) = a `quotRem` b
    Natural a `divMod` Natural b = (Natural q,Natural r) where ~(q,r) = a `divMod` b

instance Monoid Natural where
    mempty = 0
    mappend = (+)

instance Multiplicative Natural where
    one = 1
    times = (*)

instance Ringoid Natural
instance LeftSemiNearRing Natural
instance RightSemiNearRing Natural
instance SemiRing Natural

instance LeftModule Natural () where _ *. _ = ()
instance RightModule Natural () where _ .* _ = ()
instance Module Natural ()

-- idempotent monoids
instance LeftModule Natural Any where 
    0 *. _ = mempty
    _ *. m = m
instance RightModule Natural Any where 
    _ .* 0 = mempty
    m .* _ = m 
instance Module Natural Any 

instance LeftModule Natural All where 
    0 *. _ = mempty
    _ *. m = m
instance RightModule Natural All where 
    _ .* 0 = mempty
    m .* _ = m
instance Module Natural All

instance LeftModule Natural (First a) where 
    0 *. _ = mempty
    _ *. m = m
instance RightModule Natural (First a) where 
    _ .* 0 = mempty
    m .* _ = m
instance Module Natural (First a) 

instance LeftModule Natural (Last a) where 
    0 *. _ = mempty
    _ *. m = m
instance RightModule Natural (Last a) where 
    _ .* 0 = mempty
    m .* _ = m
instance Module Natural (Last a)

instance LeftModule Natural Ordering where 
    0 *. _ = mempty
    _ *. m = m
instance RightModule Natural Ordering where 
    _ .* 0 = mempty
    m .* _ = m 
instance Module Natural Ordering

-- other monoids

instance LeftModule Natural [a] where (*.) = flip Monoid.replicate
instance RightModule Natural [a] where (.*) = Monoid.replicate
instance Module Natural [a]

instance Monoid m => LeftModule Natural (a -> m) where (*.) = flip Monoid.replicate
instance Monoid m => RightModule Natural (a -> m) where (.*) = Monoid.replicate
instance Monoid m => Module Natural (a -> m)

instance Num a => LeftModule Natural  (Sum a) where (*.) = flip Monoid.replicate
instance Num a => RightModule Natural (Sum a) where (.*) = Monoid.replicate
instance Num a => Module Natural (Sum a)

instance Num a => LeftModule Natural  (Product a) where (*.) = flip (.*)
instance Num a => RightModule Natural (Product a) where Product m .* Natural n = Product (m ^ n)
instance Num a => Module Natural (Product a)

instance LeftModule Natural  (Endo a) where (*.) = flip Monoid.replicate
instance RightModule Natural (Endo a) where (.*) = Monoid.replicate
instance Module Natural (Endo a)

instance Monoid m => LeftModule  Natural (Dual m) where (*.) = flip Monoid.replicate
instance Monoid m => RightModule Natural (Dual m) where (.*) = Monoid.replicate
instance Monoid m => Module Natural (Dual m)

-- Self
instance Monoid m => LeftModule  Natural (Self m) where (*.) = flip Monoid.replicate
instance Monoid m => RightModule Natural (Self m) where (.*) = Monoid.replicate
instance Monoid m => Module Natural (Self m)

-- Free Generator
instance LeftModule  Natural (Free a) where (*.) = flip Monoid.replicate
instance RightModule Natural (Free a) where (.*) = Monoid.replicate
instance Module Natural (Free a)

-- Categorical
instance Category k => LeftModule Natural  (GEndo k a) where (*.) = flip Monoid.replicate
instance Category k => RightModule Natural (GEndo k a) where (.*) = Monoid.replicate
instance Category k => Module Natural (GEndo k a)

instance Monoid m => LeftModule Natural  (CMonoid m m m) where (*.) = flip Monoid.replicate
instance Monoid m => RightModule Natural (CMonoid m m m) where (.*) = Monoid.replicate
instance Monoid m => Module Natural (CMonoid m m m)

-- Alternative
instance Applicative f => LeftModule Natural  (Traversal f) where (*.) = flip Monoid.replicate
instance Applicative f => RightModule Natural (Traversal f) where (.*) = Monoid.replicate
instance Applicative f => Module Natural (Traversal f) 

instance Alternative f => LeftModule Natural  (Alt f a) where (*.) = flip Monoid.replicate
instance Alternative f => RightModule Natural (Alt f a) where (.*) = Monoid.replicate
instance Alternative f => Module Natural (Alt f a) 

--instance (Alternative f, Monoid m) => LeftModule Natural  (App f m) where (*.) = flip Monoid.replicate
--instance (Alternative f, Monoid m) => RightModule Natural (App f m) where (.*) = Monoid.replicate
--instance (Alternative f, Monoid m) => Module Natural (App f m)  

-- Monad
instance Monad f => LeftModule Natural  (Action f) where (*.) = flip Monoid.replicate
instance Monad f => RightModule Natural (Action f) where (.*) = Monoid.replicate
instance Monad f => Module Natural (Action f) 

instance MonadPlus f => LeftModule Natural  (MonadSum f a) where (*.) = flip Monoid.replicate
instance MonadPlus f => RightModule Natural (MonadSum f a) where (.*) = Monoid.replicate
instance MonadPlus f => Module Natural (MonadSum f a) 

--instance (MonadPlus f, Monoid m) => LeftModule Natural  (Mon f m) where (*.) = flip Monoid.replicate
--instance (MonadPlus f, Monoid m)  => RightModule Natural (Mon f m) where (.*) = Monoid.replicate
--instance (MonadPlus f, Monoid m) => Module Natural (Mon f m)  

-- Lexical 
instance LeftModule Natural  (SourcePosition f) where 
    0 *. _ = mempty
    n *. Columns x = Columns (fromIntegral n * x) 
    n *. Lines l c = Lines (fromIntegral n * l) c
    _ *. Pos f l c = Pos f l c 
    n *. t = Monoid.replicate t n 

instance RightModule Natural (SourcePosition f) where (.*) = flip (*.)
instance Module Natural (SourcePosition f) 

instance CharReducer m => LeftModule Natural  (UTF8 m) where (*.) = flip Monoid.replicate
instance CharReducer m => RightModule Natural (UTF8 m) where (.*) = Monoid.replicate
instance CharReducer m => Module Natural (UTF8 m) 

instance Multiplicative m => LeftModule Natural (Log m) where (*.) = flip Monoid.replicate
instance Multiplicative m => RightModule Natural (Log m) where (.*) = Monoid.replicate
instance Multiplicative m => Module Natural (Log m) 

#ifdef M_CONTAINERS
-- RLE Seq
instance Eq a => LeftModule  Natural (RLE Seq a) where (*.) = flip Monoid.replicate
instance Eq a => RightModule Natural (RLE Seq a) where (.*) = Monoid.replicate
instance Eq a => Module Natural (RLE Seq a)
#endif

#ifdef X_OverloadedStrings
-- FromString
instance Monoid m => LeftModule  Natural (FromString m) where (*.) = flip Monoid.replicate
instance Monoid m => RightModule Natural (FromString m) where (.*) = Monoid.replicate
instance Monoid m => Module Natural (FromString m)
#endif

-- TODO
--
-- Control.Monad.*
-- ParsecT
-- FingerTree
-- Int, Integer, Ratio
-- SourcePosition
-- Replace Natural here with some other notion of NonNegative a 
-- Words, Lines, Unspaced, Unlined
-- Union/UnionWith, Map, Set, etc.
-- Max, Min, MaxPriority, MinPriority idempotent
-- BoolRing
-- Seq
