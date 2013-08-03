{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.Ring.Semi.Ord
-- Copyright   :  (c) Edward Kmett 2009, Conal Elliott 2008
-- License     :  BSD3
-- 
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- 
-- Turn an instance of 'Ord' into a 'SemiRing' over 'max' and 'min'
------------------------------------------------------------------------

module Data.Ring.Semi.Ord
    ( module Data.Ring
    , Order(Order,getOrder)
    , Priority(MinBound,Priority,MaxBound)
    ) where

-- import Control.Applicative
import Control.Functor.Pointed
import Data.Ring
import Data.Monoid.Ord
import Data.Monoid.Reducer

#ifdef M_QUICKCHECK
import Test.QuickCheck
#endif

-- | A 'SemiRing' using a type's built-in Bounded instance.
newtype Order a = Order { getOrder :: a } deriving 
    ( Eq
    , Ord
    , Read
    , Show
    , Bounded
#ifdef M_QUICKCHECK
    , Arbitrary
    , CoArbitrary
#endif
    )

instance (Bounded a, Ord a) => Monoid (Order a) where
    mappend = max
    mempty = minBound

instance (Bounded a, Ord a) => Multiplicative (Order a) where
    times = min
    one = maxBound
    
instance (Bounded a, Ord a) => Ringoid (Order a)
instance (Bounded a, Ord a) => RightSemiNearRing (Order a)
instance (Bounded a, Ord a) => LeftSemiNearRing (Order a)
instance (Bounded a, Ord a) => SemiRing (Order a)
instance (Bounded a, Ord a) => Reducer a (Order a) where
    unit = Order

instance Functor Order where
    fmap f (Order a) = Order (f a)

instance Pointed Order where
    point = Order

instance Copointed Order where
    extract = getOrder

-- | A 'SemiRing' which adds 'minBound' and 'maxBound' to a pre-existing type.
data Priority a = MinBound | Priority a | MaxBound deriving (Eq,Read,Show)

instance Bounded (Priority a) where
    minBound = MinBound
    maxBound = MaxBound

instance Ord a => Ord (Priority a) where
  MinBound   <= _         = True
  Priority _ <= MinBound  = False
  Priority a <= Priority b = a <= b
  Priority _ <= MaxBound  = True
  MaxBound   <= MaxBound  = True
  MaxBound   <= _         = False

  MinBound   `min` _          = MinBound
  _          `min` MinBound   = MinBound
  Priority a `min` Priority b = Priority (a `min` b)
  u          `min` MaxBound   = u
  MaxBound   `min` v          = v
  
  MinBound   `max` v          = v
  u          `max` MinBound   = u
  Priority a `max` Priority b = Priority (a `max` b)
  _          `max` MaxBound   = MaxBound
  MaxBound   `max` _          = MaxBound

#ifdef M_QUICKCHECK
instance Arbitrary a => Arbitrary (Priority a) where
  arbitrary = frequency [ (1 ,return MinBound)
                        , (10, fmap Priority arbitrary)
                        , (1 ,return MaxBound) ]
  shrink (Priority x) = MinBound : MaxBound : fmap Priority (shrink x)
  shrink MinBound = []
  shrink MaxBound = []

instance CoArbitrary a => CoArbitrary (Priority a) where
  coarbitrary MinBound     = variant (0 :: Int)
  coarbitrary (Priority a) = variant (1 :: Int) . coarbitrary a
  coarbitrary MaxBound     = variant (2 :: Int)
#endif

instance Ord a => Monoid (Priority a) where
    mappend = max
    mempty = minBound

instance Ord a => Multiplicative (Priority a) where
    times = min
    one = maxBound

instance Ord a => Ringoid (Priority a)
instance Ord a => LeftSemiNearRing (Priority a)
instance Ord a => RightSemiNearRing (Priority a)
instance Ord a => SemiRing (Priority a)

instance Ord a => Reducer a (Priority a) where
    unit = Priority

instance Ord a => Reducer (MinPriority a) (Priority a) where
    unit (MinPriority Nothing)  = MaxBound
    unit (MinPriority (Just x)) = Priority x

instance Ord a => Reducer (MaxPriority a) (Priority a) where
    unit (MaxPriority Nothing)  = MinBound
    unit (MaxPriority (Just x)) = Priority x

instance Functor Priority where
    fmap _ MaxBound = MaxBound
    fmap f (Priority a) = Priority (f a)
    fmap _ MinBound = MinBound

instance Pointed Priority where
    point = Priority
