{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
---- |
---- Module      :  Data.Ring.Semi.Tropical
---- Copyright   :  (c) Edward Kmett 2009
---- License     :  BSD-style
---- Maintainer  :  ekmett@gmail.com
---- Stability   :  experimental
---- Portability :  portable
----
-----------------------------------------------------------------------------

module Data.Ring.Semi.Tropical
    ( module Data.Monoid.Reducer
    , module Data.Ring
    -- * Tropical Semirings
    , infinity
    , Tropical(Tropical,getTropical)
    ) where

import Control.Functor.Pointed
import Data.Monoid.Reducer
import Data.Monoid.Combinators as Monoid
import Data.Ring.Semi.Natural
import Data.Ring
import Data.Ring.Module
import Data.Monoid.Ord hiding (infinity)

#ifdef M_QUICKCHECK
import Test.QuickCheck
#endif

infinity :: Tropical a
infinity = Tropical Nothing

-- | The 'SemiRing' @('min','+')@ over @'a' extended with 'infinity'@.
--   When @a@ has a Num instance with an addition that respects order, then this is 
--   transformed into a tropical semiring. It is assumed that 0 is the least element
--   of a.
--
--   <http://hal.archives-ouvertes.fr/docs/00/11/37/79/PDF/Tropical.pdf>

newtype Tropical a = Tropical { getTropical :: Maybe a } deriving 
    ( Eq
    , Show
    , Read
#ifdef M_QUICKCHECK
    , Arbitrary
    , CoArbitrary
#endif
    )

instance Ord a => Ord (Tropical a) where
    Tropical Nothing  `compare` Tropical Nothing  = EQ
    Tropical Nothing  `compare` _                 = GT
    _                 `compare` Tropical Nothing  = LT
    Tropical (Just a) `compare` Tropical (Just b) = a `compare` b

instance Ord a => Monoid (Tropical a) where
    mempty = infinity
    mappend = min

instance Ord a => Reducer a (Tropical a) where
    unit = Tropical . Just

instance Ord a => Reducer (Maybe a) (Tropical a) where
    unit = Tropical

instance Ord a => Reducer (MinPriority a) (Tropical a) where
    unit = Tropical . getMinPriority

instance Functor Tropical where
    fmap f (Tropical a) = Tropical (fmap f a)

instance Pointed Tropical where
    point = Tropical . Just

instance Num a => Multiplicative (Tropical a) where
    one = point $ fromInteger 0
    Tropical Nothing `times` _       = infinity
    Tropical (Just a) `times` Tropical (Just b) = point (a + b)
    _  `times` Tropical Nothing      = infinity

instance (Ord a, Num a) => Ringoid (Tropical a)
instance (Ord a, Num a) => LeftSemiNearRing (Tropical a)
instance (Ord a, Num a) => RightSemiNearRing (Tropical a)
instance (Ord a, Num a) => SemiRing (Tropical a)

instance (Ord a, Num a) => Module (Tropical a) (Tropical a)
instance (Ord a, Num a) => LeftModule (Tropical a) (Tropical a) where (*.) = times
instance (Ord a, Num a) => RightModule (Tropical a) (Tropical a) where (.*) = times
instance (Ord a, Num a) => Bimodule (Tropical a) (Tropical a)

instance (Ord a, Num a) => Module Natural (Tropical a)
instance (Ord a, Num a) => LeftModule Natural (Tropical a) where (*.) = flip Monoid.replicate
instance (Ord a, Num a) => RightModule Natural (Tropical a) where (.*) = Monoid.replicate
instance (Ord a, Num a) => Bimodule Natural (Tropical a)
