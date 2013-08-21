{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RankNTypes, FunctionalDependencies, UndecidableInstances, FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Ring.Module.AutomaticDifferentiation
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  portable (instances use MPTCs)
--
-----------------------------------------------------------------------------

module Data.Ring.Module.AutomaticDifferentiation 
    ( module Data.Ring.Module
    , D
    , d
    , lift
    ) where

import Prelude
import Data.Ring.Module
import Data.Monoid.Reducer
import Test.QuickCheck
import Control.Monad

data D s r m = D r m deriving (Show,Read)

lift :: (r `Bimodule` m) => r -> D s r m
lift x = D x zero

infinitesimal :: (r `Bimodule` m, Ringoid m) => D s r m
infinitesimal = D zero one

instance Eq r => Eq (D s r m) where
    D x _ == D y _ = x == y

instance Ord r => Ord (D s r m) where
    D x _ `compare` D y _ = compare x y

instance (r `Bimodule` m) => Monoid (D s r m) where
    mempty = D mempty mempty
    D x m `mappend` D y n = D (x `mappend` y) (m `mappend` n)

instance (r `Bimodule` m) => Multiplicative (D s r m) where
    one = D one zero
    D x m `times` D y n = D (x `times` y) (x *. n `plus` m .* y)

instance (Group r, r `Bimodule` m, Group m) => Group (D s r m) where
    gnegate (D x m) = D (gnegate x) (gnegate m)
    D x m `minus` D y n = D (x `minus` y) (m `minus` n)
    D x m `gsubtract` D y n = D (x `gsubtract` y) (m `gsubtract` n)

instance Num a => Num (D s a a) where
    D x x' + D y y' = D (x + y) (x' + y')
    D x x' * D y y' = D (x * y) (x * y' + x' * y)
    D x x' - D y y' = D (x - y) (x' - y')
    negate (D x x') = D (negate x) (negate x')
    abs (D x x') = D (abs x) (signum x * x')
    signum (D x _) = D (signum x) 0
    fromInteger x = D (fromInteger x) 0

instance Fractional a => Fractional (D s a a) where
    recip (D x x') = D (recip x) (-x'/x/x)
    fromRational x = D (fromRational x) 0

instance (Ringoid r, r `Bimodule` m) => Ringoid (D s r m)
instance (LeftSemiNearRing r, Bimodule r m) => LeftSemiNearRing (D s r m)
instance (RightSemiNearRing r, Bimodule r m) => RightSemiNearRing (D s r m)
instance (SemiRing r, r `Bimodule` m) => SemiRing (D s r m)
instance (Ring r, r `Bimodule` m, Group m) => Ring (D s r m)

instance (r `Bimodule` m, c `Reducer` r, c `Reducer` m) => Reducer c (D s r m) where
    unit c = D (unit c) (unit c)
    c `cons` D x m = D (c `cons` x) (c `cons` m)
    D x m `snoc` c = D (x `snoc` c) (m `snoc` c)

instance (Arbitrary r, Arbitrary m) => Arbitrary (D s r m) where
    arbitrary = liftM2 D arbitrary arbitrary
    shrink (D r m) = liftM2 D (shrink r) (shrink m)

instance (CoArbitrary r, CoArbitrary m) => CoArbitrary (D s r m) where
    coarbitrary (D r m) = coarbitrary r >< coarbitrary m

d :: (r `Bimodule` m, Ringoid m) => (forall s. D s r m -> D s r m) -> (r,m)
d f = (y,y') where D y y' = f infinitesimal

