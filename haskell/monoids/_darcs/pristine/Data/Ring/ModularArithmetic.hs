{-# LANGUAGE RankNTypes, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, EmptyDataDecls, FunctionalDependencies, TypeOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Ring.ModularArithmetic
-- Copyright   :  Edward Kmett 2009, Oleg Kiselyov and Chung-chieh Shan 2004
--                  
-- License     :  BSD-style
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (MPTCs, scoped types, empty decls, type operators)
--
-----------------------------------------------------------------------------

module Data.Ring.ModularArithmetic
    ( module Data.Ring
    , Mod(getMod), Modular, modulus
    , withIntegralModulus
    ) where

import Data.Ring
import Data.Reflection

newtype (a `Mod` s) = M { getMod :: a } 
    deriving (Eq,Show)

class Modular s a | s -> a where
    modulus :: s -> a

normalize :: (Modular s a, Integral a) => a -> (a `Mod` s)
normalize = normalize' undefined where
    normalize' :: (Modular s a, Integral a) => s -> a -> (a `Mod` s)
    normalize' s a = M (a `mod` modulus s)

data ModulusNum s a

instance (ReflectedNum s, Num a) => Modular (ModulusNum s a) a where
    modulus _ = reflectNum (undefined :: s)

withIntegralModulus :: Integral a => a -> (forall s. Modular s a => w `Mod` s) -> w
withIntegralModulus = withIntegralModulus' undefined where
    withIntegralModulus' :: Integral a => w -> a -> (forall s. Modular s a => w `Mod` s) -> w
    withIntegralModulus' (_ :: w) (i :: a) k = 
        reifyIntegral i (\(_ :: t) -> 
        getMod (k :: w `Mod` ModulusNum t a))

instance (Modular s a, Integral a) => Num (a `Mod` s) where
    M a + M b = normalize (a + b)
    M a - M b = normalize (a - b)
    M a * M b = normalize (a * b)
    negate (M a)  = normalize (negate a)
    fromInteger i = normalize (fromInteger i)
    signum = error "broken numerical type tower"
    abs    = error "broken numerical type tower"

instance (Modular s a, Integral a) => Monoid (a `Mod` s) where
    mempty = 0
    mappend = (+)

instance (Modular s a, Integral a) => Multiplicative (a `Mod` s) where
    one = 1
    times = (*)

instance (Modular s a, Integral a) => Group (a `Mod` s) where
    gnegate = negate
    minus = (-)
    gsubtract = subtract

instance (Modular s a, Integral a) => Ringoid (a `Mod` s)
instance (Modular s a, Integral a) => LeftSemiNearRing (a `Mod` s)
instance (Modular s a, Integral a) => RightSemiNearRing (a `Mod` s)
instance (Modular s a, Integral a) => SemiRing (a `Mod` s)
instance (Modular s a, Integral a) => Ring (a `Mod` s)
