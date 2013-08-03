{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Ring.FromNum
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (MPTCs)
--
-- A wrapper that lies for you and claims any instance of 'Num' is a 'Ring'.
-- Who knows, for your type it might even be telling the truth!
--
-----------------------------------------------------------------------------

module Data.Ring.FromNum 
    ( module Data.Ring
    , FromNum(FromNum, getFromNum)
    ) where

import Data.Ring
import Data.Monoid.Reducer
import Test.QuickCheck

newtype FromNum a = FromNum { getFromNum :: a } deriving (Eq,Show,Num,Arbitrary,CoArbitrary)

instance Num a => Monoid (FromNum a) where
    mempty = fromInteger 0
    mappend = (+)

instance Num a => Group (FromNum a) where
    minus = (-)
    gnegate = negate
    
instance Num a => Multiplicative (FromNum a) where
    one = fromInteger 1
    times = (*)

-- you can assume these, but you're probably lying to yourself
instance Num a => Ringoid (FromNum a)
instance Num a => LeftSemiNearRing (FromNum a)
instance Num a => RightSemiNearRing (FromNum a)
instance Num a => SemiRing (FromNum a)
instance Num a => Ring (FromNum a)
    
instance Num a => Reducer Integer (FromNum a) where
    unit = fromInteger

