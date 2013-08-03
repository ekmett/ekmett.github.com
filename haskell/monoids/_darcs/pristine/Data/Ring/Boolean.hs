{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Ring.Boolean
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (MPTCs)
--
-- A Boolean 'Ring' over any Bits instance. Note well that the 'mappend' of this ring is xor.
-- You should use use 'Ord' from "Data.Ring.Semi.Ord.Order" on 'Bool' to get the '&&'/'||'-based 
-- distributive-lattice 'SemiRing'.
--
-- Also note that @gnegate = id@ in a Boolean Ring!
-----------------------------------------------------------------------------

module Data.Ring.Boolean
    ( module Data.Ring
    , Boolean(Boolean, getBoolean)
    ) where

import Data.Bits
import Data.Ring
import Data.Ring.Module
import Data.Ring.Semi.Natural
import Data.Monoid.Reducer
import Test.QuickCheck hiding ((.&.))

newtype Boolean a = Boolean { getBoolean :: a } deriving (Eq,Ord,Show,Read,Arbitrary,CoArbitrary)

-- | @xor@
instance Bits a => Monoid (Boolean a) where
    mempty = Boolean 0  
    Boolean a `mappend` Boolean b = Boolean ((a .|. b) .&. complement (a .&. b))

-- | @id@, since @x `xor` x = zero@
instance Bits a => Group (Boolean a) where
    gnegate = Boolean . id . getBoolean

-- | @and@
instance Bits a => Multiplicative (Boolean a) where
    one = Boolean (complement 0)
    Boolean a `times` Boolean b = Boolean (a .&. b)

-- | the boolean ring (using symmetric difference as addition) is a ring
instance Bits a => Ringoid (Boolean a)
instance Bits a => LeftSemiNearRing (Boolean a)
instance Bits a => RightSemiNearRing (Boolean a)
instance Bits a => SemiRing (Boolean a)
instance Bits a => Ring (Boolean a)

-- | it reduces boolean values
instance Bits a => Reducer a (Boolean a) where
    unit = Boolean

-- | every monoid is a module over the naturals, boolring is idempotent
instance Bits a => Module Natural (Boolean a)
instance Bits a => LeftModule Natural (Boolean a) where
    0 *. _ = mempty
    _ *. m = m
instance Bits a => RightModule Natural (Boolean a) where
    _ .* 0 = mempty
    m .* _ = m
instance Bits a => Bimodule Natural (Boolean a)

-- | every group is a module over the integers, boolring is idempotent
instance Bits a => Module Integer (Boolean a)
instance Bits a => LeftModule Integer (Boolean a) where
    0 *. _ = mempty
    _ *. m = m
instance Bits a => RightModule Integer (Boolean a) where
    _ .* 0 = mempty
    m .* _ = m
instance Bits a => Bimodule Integer (Boolean a)

-- | every ring is a module over itself
instance Bits a => Module (Boolean a) (Boolean a)
instance Bits a => LeftModule (Boolean a) (Boolean a) where 
    (*.) = times
instance Bits a => RightModule (Boolean a) (Boolean a) where 
    (.*) = times
instance Bits a => Bimodule (Boolean a) (Boolean a)
instance Bits a => Normed (Boolean a) (Boolean a) where mabs = id
