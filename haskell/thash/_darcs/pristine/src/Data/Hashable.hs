-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Hashable
-- Copyright   :  (C) 2006 Edward Kmett
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- A simple class for hashable objects to facilitate the use of "Data.THash".
-- We try to use a number of non-prime mix functions to better support the
-- power of 2 hashing function under the hood.
----------------------------------------------------------------------------

module Data.Hashable (Hashable, hash) where

import Data.Bits
import Data.Char (ord)

class Hashable a where
    hash :: a -> Int

instance Hashable Int where
    {-# INLINE hash #-}
    hash = ap xor (`shiftR` 16) 
	. ap (+) (complement . (`shiftL` 11)) 
	. ap xor (`shiftR` 6) 
	. ap (+) (`shiftL` 3) 
	. ap xor (`shiftR` 10)
	. ap (+) (complement . (`shiftL` 15))
	where ap x y z = x z $ y z 

-- based on hashString in Data.HashTable
instance Hashable a => Hashable [a] where
    {-# INLINE hash #-}
    hash = foldr f 0 
	where f c m = hash c + (m * 128) `mod` 1500007

instance Hashable Char where
    {-# INLINE hash #-}
    hash = hash . ord  

instance (Hashable a, Hashable b) => Hashable (a,b) where
    {-# INLINE hash #-}
    hash (a,b) = hash a - hash b
	
instance (Hashable a, Hashable b, Hashable c) => Hashable (a,b,c) where
    {-# INLINE hash #-}
    hash (a,b,c) = ha - (hb `xor` hc) + hc
        where 
	    ha = hash a
	    hb = hash b
	    hc = hash c 

instance (Hashable a, Hashable b, Hashable c, Hashable d) => Hashable (a,b,c,d) where
    {-# INLINE hash #-}
    hash (a,b,c,d) = hash (a,b,c) - hash d

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e) => Hashable (a,b,c,d,e) where
    {-# INLINE hash #-}
    hash (a,b,c,d,e) = hash ((a,b),c,d,e)

