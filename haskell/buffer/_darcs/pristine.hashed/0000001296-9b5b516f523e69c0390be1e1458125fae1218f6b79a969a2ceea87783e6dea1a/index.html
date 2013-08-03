{-# LANGUAGE CPP #-}
-- We cannot actually specify all the language pragmas, see ghc ticket #
-- If we could, these are what they would be:
{- LANGUAGE MagicHash, UnboxedTuples #-}
{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module      : Data.Buffer.Internal.Classes
-- Copyright   : c) Edward Kmett 2010
-- License     : BSD-style
--
-- Maintainer  : ekmett@gmail.com
-- Stability   : experimental
-- Portability : portable
-- 
-- NB: the types for measure' and size' vary across platforms. Use measure and size where possible

module Data.Buffer.Internal.Classes (
    Measured, measure, measureU, 
    Sized, size, sizeU,
    Valid, valid
    ) where

#include "measure.h"

class Measured a where
    -- | /O(1)/ The number of bytes in the sequence and the number of multibyte tail bytes present as well
    measureU :: a -> TUPLE(INT,INT)

measure :: a -> (Int, Int)
measure a = (sa, ea) where MEASURED(a)

chars :: a -> Int
chars = sa - ea where MEASURED(a)

class Sized a where
    -- | /O(1)/ The number of elements in the sequence.
    sizeU :: a -> INT

size :: a -> Int
size a = BOX (sizeU a)

class Valid a where
    -- | Sanity check to make sure invariants are all met. 
    -- No use of the public portion of the API should be able to cause this to return False.
    valid :: a -> Bool
