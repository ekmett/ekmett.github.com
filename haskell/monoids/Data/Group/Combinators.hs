-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Group.Combinators
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Utilities for working with Groups that conflict with names from the "Prelude".
--
-- Intended to be imported qualified.
--
-- > import Data.Group.Combinators as Group (replicate)
--
-----------------------------------------------------------------------------

module Data.Group.Combinators
    ( module Data.Group
    -- * Combinators
    , replicate
    -- * QuickCheck Properties
    , prop_replicate_right_distributive
    ) where

import Prelude hiding (replicate)
import Data.Group
import Test.QuickCheck

-- shamelessly stolen from Lennart Augustsson's post: 
-- http://augustss.blogspot.com/2008/07/lost-and-found-if-i-write-108-in.html
-- adapted to groups, which can permit negative exponents
replicate :: (Group m, Integral n) =>  m -> n -> m
replicate x0 y0 
    | y0 < 0 = f (gnegate x0) (negate y0)
    | y0 == 0 = mempty
    | otherwise = f x0 y0
    where
        f x y 
            | even y = f (x `mappend` x) (y `quot` 2)
            | y == 1 = x
            | otherwise = g (x `mappend` x) ((y - 1) `quot` 2) x
        g x y z 
            | even y = g (x `mappend` x) (y `quot` 2) z
            | y == 1 = x `mappend` z
            | otherwise = g (x `mappend` x) ((y - 1) `quot` 2) (x `mappend` z)

prop_replicate_right_distributive :: (Eq g, Group g, Arbitrary g, Integral n) => g -> n -> n -> Bool
prop_replicate_right_distributive g x y 
    = replicate g (x + y) == replicate g x `mappend` replicate g y
