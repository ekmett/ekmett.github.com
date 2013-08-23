{-# LANGUAGE UndecidableInstances, TypeOperators, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, TypeFamilies, CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Combinators
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (type families, MPTCs)
--
-- Utilities for working with Monoids that conflict with names from the "Prelude",
-- "Data.Foldable", "Control.Monad" or elsewhere. Intended to be imported qualified.
--
-- > import Data.Monoid.Combinators as Monoid 
--
-----------------------------------------------------------------------------

module Data.Monoid.Combinators
    ( 
    -- * List-Like Monoid Production
      repeat
    , replicate
    , cycle
#ifdef M_QUICKCHECK
    -- * QuickCheck Properties
    , prop_replicate_right_distributive
#endif
    ) where

import Prelude hiding (replicate, cycle, repeat)
import Data.Monoid.Reducer

#ifdef M_QUICKCHECK 
import Test.QuickCheck
#endif

-- | A generalization of 'Data.List.cycle' to an arbitrary 'Monoid'. May fail to terminate for some values in some monoids.
cycle :: Monoid m => m -> m
cycle xs = xs' where xs' = xs `mappend` xs'

-- | A generalization of 'Data.List.repeat' to an arbitrary 'Monoid'. May fail to terminate for some values in some monoids.
repeat :: (e `Reducer` m) => e -> m 
repeat x = xs where xs = cons x xs 

-- | A generalization of 'Data.List.replicate' to an arbitrary 'Monoid'. Adapted from 
-- <http://augustss.blogspot.com/2008/07/lost-and-found-if-i-write-108-in.html>
replicate :: (Monoid m, Integral n) => m -> n -> m
replicate x0 y0 
    | y0 < 0 = error "Data.Monoid.Combinators.replicate: negative length"
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
{-# INLINE replicate #-}

#ifdef M_QUICKCHECK
prop_replicate_right_distributive :: (Eq m, Monoid m, Arbitrary m, Integral n) => m -> n -> n -> Bool
prop_replicate_right_distributive m x y
    = replicate m (x + y) == replicate m x `mappend` replicate m y
#endif
