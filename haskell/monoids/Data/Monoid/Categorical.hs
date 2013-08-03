{-# LANGUAGE GADTs, FlexibleInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Categorical
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Data.Monoid.Categorical
    ( module Data.Monoid.Reducer
    , module Control.Category
    -- * Generalized Endo
    , GEndo(GEndo, getGEndo)
    -- * Monoids as Categories
    , CMonoid
    , categoryToMonoid
    , monoidToCategory
    ) where

import Prelude hiding ((.),id)
import Data.Monoid.Reducer
import Control.Category

-- | The 'Monoid' of the endomorphisms over some object in an arbitrary 'Category'.
data GEndo k a = GEndo { getGEndo :: k a a } 

instance Category k =>  Monoid (GEndo k a) where
    mempty = GEndo id
    GEndo f `mappend` GEndo g = GEndo (f . g)

-- | A 'Monoid' is just a 'Category' with one object. This fakes that with a GADT
data CMonoid m n o where
    M :: Monoid m => m -> CMonoid m a a

-- | Extract the 'Monoid' from its representation as a 'Category'
categoryToMonoid :: CMonoid m m m -> m 
categoryToMonoid (M m) = m
{-# INLINE categoryToMonoid #-}

-- | Convert a value in a 'Monoid' into an arrow in a 'Category'.
monoidToCategory :: Monoid m => m -> CMonoid m m m 
monoidToCategory = M 
{-# INLINE monoidToCategory #-}

instance Monoid m => Category (CMonoid m) where
    id = M mempty
    M a . M b = M (a `mappend` b)

instance Monoid m => Monoid (CMonoid m m m) where
    mempty = id
    mappend = (.)

instance (c `Reducer` m) => Reducer c (CMonoid m m m) where
    unit = M . unit

instance Monoid m => Reducer (CMonoid m m m) m where
    unit (M m) = m 
