-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Additive
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- More easily understood aliases for "mappend" and "mempty" 
--
-- > import Data.Monoid.Additive
--
-----------------------------------------------------------------------------

module Data.Monoid.Additive
    ( module Data.Monoid 
    , plus
    , zero
    ) where

import Data.Monoid

infixl 6 `plus`

plus :: Monoid m => m -> m -> m 
plus = mappend

zero :: Monoid m => m 
zero = mempty
