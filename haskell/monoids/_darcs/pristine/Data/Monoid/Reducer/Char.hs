{-# LANGUAGE UndecidableInstances, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Reducer.Char
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (MPTCs)
--
-----------------------------------------------------------------------------

module Data.Monoid.Reducer.Char
    ( module Data.Monoid.Reducer
    , CharReducer
    , invalidChar
    , fromChar
    ) where

import Data.Monoid.Reducer
import Data.Word (Word8)

-- | Provides a mechanism for the UTF8 'Monoid' to report invalid characters to one or more monoids.

class Reducer Char m => CharReducer m where
    fromChar :: Char -> m 
    fromChar = unit

    invalidChar :: [Word8] -> m
    invalidChar = const mempty

instance (CharReducer m, CharReducer m') =>  CharReducer (m,m') where
    invalidChar bs = (invalidChar bs, invalidChar bs)

instance (CharReducer m, CharReducer m', CharReducer m'') =>  CharReducer (m,m',m'') where
    invalidChar bs = (invalidChar bs, invalidChar bs, invalidChar bs)

instance (CharReducer m, CharReducer m', CharReducer m'', CharReducer m''') =>  CharReducer (m,m',m'',m''') where
    invalidChar bs = (invalidChar bs, invalidChar bs, invalidChar bs, invalidChar bs)

instance CharReducer [Char]
