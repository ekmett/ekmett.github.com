{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.FromString
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (overloaded strings, MPTCs)
--
-- Transform any 'Char' 'Reducer' into an 'IsString' instance so it can be
-- used directly with overloaded string literals.
--
-----------------------------------------------------------------------------

module Data.Monoid.FromString 
    ( module Data.Monoid.Reducer
    , FromString(FromString,getFromString)
    ) where

import Control.Functor.Pointed
import Data.Generator
import Data.Monoid.Reducer
import Data.Monoid.Instances ()
import Data.String

data FromString m = FromString { getFromString :: m } 

instance Monoid m => Monoid (FromString m) where
    mempty = FromString mempty
    FromString a `mappend` FromString b = FromString (a `mappend` b)

instance (Char `Reducer` m) => Reducer Char (FromString m) where
    unit = FromString . unit

instance (Char `Reducer` m) => IsString (FromString m) where
    fromString = FromString . reduce

instance Pointed FromString where
    point = FromString

instance Copointed FromString where
    extract = getFromString

instance Functor FromString where
    fmap f (FromString x) = FromString (f x)
