{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Self
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- A simple 'Monoid' transformer that takes a 'Monoid' m and produces a new @m@-Reducer named 'Self' @m@
-- 
-- This is useful when you have a generator that already contains monoidal values or someone supplies
-- the map to the monoid in the form of a function rather than as a "Reducer" instance. You can just
-- @'getSelf' . `reduce`@ or @'getSelf' . 'mapReduce' f@ in those scenarios. These behaviors are encapsulated 
-- into the 'fold' and 'foldMap' combinators in "Data.Monoid.Combinators" respectively.
--
-----------------------------------------------------------------------------

module Data.Monoid.Self
    ( module Data.Monoid.Reducer
    , Self(Self, getSelf)
    )  where

import Control.Functor.Pointed
import Data.Monoid.Reducer
import Data.Generator

newtype Self m = Self { getSelf :: m } deriving (Monoid)

instance Monoid m => Reducer m (Self m) where
    unit = Self

instance Functor Self where
    fmap f (Self x) = Self (f x)

instance Pointed Self where
    point = Self

instance Copointed Self where
    extract = getSelf
