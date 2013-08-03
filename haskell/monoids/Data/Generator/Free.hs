{-# LANGUAGE UndecidableInstances , FlexibleContexts , MultiParamTypeClasses , FlexibleInstances , GeneralizedNewtypeDeriving, ExistentialQuantification, TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generator.Free
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (MPTCs)
--
-----------------------------------------------------------------------------

module Data.Generator.Free
    ( module Data.Generator
    , module Data.Monoid.Reducer
    , Free (AnyGenerator)
    ) where

import Control.Functor.Pointed
import Control.Monad
import Data.Generator
import Data.Foldable
import Data.Monoid.Reducer
import Data.Monoid.Additive
import qualified Data.Generator.Combinators as Generator
import Data.Monoid.Self

data Free a 
    = a `Cons` Free a
    | Free a `Snoc` a
    | Free a `Plus` Free a
    | Unit a
    | Empty
    | forall c. (Generator c, Elem c ~ a) => AnyGenerator c

instance Eq a => Eq (Free a) where
    a == b = Generator.toList a == Generator.toList b
    a /= b = Generator.toList a == Generator.toList b

instance Ord a => Ord (Free a) where
    a <= b = Generator.toList a <= Generator.toList b
    a >= b = Generator.toList a >= Generator.toList b
    a < b  = Generator.toList a <  Generator.toList b
    a > b  = Generator.toList a >  Generator.toList b
    a `compare` b = Generator.toList a `compare` Generator.toList b

instance Monoid (Free a) where
    mempty = Empty
    mappend = Plus

instance Reducer a (Free a) where
    unit = Unit

    snoc Empty a = Unit a
    snoc a b = Snoc a b

    cons b Empty = Unit b
    cons a b = Cons a b 

instance Functor Free where
    fmap f (a `Cons` b) = f a `Cons` fmap f b
    fmap f (a `Snoc` b) = fmap f a `Snoc` f b
    fmap f (a `Plus` b) = fmap f a `Plus` fmap f b
    fmap f (Unit a) = Unit (f a)
    fmap _ Empty = Empty
    fmap f (AnyGenerator c) = mapReduce f c

instance Pointed Free where
    point = Unit

instance Monad Free where
    return = Unit
    a `Cons` b >>= k     = k a `Plus` (b >>= k)
    a `Snoc` b >>= k     = (a >>= k) `Plus` k b
    a `Plus` b >>= k     = (a >>= k) `Plus` (b >>= k)
    Unit a >>= k         = k a
    Empty >>= _          = Empty
    AnyGenerator c >>= k = getSelf (mapReduce k c)

instance MonadPlus Free where
    mzero = Empty
    mplus = Plus

instance Foldable Free where
    foldMap f (a `Cons` b)     = f a `mappend` foldMap f b
    foldMap f (a `Snoc` b)     = foldMap f a `mappend` f b
    foldMap f (a `Plus` b)     = foldMap f a `mappend` foldMap f b
    foldMap f (Unit a)         = f a 
    foldMap _ Empty            = mempty
    foldMap f (AnyGenerator c) = Generator.foldMap f c

instance Generator (Free a) where
    type Elem (Free a) = a
    mapReduce f (a `Cons` b)     = f a `cons` mapReduce f b
    mapReduce f (a `Snoc` b)     = mapReduce f a `snoc` f b
    mapReduce f (a `Plus` b)     = mapReduce f a `plus` mapReduce f b
    mapReduce f (Unit a)         = unit (f a)
    mapReduce _ Empty            = mempty
    mapReduce f (AnyGenerator c) = mapReduce f c
    
    mapTo f m (a `Cons` b)       = m `plus` (f a `cons` mapReduce f b)
    mapTo f m (a `Snoc` b)       = mapTo f m a `snoc` f b
    mapTo f m (a `Plus` b)       = mapTo f m a `plus` mapReduce f b
    mapTo f m (Unit a)           = m `snoc` f a
    mapTo _ m Empty              = m 
    mapTo f m (AnyGenerator c)   = mapTo f m c
    
    mapFrom f (a `Cons` b)     m = f a `cons` mapFrom f b m 
    mapFrom f (a `Snoc` b)     m = mapFrom f a (f b `cons` m)
    mapFrom f (a `Plus` b)     m = mapReduce f a `plus` mapFrom f b m
    mapFrom f (Unit a)         m = f a `cons` m
    mapFrom _ Empty            m = m 
    mapFrom f (AnyGenerator c) m = mapFrom f c m 
