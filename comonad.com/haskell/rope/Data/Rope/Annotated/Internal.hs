{-# LANGUAGE TypeOperators, Rank2Types, EmptyDataDecls, 
             MultiParamTypeClasses, FunctionalDependencies, 
             FlexibleContexts, FlexibleInstances, UndecidableInstances,
             IncoherentInstances, OverlappingInstances #-}
module Data.Rope.Annotated.Internal 
    ( A(A,rope)
    , null      -- :: A s a -> Bool
    -- * Unpackable Ropes
    , head      -- :: Unpackable t => A s a -> t
    , last      -- :: Unpackable t => A s a -> t
    , unpack    -- :: Unpackable t => A s a -> [t]
    -- * Annotation Products
    ) where

import Prelude hiding (null, head, last, take, drop, span, break, splitAt, takeWhile, dropWhile)
import Control.Applicative hiding (empty)
import Data.Rope.Util.Comonad
import Data.FingerTree (Measured(..))
import Data.Foldable (Foldable, foldMap)
import qualified Data.Foldable
import Data.Traversable (Traversable(traverse))
import qualified Data.Rope.Internal as Rope
import Data.Rope.Body (Offset(..))
import Data.Rope.Internal (Rope(..),Unpackable)

data A s a = A { rope :: !Rope, extractA :: a }

null :: A s a -> Bool
null = Rope.null . rope

head :: Unpackable t => A s a -> t
head = Rope.head . rope

last :: Unpackable t => A s a -> t
last = Rope.last . rope

unpack :: Unpackable t => A s a -> [t]
unpack (A s _) = Rope.unpack s

instance Measured Offset (A s a) where
    measure = measure . rope

instance Functor (A s) where
    fmap f (A s a) = A s (f a) 

instance Comonad (A s) where
    extract = extractA
    extend f a@(A s _) = A s (f a)
    duplicate a@(A s _) = A s a

instance Foldable (A s) where
    foldr f z (A _ a) = f a z
    foldr1 _ (A _ a) = a
    foldl f z (A _ a) = f z a
    foldl1 _ (A _ a) = a
    foldMap f (A _ a) = f a

instance Traversable (A s) where
    traverse f (A s a) = A s <$> f a
