{-# LANGUAGE EmptyDataDecls #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parsimony.Util
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (type families, GADTs)
--
-- Miscellaneous data types used by the rest of Parsimony.
--
-----------------------------------------------------------------------------

module Text.Parsimony.Util
    ( TrivialApplicative
    , TrivialArrow
    , Id(..)
    , Magic
    ) where

import Control.Applicative
import Control.Category
import Control.Arrow

data TrivialApplicative a 
instance Functor TrivialApplicative where
    fmap = undefined
instance Applicative TrivialApplicative where
    pure = undefined
    (<*>) = undefined

data TrivialArrow a b
instance Functor (TrivialArrow a) where
    fmap = undefined
instance Category TrivialArrow where
    (.) = undefined
    id = undefined
instance Arrow TrivialArrow where
    arr = undefined
    first = undefined

-- Control.Monad.Identity may lacks an Applicative definition by default
newtype Id a = Id { runId :: a } 
instance Functor Id where
    fmap f (Id a) = Id (f a) 

instance Applicative Id where
    pure = Id
    Id f <*> Id x = Id (f x)

-- place holder used when we need a typing dodge
data Magic
