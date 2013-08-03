-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Functor
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (class-associated types)
--
-- Redefines 'Functor' to allow it to represent a functor between different categories
-------------------------------------------------------------------------------------------

module Control.Category.Functor ( Functor(map)) where

import Prelude hiding (Functor, map)
import qualified Prelude

import Control.Category

{- | Must satisfy the functor laws:

> map id = id
> map (f . g) = map f . map g
-}
class (Category c, Category d) => Functor f c d | f c -> d, f d -> c where
	map :: c a b -> d (f a) (f b)

{-# RULES
  "map identity"       map id = id
  "map deforestation"  forall f g.  map f . map g = map (f . g)
 #-}
