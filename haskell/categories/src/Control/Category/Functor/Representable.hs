-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Functor.Representable
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (class-associated types)
--
-------------------------------------------------------------------------------------------

module Control.Category.Functor.Representable where

import Prelude hiding (Functor, map)
import qualified Prelude

import Control.Category
import Control.Category.Functor
import Control.Category.Cartesian

class Functor f k (->) => Representable f x k where
	rep :: k x a -> f a
	unrep :: f a -> k x a

{-# RULES
  "rep/unrep"       	rep . unrep = id
  "unrep/rep"   	unrep . rep = id
 #-}
