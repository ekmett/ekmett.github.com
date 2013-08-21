-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Comonad
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (class-associated types)
--
-- Defines 'Comonad' in a way that allows it to represent a 'Comonad' over 
-- a 'Category' other than just 'Hask'.
-------------------------------------------------------------------------------------------

module Control.Category.Comonad 
	( Comonad (extend, duplicate), mapComonad
	) where

import Prelude hiding (Functor, map, (.), id, Monad, return, (>>=),(=<<),(>>))
import Control.Category
import Control.Category.Functor
import Control.Category.Functor.Pointed

{- | Instances of Comonad should satisfy the following laws:

> extract . extend k = k
> extend extract = id
> map f = extend (f . extract)

-}

class Copointed w k => Comonad w k where
	extend :: k (w a) b -> k (w a) (w b)
	duplicate :: k (w a) (w (w a))

	extend f = map f . duplicate
	duplicate = extend id

{-# RULES
        "extend extract"        extend extract = id
        "extract . extend k"    forall k. extract . extend k = k
 #-}

-- | free definition of 'Functor' for a 'Comonad' @m@ over a 'Category' @k@

mapComonad :: Comonad w k => k a b -> k (w a) (w b)
mapComonad f = extend (f . extract)
