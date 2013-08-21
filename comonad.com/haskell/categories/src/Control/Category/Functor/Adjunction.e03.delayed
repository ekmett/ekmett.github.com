{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}
-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Functor.Adjunction
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (functional-dependencies)
--
-------------------------------------------------------------------------------------------

module Control.Category.Functor.Adjunction where

import Prelude hiding (Functor, map, (.), id, fst, snd, Monad, return, (>>=))
import qualified Prelude

import Control.Category
import Control.Category.Functor

-- | An 'Adjunction' between 'Category' c and 'Category' d formed by the 'Functor' f and 'Functor' g. 

-- Minimal definition:

-- 1. @leftAdjunct@ and @rightAdjunct@ 

-- 2. @unit@ and @counit@

class (Functor f c d, Functor g d c) => Adjunction f g c d | f c -> g d, f d -> g c, g c -> f d, g d -> f c where
	unit   :: c a (g (f a))
	counit :: d (f (g a)) a
	leftAdjunct :: d (f a) b -> c a (g b)
	rightAdjunct :: c a (g b) -> d (f a) b

	unit = leftAdjunct id
	counit = rightAdjunct id
	leftAdjunct f = map f . unit
	rightAdjunct f = counit . map f
