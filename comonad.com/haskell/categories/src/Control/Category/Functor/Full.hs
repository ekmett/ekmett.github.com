-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Functor.Full
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (class-associated types)
--
-------------------------------------------------------------------------------------------

module Control.Category.Functor.Full where

import Prelude hiding (Functor, map, (.), id)
import Control.Category
import Control.Category.Functor

{- |
	A 'Full' 'Functor' @F : C -> D@ provides for every pair of objects @c@, @c'@ in @C@
	and every morphism @g : F c -> F c'l@ in @D@, a morphism @g' : c -> c'@ in @C@. In short
	map has a right-inverse under composition.

> map . premap = id
-}

class Functor f c d => Full f c d | f c -> d, f d -> c where
	premap :: d (f a) (f b) -> c a b
	
{-# RULES
	"map/premap" 	map . premap = id
 #-}

class Functor f c d => Faithful f c d | f c -> d, f d -> c

{- | 

For every pair of objects @a@ and @b@ in @C@ a 'Full' 'Faithful' 'Functor' @F : C -> D@ maps every morphism 
@f : a -> b@ onto a distinct morphism @f : T a -> T b@ (since it is faithful) and every morphism from 
@g : T a -> T b@ can be obtained from some @f@. (It maps Hom-sets bijectively, or in short map has both
a left and right inverse under composition.

> unmap . map = id
-}

unmap :: (Full f c d, Faithful f c d) => d (f a) (f b) -> c a b
unmap = premap

{-# RULES
	"unmap/map"	unmap . map = id
 #-}
