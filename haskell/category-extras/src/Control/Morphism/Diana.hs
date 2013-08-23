{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Morphism.Diana
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
-- 
----------------------------------------------------------------------------
module Control.Morphism.Diana 
	( Dialgebra
	, NuD(..)
	, diana
	) where

import Control.Functor.Algebra
import Control.Functor.Fix

diana :: Dialgebra f g a -> a -> NuD f g
diana = NuD

outD :: (Functor f, Functor g) => NuD f g -> Colim f -> g (NuD f g)
outD (Diana f a) (Colim bs) = fmap (Diana f) (f (fmap (const a) bs))
