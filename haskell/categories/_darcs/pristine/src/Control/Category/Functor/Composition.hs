-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Functor.Composition
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (class-associated types)
--
-- Generalized functor composition.
-------------------------------------------------------------------------------------------

module Control.Category.Functor.Composition where

import Prelude hiding (Functor, map, id, (.), Monad, return)
import qualified Prelude
import Control.Category
import Control.Category.Bifunctor
import Control.Category.Functor
import Control.Category.Functor.Adjunction
import Control.Category.Functor.Pointed
import Control.Category.Functor.Full
import Control.Category.Monad
import Control.Category.Comonad

newtype Comp f g a = Comp (f (g a))

-- | Categories don't always give us a way to deal with the newtype labels. 
-- This hack witnesses the isomorphism.
class Category e => HasFunctorComposition e 
    where
	comp :: e (f (g a)) (Comp f g a)
	decomp :: e (Comp f g a) (f (g a))

{-# RULES
  "decomp/comp" decomp . comp = id
  "comp/decomp" comp . decomp = id
  #-}

-- | 'Bitransform' transforms a pair of functors using a bifunctor as a functor transformer
-- and boxes it up for typeclass purposes. 'HasBitransform' witnesses this isomorphism.

newtype Bitransform p f g a = Bitransform (p (f a) (g a))

class Bifunctor p k1 k2 k3 => HasBitransform p k1 k2 k3 | p k1 -> k2 k3, p k2 -> k1 k3, p k3 -> k1 k2 where
	bitransform   :: k3 (p (f a) (g a)) (Bitransform p f g a)
	unbitransform :: k3 (Bitransform p f g a) (p (f a) (g a))

{-# RULES
  "bitransform/unbitransform" bitransform . unbitransform = id
  "unbitransform/bitransform" unbitransform . bitransform = id
  #-}

