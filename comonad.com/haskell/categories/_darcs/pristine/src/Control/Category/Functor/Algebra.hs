-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Functor.Algebra
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (functional-dependencies)
--
-------------------------------------------------------------------------------------------

module Control.Category.Functor.Algebra where

import Prelude hiding (Functor, map, (.), id, fst, snd, Monad, return, (>>=))
import qualified Prelude

import Control.Category.Classes

#ifndef __HADDOCK__
newtype Alg (f :: * -> *) k a b = Alg { runAlg :: k a b }
#endif

postAlg :: Functor f k k => k (f a) a -> Alg f k a b -> k (f a) b
postAlg f (Alg g) = g . f

preAlg :: Functor f k k => Alg f k a b -> k (f b) b -> k (f a) b
preAlg (Alg f) g = g . map f

instance Functor f k k => CategoryTransformer (Alg f) k where
	lift = Alg

instance Functor f k k => Category (Alg f k) where
	id = Alg id
	g . f = Alg (runAlg g . runAlg f)

