-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Comma
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (class-associated types)
--
-- Comma categories (aka Slice categories) <http://en.wikipedia.org/wiki/Comma_category>.
-- Note the similarity to f-algebras and f-coalgebras and the definition for a natural
-- transformation.
-------------------------------------------------------------------------------------------

module Control.Category.Comma where

import Prelude hiding (Functor, map, (.))
import qualified Prelude

import Control.Category
import Control.Category.Functor

#ifndef __HADDOCK__
data Comma (t :: * -> *) (s :: * -> *) ka kb (k :: * -> * -> *) a b where 
	Comma   :: (Functor t ka k, Functor s kb k) => ka a a' -> kb b b' -> Comma t s ka kb k (a,b) (a',b')
	CommaId :: Comma t s ka kb k a a
#endif

instance (Functor t ka k, Functor s kb k) => Category (Comma t s ka kb k) where
	id = CommaId
	CommaId . CommaId = CommaId
	CommaId . Comma g h = Comma g h
	Comma g h . CommaId = Comma g h
	Comma g h . Comma g' h' = Comma (g . g') (h . h')

preComma :: (Functor t ka k, Functor t kb k) => Comma t s ka kb k (a,b) (a',b') -> k (t a) (s b) -> k (t a) (s b')
preComma CommaId f = f
preComma (Comma g h) f = map h . f

postComma :: (Functor t ka k, Functor t kb k) => k (t a') (s b') -> Comma t s ka kb k (a,b) (a',b') -> k (t a) (s b')
postComma f CommaId = f
postComma f (Comma g h) = f . map g
