-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Monad
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (class-associated types)
--
-- Defines 'Monad' and 'Comonad' in a way that allows it to represent a (co)monad over 
-- other categories than just the category of types.
-------------------------------------------------------------------------------------------

module Control.Category.Monad 
	( 
	-- * Categorical monads and comonads
	  Monad (bind, join)
	, mapMonad
	-- * Traditional monad sugar
	, (=<<), (>>=), (<<), (>>)
	) where

import Prelude hiding (Functor, map, (.), id, Monad, return, (>>=),(=<<),(>>))
import Control.Category
import Control.Category.Functor
import Control.Category.Functor.Pointed

infixr 1 =<<, <<
infixl 1 >>=, >>

{- | Instances of Monad should satisfy the following laws:

> bind k . return = k
> bind return = id
> map f = bind (return . f)
-}

class Pointed m k => Monad m k where
	bind :: k a (m b) -> k (m a) (m b)
	join :: k (m (m a)) (m a)

	bind f = join . map f
	join = bind id

{-# RULES
	"bind return" 		bind return = id
	"bind k/return" 	forall k. bind k . return = k
-}

-- | free definition of 'Functor' for a 'Monad' @m@ over a 'Category' @k@
mapMonad :: Monad m k => k a b -> k (m a) (m b)
mapMonad f = bind (return . f)

(>>=) :: Monad m (->) => m a -> (a -> m b) -> m b
m >>= k = bind k m 
(>>) :: Monad m (->) => m a -> m b -> m b
m >> n = bind (const n) m
(<<) :: Monad m (->) => m b -> m a -> m b
n << m = m >> n
(=<<) :: Monad m (->) => (a -> m b) -> m a -> m b
k =<< m = m >>= k
