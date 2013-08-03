-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Cartesian
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (class-associated types)
--
-------------------------------------------------------------------------------------------
module Control.Category.Cartesian
	( 
	-- * Cartesian categories
	  Cartesian(..)
	, bimapCartesian, braidCartesian, associateCartesian, coassociateCartesian
	-- * Co-Cartesian categories
	, CoCartesian(..)
	, bimapCoCartesian, braidCoCartesian, associateCoCartesian, coassociateCoCartesian
	) where

import Prelude hiding (Functor, map, (.), id, fst, snd, curry, uncurry)

import Control.Category
import Control.Category.Functor
import Control.Category.Functor.Adjunction

import Control.Category.Bifunctor
import Control.Category.Bifunctor.Associative
import Control.Category.Bifunctor.Braided
import Control.Category.Bifunctor.Monoidal

{- |
NB: This is weaker than traditional category with products! That would have to look like
@class (Monoidal k (Prod k), Braided k (Prod k)) => Cartesian k where ...@
the problem is @(->)@ lacks an initial object, since every type is inhabited in Haskell.
Consequently its coproduct 'monoid' has no identity and we want to be able to
describe its dual category, which has this non-traditional form being built
over a category with an associative bifunctor rather than as a monoidal category
for the product monoid.

Minimum definition: 

> fst, snd, diag 
> fst, snd, (&&&)
-}
class (Associative (Prod k) k, Coassociative (Prod k) k, Braided (Prod k) k) => Cartesian k where
#ifndef __HADDOCK__
	type Prod k :: * -> * -> *
#endif
	fst :: k (Prod k a b) a
	snd :: k (Prod k a b) b
	diag :: k a (Prod k a a)
	(&&&) :: k a b -> k a c -> k a (Prod k b c)

	diag = id &&& id
	f &&& g = diag >>> bimap f g 

{-# RULES
	"fst . diag"  	fst . diag = id
	"snd . diag"	snd . diag = id
	"fst . f &&& g" forall f g. fst . (f &&& g) = f
	"snd . f &&& g" forall f g. snd . (f &&& g) = g
 #-}

-- | free construction of 'Bifunctor' for the product 'Bifunctor' @Prod k@ if @(&&&)@ is known
bimapCartesian :: Cartesian k => k a c -> k b d -> k (Prod k a b) (Prod k c d)
bimapCartesian f g = (f . fst) &&& (g . snd)
	
-- | free construction of 'Braided' for the product 'Bifunctor' @Prod k@
braidCartesian :: Cartesian k => k (Prod k a b) (Prod k b a)
braidCartesian = snd &&& fst

-- | free construction of 'Associative' for the product 'Bifunctor' @Prod k@
associateCartesian :: Cartesian k => k (Prod k (Prod k a b) c) (Prod k a (Prod k b c))
associateCartesian = (fst . fst) &&& first snd

-- | free construction of 'Coassociative' for the product 'Bifunctor' @Prod k@
coassociateCartesian :: Cartesian k => k (Prod k a (Prod k b c)) (Prod k (Prod k a b) c)
coassociateCartesian = braid . second braid . associateCartesian . first braid . braid 

-- * Co-Cartesian categories

-- a category that has finite coproducts, weakened the same way as Cartesian above was weakened
class (Associative (Sum k) k, Coassociative (Sum k) k, Braided (Sum k) k) => CoCartesian k where
#ifndef __HADDOCK__
	type Sum k :: * -> * -> *
#endif
	inl :: k a (Sum k a b)
	inr :: k b (Sum k a b)
	codiag :: k (Sum k a a) a
	(|||) :: k a c -> k b c -> k (Sum k a b) c

	codiag = id ||| id
	f ||| g = bimap f g >>> codiag 

{-# RULES
	"codiag . inl"  codiag . inl = id
	"codiag . inr"	codiag . inr = id
	"(f ||| g) . inl" forall f g. (f ||| g) . inl = f
	"(f &&& g) . inr" forall f g. (f &&& g) . inr = g
 #-}

-- | free construction of 'Bifunctor' for the coproduct 'Bifunctor' @Sum k@ if @(|||)@ is known
bimapCoCartesian :: CoCartesian k => k a c -> k b d -> k (Sum k a b) (Sum k c d)
bimapCoCartesian f g = (inl . f) ||| (inr . g)

-- | free construction of 'Braided' for the coproduct 'Bifunctor' @Sum k@
braidCoCartesian :: CoCartesian k => k (Sum k a b) (Sum k b a)
braidCoCartesian = inr ||| inl

-- | free construction of 'Associative' for the coproduct 'Bifunctor' @Sum k@
associateCoCartesian :: CoCartesian k => k (Sum k (Sum k a b) c) (Sum k a (Sum k b c))
associateCoCartesian = braid . first braid . coassociateCoCartesian . second braid . braid

-- | free construction of 'Coassociative' for the coproduct 'Bifunctor' @Sum k@
coassociateCoCartesian :: CoCartesian k => k (Sum k a (Sum k b c)) (Sum k (Sum k a b) c)
coassociateCoCartesian = (inl . inl) ||| first inr

