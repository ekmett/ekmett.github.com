-- {-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}
-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Bifunctor.Associative
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (class-associated types)
--
-- In order to appropriately represent the category of Haskell types and its dual, we need
-- a category with a bifunctor that acts as a semigroup (that is to say 'is associative') 
-- but which unlike a monoidal category, lacks an identity (since there can be no initial 
-- object in the category Hask, but it also has a near-coproduct in the form of 'Either'.
--
-- NB: this contradicts another common meaning for an 'Associative' 'Category', which is one 
-- where the pentagonal condition does not hold, but for which there is an identity.
--
-------------------------------------------------------------------------------------------
module Control.Category.Bifunctor.Associative where

import Control.Category.Bifunctor

{- | A category with an associative bifunctor satisfying Mac Lane\'s pentagonal coherence identity law:

> second associate . associate . first associate = associate . associate
-}
class Bifunctor p k k k => Associative p k where
	associate :: k (p (p a b) c) (p a (p b c))

{- | A category with a coassociative bifunctor satisyfing the dual of Mac Lane's pentagonal coherence identity law:

> first coassociate . coassociate . second coassociate = coassociate . coassociate
-}
class Bifunctor s k k k => Coassociative s k where
	coassociate :: k (s a (s b c)) (s (s a b) c)

{-# RULES
	"copentagonal coherence"
		first coassociate . coassociate . second coassociate = coassociate . coassociate
	"pentagonal coherence"
		second associate . associate . first associate = associate . associate
 #-}
