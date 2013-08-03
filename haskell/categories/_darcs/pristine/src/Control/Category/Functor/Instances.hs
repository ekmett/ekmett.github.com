-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Functor.Instances
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (class-associated types)
--
-- Automatically derives functors from bifunctors, and implements the laws for composition,
-- functor products and functor coproducts
-------------------------------------------------------------------------------------------

module Control.Category.Functor.Instances where

import Prelude hiding (Functor, map,(.), id, fst, snd, Monad, return)
import qualified Prelude

import Control.Category
import Control.Category.Bifunctor
import Control.Category.Cartesian
import Control.Category.Cartesian.Closed
import Control.Category.Comonad
import Control.Category.Functor
import Control.Category.Functor.Adjunction
import Control.Category.Functor.Full
import Control.Category.Functor.Composition
import Control.Category.Functor.Pointed
import Control.Category.Functor.Representable
import Control.Category.Monad
import Control.Category.Hask



{- |
-- * Functor composition laws

instance Bifunctor f k1 k2 k3 => Functor (f a) k2 k3 where
	map = second

instance (HasFunctorComposition e, Functor f d e, Functor g c d) => Functor (Comp f g) c e where
        map f = comp . map (map f) . decomp

instance (HasFunctorComposition c, Adjunction f g c d) => Pointed (Comp g f) c where
        return = comp . unit

instance (HasFunctorComposition c, Adjunction f g c d) => Monad (Comp g f) c where
        bind f = comp . map (rightAdjunct (decomp . f)) . decomp

instance (HasFunctorComposition d, Adjunction f g c d) => Copointed (Comp f g) d where
        extract = counit . decomp

instance (HasFunctorComposition d, Adjunction f g c d) => Comonad (Comp f g) d where
        extend f = comp . map (leftAdjunct (f . comp)) . decomp

instance (HasFunctorComposition e, Full f d e, Full g c d) => Full (Comp f g) c e where
        premap f = premap $ premap $ decomp . f . comp

instance (HasFunctorComposition e, Faithful f d e, Faithful g c d) => Faithful (Comp f g) c e 

-- * Functor bifunctor transformation laws (these are probably obvious in publication somewhere, but I derived them here)

instance (HasBitransform p k1 k2 k3, Functor f k0 k1, Functor g k0 k2) => Functor (Bitransform p f g) k0 k3 where
	map f = bitransform . bimap (map f) (map f) . unbitransform

instance (Cartesian c, HasBitransform (Prod c) c c c, Pointed f c, Pointed g c) => Pointed (Bitransform (Prod c) f g) c where
	return = bitransform . (return &&& return)

instance (CoCartesian c, HasBitransform (Sum c) c c c, Copointed f c, Copointed g c) => Copointed (Bitransform (Sum c) f g) c where
	extract = (extract ||| extract) . unbitransform

instance (HasBitransform (Prod d) d d d, Faithful f c d, Faithful g c d) => Faithful (Bitransform (Prod d) f g) c d

-}

{-
data Rep a b c = Rep (a -> c) (b -> c) 

class Functor (Rep a b) (->) (->) where
	map f (Rep l r) = Rep (f . l) (f . r)

instance Representable (Rep a b) (Either a b) (->) where
	rep f = Rep (f . inl) (f . inr)
	unrep (Rep l r) = either l r
-}
