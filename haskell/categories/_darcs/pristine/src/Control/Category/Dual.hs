-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Dual
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (class-associated types)
--
-- The dual of a category is another category. 
-- 
-- This notion appears to be fundamentally flawed because the instances here seem to actively
-- get in the way of other instances. TODO: wrap functors 
-------------------------------------------------------------------------------------------

module Control.Category.Dual where

import Prelude hiding (Functor, map, (.), id, fst, snd, Monad, return, (>>=), curry, uncurry)
import qualified Prelude

import Control.Category.Classes

newtype Dual k a b = Dual { runDual :: k b a }

instance Category k => Category (Dual k) where
	id = Dual id
	f . g = Dual (runDual g . runDual f)

instance Bifunctor p k1 k2 k3 => Bifunctor p (Dual k1) (Dual k2) (Dual k3) where
	bimap f g = Dual $ bimap (runDual f) (runDual g)

instance Associative p k => Coassociative p (Dual k) where
	coassociate = Dual associate

instance Coassociative p k => Associative p (Dual k) where
	associate = Dual coassociate

instance Braided p k => Braided p (Dual k) where
	braid = Dual braid

instance Cartesian k => CoCartesian (Dual k) where
#ifndef __HADDOCK__
	type Sum (Dual k) = Prod k
#endif
	inl = Dual fst
	inr = Dual snd
	f ||| g = Dual (runDual f &&& runDual g)
	codiag = Dual diag

instance CoCartesian k => Cartesian (Dual k) where
#ifndef __HADDOCK__
	type Prod (Dual k) = Sum k
#endif
	fst = Dual inl
	snd = Dual inr
	f &&& g = Dual (runDual f ||| runDual g)
	diag = Dual codiag

instance Monoidal p k => Comonoidal p (Dual k) where
	coidl = Dual idl
	coidr = Dual idr
	
instance Comonoidal p k => Monoidal p (Dual k) where
	idl = Dual coidl
	idr = Dual coidr

instance HasIdentity p k => HasIdentity p (Dual k) 
#ifndef __HADDOCK__
	where type Identity (Dual k) p = Identity k p 
#endif

instance Functor f k k => Functor f (Dual k) (Dual k) where
	map = Dual . map . runDual

instance Full f k k => Full f (Dual k) (Dual k) where
	premap = Dual . premap . runDual

instance Faithful f k k => Faithful f (Dual k) (Dual k)

instance Pointed f k => Copointed f (Dual k) where
	extract = Dual return

instance Copointed f k => Pointed f (Dual k) where
	return = Dual extract

instance Monad m k => Comonad m (Dual k) where
	extend = Dual . bind . runDual
	duplicate = Dual join

instance Comonad w k => Monad w (Dual k) where
	bind = Dual . extend . runDual
	join = Dual duplicate

instance Adjunction f g k k => Adjunction g f (Dual k) (Dual k) where
	unit = Dual counit
	counit = Dual unit
	leftAdjunct = Dual . rightAdjunct . runDual
	rightAdjunct = Dual . leftAdjunct . runDual

instance CCC k => CoCCC (Dual k) where
#ifndef __HADDOCK__
	type Coexp (Dual k) = Exp k
#endif
	coapply = Dual apply
	cocurry = Dual . curry . runDual
	uncocurry = Dual . uncurry . runDual

instance CoCCC k => CCC (Dual k) where
#ifndef __HADDOCK__
	type Exp (Dual k) = Coexp k
#endif
	apply = Dual coapply
	curry = Dual . cocurry . runDual
	uncurry = Dual . uncocurry . runDual

instance (
	Arrow k
#ifndef __HADDOCK__
	, Prod k ~ (,)
#endif
	) => DualArrow (Dual k) where
	dualarr f = Dual (arr f)

instance (
	DualArrow k
#ifndef __HADDOCK__
	, Sum k ~ (,)
#endif
	) => Arrow (Dual k) where
	arr f = Dual (dualarr f)

liftDualDual :: k a b -> Dual (Dual k) a b
liftDualDual = Dual . Dual

lowerDualDual :: Dual (Dual k) a b -> k a b
lowerDualDual = runDual . runDual

instance HasFunctorComposition k => HasFunctorComposition (Dual k) where
	comp = Dual decomp
	decomp = Dual comp

instance HasBitransform p c d e => HasBitransform p (Dual c) (Dual d) (Dual e) where
	unbitransform = Dual bitransform
	bitransform = Dual unbitransform

