{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -cpp #-}
-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Hask
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (class-associated types)
--
-- The category of Haskell types. Analogous to the 'Arrow' instance for @(->)@
-- The shortened name is a stylized convention from folklore.
-------------------------------------------------------------------------------------------

module Control.Category.Hask (Bottom) where

import Prelude hiding (Functor, map, (.), id, fst, snd, Monad, return, (>>=))
import qualified Prelude

import Control.Category.Classes
import Control.Category.Dual

data Bottom
-- NB: Hask does not actually have a Terminal object when seq is included, since you can distinguish const undefined from undefined!
instance HasTerminalObject (->) where
#ifndef __HADDOCK__
	type Terminal (->) = Bottom
#endif
	terminate = const (error "bottom encountered")
--instance Category (->) where
--	id = Prelude.id
--	f . g = \x -> f (g x)

instance Bifunctor (,) (->) (->) (->) where
	bimap f g ~(x,y) = (f x, g y)
instance Associative (,) (->) where
	associate = associateCartesian
instance Coassociative (,) (->) where
	coassociate = coassociateCartesian
instance HasIdentity (,) (->) 
#ifndef __HADDOCK__
	where type Identity (->) (,) = Bottom
#endif
instance Monoidal (,) (->) where
	idl = Prelude.snd
	idr = Prelude.fst
instance Comonoidal (,) (->) where
	coidl = \x -> (undefined,x)
	coidr = \x -> (x,undefined)
instance Braided (,) (->) where
	braid ~(x,y) = (y,x)
instance Cartesian (->) where
#ifndef __HADDOCK__
	type Prod (->) = (,)
#endif
	fst = Prelude.fst
	snd = Prelude.snd
	diag x = (x,x)

-- we only have a strict associative coproduct, not a comonoidal one!
instance Bifunctor Either (->) (->) (->) where
	bimap f g (Left x) = Left (f x)
	bimap f g (Right x) = Right (g x)
instance Coassociative Either (->) where
	coassociate = coassociateCoCartesian
instance Associative Either (->) where
	associate = associateCoCartesian
instance Braided Either (->) where
	braid (Left x) = Right x
	braid (Right x) = Left x
instance CoCartesian (->) where
#ifndef __HADDOCK__
	type Sum (->) = Either
#endif
	inl = Left
	inr = Right
	(|||) = either
	codiag = either id id

instance Functor ((,)e) (->) (->) where
	map f ~(x,t) = (x, f t)

instance Functor ((->)e) (->) (->) where
	map f g = f . g 

instance Loop (->) where
	loop f b = let (a,c) = f (b,c) in a

instance Distributive (->) where
	distribute (x,Left y) = Left (x,y)
	distribute (x,Right z) = Right (x,z)

-- conflict!
--instance Bifunctor (->) (Dual (->)) (->) (->) where
--        bimap f g h = g . h . runDual f

instance Adjunction ((,)a) ((->)a) (->) (->) where
        unit = unitCCC
        counit = counitCCC

instance CCC (->) where
#ifndef __HADDOCK__
        type Exp (->) = (->)
#endif
        apply (f,x) = f x
        curry = Prelude.curry
        uncurry = Prelude.uncurry

-- permit the use of the built-in functor composition in this category
instance HasFunctorComposition (->) where
        comp = Comp
        decomp (Comp x) = x

instance Arrow (->) where
	arr = id

instance Bifunctor p (->) (->) (->) => HasBitransform p (->) (->) (->) where
	bitransform = Bitransform
	unbitransform (Bitransform x) = x

