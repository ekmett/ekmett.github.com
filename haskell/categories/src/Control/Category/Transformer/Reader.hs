-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Transformer.Reader
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (class-associated types)
--
-------------------------------------------------------------------------------------------

module Control.Category.Transformer.Reader where

import Prelude hiding (Functor,map,id,fst,snd,curry,uncurry,(.))
import Control.Category.Classes

class Category k => CategoryReader e k | k -> e where
	ask :: k a e

-- t| his is the CoKleisli construction for the Coreader Comonad 
-- unfortunately, I can't wrap that comonad in a newtype to use
-- it in its full generality
newtype Reader e k a b = Reader { runReader :: k (Prod k e a) b }

instance Cartesian k => Category (Reader e k) where
	id = Reader snd
	Reader f . Reader g = Reader $ f . (fst &&& g)

instance Cartesian k => CategoryTransformer (Reader e) k where
	lift g = Reader (g . snd)

instance Cartesian k => CategoryReader e (Reader e k) where
	ask = Reader fst

instance (Cartesian k, HasTerminalObject k) => HasTerminalObject (Reader e k) where
#ifndef __HADDOCK__
	type Terminal (Reader e k) = Terminal k
#endif
	terminate = lift terminate

instance (Cartesian k, HasInitialObject k) => HasInitialObject (Reader e k) where
#ifndef __HADDOCK__
	type Initial (Reader e k) = Initial k
#endif
	initiate = lift initiate

--instance (Functor f k k, Cartesian k) => Functor f (Reader e k) (Reader e k) where
--	map (Reader f) = lift . map . runReader

instance (Cartesian k) => Bifunctor (Prod k) (Reader e k) (Reader e k) (Reader e k) where
	bimap (Reader f) (Reader g) = Reader $ bimap f g . (second fst &&& second snd)
	--first (Reader f) = Reader $ first f . (second fst &&& second snd) 
	--second (Reader g) = Reader $ second g . (second fst &&& second snd)

instance (Cartesian k, CoCartesian k) => Bifunctor (Sum k) (Reader e k) (Reader e k) (Reader e k) where
	 bimap (Reader f) (Reader g) = Reader undefined -- bimap f g . (second inl ||| second inr)
	-- ...

instance (Cartesian k, Bifunctor f (Reader e k) (Reader e k) (Reader e k), Associative f k) => Associative f (Reader e k) where
	associate = lift associate

instance (Cartesian k, Bifunctor f (Reader e k) (Reader e k) (Reader e k), Coassociative f k) => Coassociative f (Reader e k) where
	coassociate = lift coassociate

instance (Bifunctor f (Reader e k) (Reader e k) (Reader e k), HasIdentity f k) => HasIdentity f (Reader e k) 
#ifndef __HADDOCK__
	where type Identity (Reader e k) f = Identity k f
#endif

instance (Bifunctor f (Reader e k) (Reader e k) (Reader e k), Monoidal f k, Cartesian k) => Monoidal f (Reader e k) where
	idl = lift idl
	idr = lift idr

instance (Bifunctor f (Reader e k) (Reader e k) (Reader e k), Comonoidal f k, Cartesian k) => Comonoidal f (Reader e k) where
	coidl = lift coidl
	coidr = lift coidr

instance Cartesian k => Cartesian (Reader e k) where
#ifndef __HADDOCK__
	type Prod (Reader e k) = Prod k
#endif
	fst = lift fst
	snd = lift snd
	diag = lift diag

instance (Cartesian k, CoCartesian k) => CoCartesian (Reader e k) where
#ifndef __HADDOCK__
	type Sum (Reader e k) = Sum k
#endif
	inl = lift inl
	inr = lift inr
	codiag = lift codiag

instance (Bifunctor p (Reader e k) (Reader e k) (Reader e k), Cartesian k, Braided p k) => Braided p (Reader e k) where
	braid = lift braid
