-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Kleisli
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (class-associated types)
--
-------------------------------------------------------------------------------------------

module Control.Category.Kleisli where

import Prelude hiding (Functor, map, (.), id, fst, snd, Monad, return, (>>=))
import qualified Prelude
import Control.Category.Classes

newtype Kleisli m k a b = Kleisli { runKleisli :: k a (m b) }
newtype CoKleisli w k a b = CoKleisli { runCoKleisli :: k (w a) b }

-- * (co)Kleisli Category

instance Monad m k => Category (Kleisli m k) where
	id = Kleisli return
	Kleisli g . Kleisli f = Kleisli (bind g . f)

instance Comonad w k => Category (CoKleisli w k) where
	id = CoKleisli extract
	CoKleisli g . CoKleisli f = CoKleisli (g . extend f)

-- * Category Transformers

instance Monad m k => CategoryTransformer (Kleisli m) k where
	lift f = Kleisli (return . f)

instance Comonad m k => CategoryTransformer (CoKleisli m) k where
	lift f = CoKleisli (f . extract)

-- * Isomorphisms

instance (Monad m k, Iso k a b) => Iso (Kleisli m k) a b where
	iso = lift iso
	uniso = lift uniso

instance (Comonad m k, Iso k a b) => Iso (CoKleisli m k) a b where
	iso = lift iso
	uniso = lift uniso

-- * Canonical forms

instance (Monad m k, Canonical k a b) => Canonical (Kleisli m k) a b
instance (Comonad w k, Canonical k a b) => Canonical (CoKleisli w k) a b
	
-- * Arrows
{-
instance (Monad m k, Arrow k) => Arrow (Kleisli m k) where
	arr f = lift (arr f)
instance (Comonad w k, Arrow k) => Arrow (CoKleisli w k) where
	arr f = lift (arr f)

-}
-- * Contravariant Arrows

{-
instance (Monad m k, DualArrow k) => DualArrow (Kleisli m k) where
	dualarr f = lift (dualarr f)
instance (Comonad w k, DualArrow k) => DualArrow (CoKleisli w k) where
	dualarr f = lift (dualarr f)
-}
