-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Functor.Native
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (class-associated types)
--
-------------------------------------------------------------------------------------------

module Control.Category.Functor.Native where

import Prelude hiding (Functor, map, id, (.), fst, snd, return, Monad)
import qualified Prelude
import Control.Category.Classes hiding ((>>=))

newtype NativeF f a = NativeF { runNativeF :: f a } 

instance Prelude.Functor f => Functor (NativeF f) (->) (->) where
	map f (NativeF x) = NativeF (Prelude.fmap f x)

instance (Prelude.Functor m, Prelude.Monad m) => Pointed (NativeF m) (->) where
	return x = NativeF (Prelude.return x)

instance (Prelude.Functor m, Prelude.Monad m) => Monad (NativeF m) (->) where
	bind f (NativeF x) = NativeF (x >>= (runNativeF . f))

