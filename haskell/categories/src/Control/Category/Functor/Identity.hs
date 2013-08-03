-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Functor.Identity
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (class-associated types)
--
-------------------------------------------------------------------------------------------

module Control.Category.Functor.Identity where

import Prelude hiding (Functor, map, id, (.), return, Monad)
import qualified Prelude

import Control.Category
import Control.Category.Functor.Full
import Control.Category.Functor.Pointed
import Control.Category.Functor.Applicative
import Control.Category.Functor
import Control.Category.Monad
import Control.Category.Comonad
import Control.Category.Hask

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity (->) (->) where
	map f = return . f . extract 

instance Full Identity (->) (->) where
	premap f = extract . f . return

instance Faithful Identity (->) (->)

instance Pointed Identity (->) where
	return = Identity

instance Copointed Identity (->) where
	extract = runIdentity

instance Applicative Identity (->) (->) where
	ap (Identity f, Identity x) = Identity (f x)
	
instance Monad Identity (->) where
	join = extract

instance Comonad Identity (->) where
	duplicate = return
