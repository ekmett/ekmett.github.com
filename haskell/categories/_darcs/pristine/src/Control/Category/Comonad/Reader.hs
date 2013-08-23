-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Comonad.Reader
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (class-associated types)
--
-------------------------------------------------------------------------------------------

module Control.Category.Comonad.Reader where

import Prelude hiding (Functor, fmap, (.), id, fst, snd)
import Control.Category
import Control.Category.Functor
import Control.Category.Morphism
import Control.Category.Cartesian
import Control.Category.Comonad
import Control.Category.Bifunctor
import Control.Category.Bifunctor.Associative
import Control.Category.Functor.Pointed
import Control.Category.Hask

newtype ReaderW e k a = ReaderW (Prod k a e)

class Cartesian k => HasReaderW k where
	readerW :: k (Prod k a e) (ReaderW e k a)
	unreaderW :: k (ReaderW e k a) (Prod k a e)
	
instance HasReaderW (->) where
	readerW = ReaderW
	unreaderW (ReaderW x) = x

--instance HasReaderW k => Functor (ReaderW e k) k k where
--	map = mapComonad 

--instance HasReaderW k => Copointed (ReaderW e k) k where
--	extract = fst . unreaderW

--instance HasReaderW k => Comonad (ReaderW e k) k where
--	extend k = readerW . (k &&& snd) . unreaderW
--	duplicate = readerW .  coassociate . second diag . unreaderW
