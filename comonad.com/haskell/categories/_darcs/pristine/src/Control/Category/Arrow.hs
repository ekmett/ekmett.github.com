------------------------------------------------------------------------------------------- -- |
-- Module	: Control.Category.Arrow
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (class-associated types)
--
-------------------------------------------------------------------------------------------
module Control.Category.Arrow where

import Prelude hiding (Functor, map, (.), id, fst, snd, curry, uncurry)

import Control.Category
import Control.Category.Cartesian

-- Arrows have finite products, a fixed notion for Prod k and can inject functions from the metalanguage
class (
	Cartesian k
#ifndef __HADDOCK__
	, Prod k ~ (,)
#endif
      ) => Arrow k where
	arr :: (a -> b) -> k a b

