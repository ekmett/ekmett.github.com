-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Arrow.Dual
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (class-associated types)
--
-------------------------------------------------------------------------------------------
module Control.Category.Arrow.Dual where

import Prelude hiding (Functor, map, (.), id, fst, snd, curry, uncurry)

import Control.Category
import Control.Category.Cartesian

-- Contravariant arrow (which is not a coarrow, which would be the same thing as an arrow)
class (
	CoCartesian k
#ifndef __HADDOCK__
	, Sum k ~ (,)
#endif
      ) => DualArrow k where
	dualarr :: (a -> b) -> k b a
