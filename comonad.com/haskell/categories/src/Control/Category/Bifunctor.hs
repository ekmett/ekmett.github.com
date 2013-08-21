-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Bifunctor
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD3
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (functional-dependencies)
--
-------------------------------------------------------------------------------------------
module Control.Category.Bifunctor where

import Prelude hiding (id)
import Control.Category

-- | A covariant 'Bifunctor' @f :: k1 * k2 -> k3@
-- | NB: Bifunctors with contravariant arguments can be represented using the 'Dual' 'Category' in that argument
class (Category k1, Category k2, Category k3) => Bifunctor f k1 k2 k3 | f k1 -> k2 k3, f k2 -> k1 k3, f k3 -> k1 k2 where
	-- | Generalizes the arrow combinators @***@ and @+++@ to any covariant bifunctor
	bimap :: k1 a c -> k2 b d -> k3 (f a b) (f c d)
	first :: k1 a b -> k3 (f a c) (f b c)
	second :: k2 a b -> k3 (f c a) (f c b)

	first f = bimap f id
	second g = bimap id g
