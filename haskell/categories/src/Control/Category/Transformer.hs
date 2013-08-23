-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Transformer
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (class-associated types)
--
-------------------------------------------------------------------------------------------

module Control.Category.Transformer 
	( CategoryTransformer(lift)
	) where

import Control.Category

class (Category k, Category (f k)) => CategoryTransformer f k where 
	lift :: k b c -> f k b c
