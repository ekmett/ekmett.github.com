-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Loop
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (class-associated types)
--
-------------------------------------------------------------------------------------------

module Control.Category.Loop where

import Control.Category.Cartesian

-- derivative of ArrowLoop, this seems like the wrong abstraction
class Cartesian k => Loop k where
	loop :: k (Prod k a c) (Prod k b c) -> k a b

