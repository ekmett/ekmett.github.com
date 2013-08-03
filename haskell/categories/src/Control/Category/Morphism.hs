-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Morphism
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (class-associated types)
--
-------------------------------------------------------------------------------------------

module Control.Category.Morphism where

import Control.Category

-- | Isomorphism
class Iso k a b where
	iso :: k a b
	uniso :: k b a

{-# RULES
	"iso/uniso" iso . uniso = id
	"uniso/iso" uniso . iso = id
 #-}

class Iso k a b => Canonical k a b | k a -> b where
