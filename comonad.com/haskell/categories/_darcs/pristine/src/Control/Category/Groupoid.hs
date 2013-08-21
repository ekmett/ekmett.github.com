-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Groupoid
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD3
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (functional-dependencies)
--
-- A groupoid is a category in which each morphism is invertible
-------------------------------------------------------------------------------------------
module Control.Category.Groupoid where

import Control.Category

class Category k => Groupoid k where
	inv :: k a b -> k b a

{-# RULES 
	"inv/inv" inv . inv = id
 #-}
