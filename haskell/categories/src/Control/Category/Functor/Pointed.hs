-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Functor.Pointed
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (class-associated types)
--
-- Defines the notion of a 'Pointed' (and 'Copointed') functor in the sense of 
-- Lenisa, Power and Watanabe''s Distributivity for Endofunctors, Pointed and Co-Pointed 
-- Endofunctors, Monads and Comonads <http://citeseer.ist.psu.edu/301388.html>
--
-- A simple wrapper @newtype@ is basically just a 'Pointed' 'Copointed' 'Functor'. When 
-- a functor is both pointed and copointed then:
-- 	
-- > extract . return = id
-- > return . extract = id
--
-- NB: There is another notion of a pointed functor that can be derived from pointed 
-- spaces in topology that is *not* represented by this type. See 'Based' in 
-- "Control.Category.Based" for that definition.
-------------------------------------------------------------------------------------------

module Control.Category.Functor.Pointed where

import Prelude hiding (Functor,return,id,(.))
import Control.Category.Functor

class Functor f k k => Pointed f k where
	return :: k a (f a)

class Functor f k k => Copointed f k where
	extract :: k (f a) a

{-# RULES
	"extract/return" extract . return = id
	"return/extract" return . extract = id
 #-}
