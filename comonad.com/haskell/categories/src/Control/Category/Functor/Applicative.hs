-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Functor.Applicative
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable
--
-- A strong lax (co)monoidal functor 
-------------------------------------------------------------------------------------------

module Control.Category.Functor.Applicative where

import Prelude hiding (Functor,return,curry,uncurry)
import Control.Category.Functor
import Control.Category.Functor.Pointed
import Control.Category.Cartesian
import Control.Category.Cartesian.Closed

class (CCC c, Cartesian d, Functor f c d) => Applicative f c d where
	ap :: d (Prod d (f (Exp c a b)) (f a)) (f b)

apc :: (CCC d, Applicative f c d) => d (f (Exp c a b)) (Exp d (f a) (f b))
apc = curry ap

class (CoCCC c, CoCartesian d, Functor f c d) => Coapplicative f c d where
	coap :: d (f b) (Sum d (f (Coexp c a b)) (f a)) 

coapc :: (CoCCC d, Coapplicative f c d) => d (Coexp d (f a) (f b)) (f (Coexp c a b))
coapc = cocurry coap
