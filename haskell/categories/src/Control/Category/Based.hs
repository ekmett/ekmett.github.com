-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Based
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (class-associated types)
--
-- This is the notion of a pointed category from topology. Maps of pointed spaces that 
-- preserve base points are called based maps. Since the term 'Pointed' 'Functor' is already
-- overloaded and 'Based' is already used in this context, this name has been used 
-- throughout. See <http://en.wikipedia.org/wiki/Pointed_space>.
-------------------------------------------------------------------------------------------

module Control.Category.Based where

import Prelude hiding (Functor, map)
import qualified Prelude

import Control.Category
import Control.Category.Functor

-- | any category with initial or terminal objects could single one out as a base
class Category k => Based k where
#ifndef __HADDOCK__
	type Base k :: *
#endif
	base :: k (Base k) (Base k)

-- | a based functor preserves base points. A based endofunctor works within a given
-- category so that we can witness this isomorphism in the form of a natural transformation
-- in the category.
class (Based k, Functor f k k) => BasedEndofunctor f k where
	extractBase :: k (f (Base k)) (Base k)
	returnBase  :: k (Base k) (f (Base k))

{-# RULES
	"extractBase/returnBase" 	extractBase . returnBase = id
	"returnBase/extractBase" 	returnBase . extractBase = id
 #-}
