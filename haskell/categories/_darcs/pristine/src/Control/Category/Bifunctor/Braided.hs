-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Bifunctor.Braided
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (mptcs)
--
-------------------------------------------------------------------------------------------
module Control.Category.Bifunctor.Braided where

import Control.Category.Bifunctor

-- * Braided (co)Monoidal Categories

{- | A braided (co)(monoidal or associative) category can commute the arguments of its bi-endofunctor. Obeys the laws:

> idr . braid = idl 
> idl . braid = idr 
> braid . coidr = coidl 
> braid . coidl = coidr 
> associate . braid . associate = second braid . associate . first braid 
> coassociate . braid . coassociate = first braid . coassociate . second braid 

-}

class Bifunctor p k k k => Braided p k where
	braid :: k (p a b) (p b a)

{-# RULES 
	"idr/braid" 			idr . braid = idl
	"idl/braid" 			idl . braid = idr
	"braid/coidr" 			braid . coidr = coidl
	"braid/coidl" 			braid . coidl = coidr
	"braid/associate/braid" 	second braid . associate . first braid = associate . braid . associate
	"braid/coassociate/braid" 	first braid . coassociate . second braid = coassociate . braid . coassociate
 #-}

-- * Symmetric (co)Monoidal Categories

{- |
If we have a symmetric (co)'Monoidal' category, you get the additional law:

> swap . swap = id
 -}
class Braided p k => Symmetric p k

swap :: Symmetric p k => k (p a b) (p b a)
swap = braid

{-# RULES
	"swap/swap" swap . swap = id
 #-}
