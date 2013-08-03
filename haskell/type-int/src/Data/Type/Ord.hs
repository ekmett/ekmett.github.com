{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Type.Ord
-- Copyright   :  (C) 2006-2007 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (FD and MPTC, undecidable-instances)
--
-- Simple equality and ordering for types.
-- Extended to include common usage cases.
--
-- Instances should all really be decidable.
----------------------------------------------------------------------------

module Data.Type.Ord
	( TEq, tEq
	, TLt, tLt
	, TGe, tGe -- closed, extend via TEq/TLt
	, TLe, tLe -- closed, extend via TEq/TLt
	, TGt, tGt -- closed, extend via TEq/TLt
) where

import Data.Type.Boolean

data Closure
class Closed a | -> a
instance Closed Closure

-- two open classes
class TBool b => TEq x y b | x y -> b
instance TEq T T T
instance TEq T F F
instance TEq F T F
instance TEq F F T
tEq :: TEq x y b => x -> y -> b;
tEq = undefined

class TBool b => TLt x y b | x y -> b
tLt :: TLt x y b => x -> y -> b
tLt = undefined

class TBool b => TCGe c x y b | x y -> b, x y b -> c
instance (TBool b', TLt x y b, TNot b b') => TCGe Closure x y b'
class TCGe Closure x y b => TGe x y b | x y -> b
instance (TBool b', TLt x y b, TNot b b') => TGe x y b'
tGe :: TGe x y b => x -> y -> b
tGe = undefined

class TBool b => TCLe c x y b | x y -> b, x y b -> c
instance (TBool b'', TEq x y b, TLt x y b', TOr b b' b'') => TCLe Closure x y b''
class TBool b => TLe x y b | x y -> b
instance (TBool b'', TEq x y b, TLt x y b', TOr b b' b'') => TLe x y b''
tLe :: TGt x y b => x -> y -> b
tLe = undefined

class TBool b => TCGt c x y b | x y -> b, x y b -> c
instance (TBool b', TLe x y b, TNot b b') => TCGt Closure x y b'
class TBool b => TGt x y b | x y -> b
instance (TBool b', TLe x y b, TNot b b') => TGt x y b'
tGt :: TGt x y b => x -> y -> b
tGt = undefined
