{-# GHC_OPTIONS -fglasgow-exts #-}
{-# GHC_OPTIONS -fth #-}
{-# GHC_OPTIONS -fallow-undecidable-instances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Type.Hex
-- Copyright   :  (C) 2006 Edward Kmett
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (MPTC, FD, TH, undecidable instances, missing constructors)
--
-- Type-level hexadecimal numbers, positive and negative with infinite
-- precision. Should work out to about 2^72 without changing the default
-- context length limit in GHC.
--
-- TODO: TDivMod, TImplies, TGCD, T*Bit, and the boolean operators
----------------------------------------------------------------------------

module Data.Type.Hex 
	( hexE, hexT
-- 	  THex, fromTHex		   	 
	, TIsZero, TIsPositive, TIsNegative
	, tIsZero, tIsPositive, tIsNegative
-- 	, LSN, tLSN
	, TNeg, tNeg
--	, TSucc
	, tSucc, tPred
	, TAdd, tAdd, tSub
	, TMul, tMul
	, TPow, tPow
--	, TShift, tShift
	, TNF, tNF
	, THexBinary
--	, TGetBit, tGetBit
--	, TSetBit, tSetBit
--	, TUnSetBit, tUnSetBit
--	, TChangeBit, tChangeBit
--	, TComplementBit, tComplementBit
--	, TCountBits, tCountBits
	, module Data.Type.Boolean
	, module Data.Type.Ord
	, module Data.Type.Hex.Stage1
	, module Data.Type.Hex.Stage2
	) where

import Data.Type.Boolean
import Data.Type.Ord
import Data.Type.Hex.Stage1
import Data.Type.Hex.Stage2
import Data.Type.Hex.Stage3
