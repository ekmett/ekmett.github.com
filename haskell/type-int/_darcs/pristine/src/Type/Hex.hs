{-# GHC_OPTIONS -fglasgow-exts #-}
{-# GHC_OPTIONS -fth #-}
{-# GHC_OPTIONS -fallow-undecidable-instances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Type.Hex
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

module Type.Hex (
	THex, fromTHex,		   	   -- Infinite precision binary
	TIsZero, TIsPositive, TIsNegative, -- Trichotomy
	tIsZero, tIsPositive, tIsNegative,
	LSN, tLSN,
	TNeg, tNeg,
	TSucc, tSucc, tPred,
	TAdd, tAdd, tSub,
	TMul, tMul,
	TPow, tPow,
--	TShift, tShift,
	TNF, tNF,
	THexBinary,

--	TGetBit, tGetBit,
--	TSetBit, tSetBit,
--	TUnSetBit, tUnSetBit,
--	TChangeBit, tChangeBit,
--	TComplementBit, tComplementBit,
--	TCountBits, tCountBits,
	-- re-exported from Type.Boolean
	T, F,
--	TNot, tNot,
--	TAnd, tAnd,
--	TOr, tOr,
--	TXOr, tXOr,
--	TImplies, tImplies,
	-- re-exported from Type.Ord
	TEq, tEq,
	TLt, tLt,
	TLe, tLe,
	TGt, tGt,
	TGe, tGe,
	-- re-exported from Type.Hex.Stage*
	hexE,
	hexT
) where

import Type.Boolean
import Type.Ord
import Type.Hex.Stage1
import Type.Hex.Stage2
import Type.Hex.Stage3
