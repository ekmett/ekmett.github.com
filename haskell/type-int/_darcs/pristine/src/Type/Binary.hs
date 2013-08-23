{-# GHC_OPTIONS -fglasgow-exts #-}
{-# GHC_OPTIONS -fth #-}
{-# GHC_OPTIONS -fallow-undecidable-instances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Type.Binary
-- Copyright   :  (C) 2006 Edward Kmett
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (MPTC, FD, TH, undecidable instances, and missing constructors)
--
-- Simple type-level binary numbers, positive and negative with infinite
-- precision. This forms a nice commutative ring with multiplicative identity
-- like we would expect from a representation for Z.
--
-- The numbers are represented as a Boolean Ring over a countable set of
-- variables, in which for every element in the set there exists an n in N
-- and a b in {T,F} such that for all n'>=n in N, x_i = b.
--
-- For uniqueness we always choose the least such n when representing numbers
-- this allows us to run most computations backwards. When we can't, and such
-- a fundep would be implied, we obtain it by combining semi-operations that
-- together yield the appropriate class fundep list.
--
-- The goal here was to pull together many of the good ideas I've seen from
-- various sources, and sprinkle a two's complement negative number
-- representation on top.
--
-- Reuses T and F from the Type.Boolean as the infinite tail of the 2s
-- complement binary number. I'm particularly fond of the symmetry exhibited
-- in the full adder.
--
-- TODO: TDivMod, TImplies, TGCD, TBit, TComplementBit, TSetBit
----------------------------------------------------------------------------

module Type.Binary (
	O, I,
	TBinary, fromTBinary,
	TIsZero, TIsPositive, TIsNegative,
	tIsZero, tIsPositive, tIsNegative,
	LSB, tLSB,
	TNeg, tNeg,
	TSucc, tSucc, tPred,
	TAdd, tAdd, tSub,
	TMul, tMul,
	TPow, tPow,
	TShift, tShift,
	TNF, tNF,
	TAbs, tAbs,
	TGetBit, tGetBit,
	TSetBit, tSetBit,
	TUnSetBit, tUnSetBit,
	TChangeBit, tChangeBit,
	TComplementBit, tComplementBit,
	TCountBits, tCountBits,
	-- re-exported from Type.Boolean
	T, tT,
	F, tF,
	TNot, tNot,
	TAnd, tAnd,
	TOr, tOr,
	TXOr, tXOr,
	TImplies, tImplies,
	-- re-exported from Type.Ord
	TEq, tEq,
	TLt, tLt,
	TLe, tLe,
	TGt, tGt,
	TGe, tGe,
	-- re-exported from Type.Binary.TH
	binaryE,
	binaryT
) where

import Type.Boolean
import Type.Ord
import Type.Binary.Internals
import Type.Binary.TH
