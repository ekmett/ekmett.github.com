{-# GHC_OPTIONS -fglasgow-exts #-}
{-# GHC_OPTIONS -fth #-}
{-# GHC_OPTIONS -fallow-undecidable-instances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Type.Binary
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
-- and a b in {T,F} such that forall n' >= n in N, x_i = b.
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
-- TODO: @TDivMod, TImplies, TGCD, TBit, TComplementBit, TSetBit@
----------------------------------------------------------------------------

module Data.Type.Binary 
	( module Data.Type.Binary.Internals
	, module Data.Type.Boolean
	, module Data.Type.Ord
	, module Data.Type.Binary.TH
	) where

import Data.Type.Boolean
import Data.Type.Ord
import Data.Type.Binary.Internals
import Data.Type.Binary.TH
