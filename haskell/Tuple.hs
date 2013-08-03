{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

-- |
-- Module      : Data.Adaptive.Tuple
-- Copyright   : (c) Don Stewart 2009
-- License     : BSD-style
-- Maintainer  : dons@galois.com
-- Stability   : experimental
-- 
-- Self optimzing pair types.
--
-- This library statically adapts the polymorphic container
-- representation of tuples to specific, more efficient representations,
-- when instantiated with particular monomorphic types. It does this via
-- an associated more efficient type type for each pair of elements you
-- wish to store in your container.
--
-- That is, instead of representing '(Int,Char)' as:
--
-- >            (,)
-- >           /   \
-- >       I# 3#   C# x#
--
-- A self-optimizing pair will unpack the constructors, yielding this
-- type representation:
--
-- >       PairIntChar 3# x#
--
-- Saving two indirections. The resulting structure should be both more
-- time and space efficient than the generic polymorphic container it is
-- derived from. For example, adaptive pairs use 8 bytes to store an Int and
-- Char pair, while a lazy pair uses 24 bytes.
--
-- > > Prelude Size> unsafeSizeof ((42, 'x') :: (Int,Char))
-- > > 24
--
-- > Prelude Size> unsafeSizeof (pair 42 'x' :: Pair Int Char)
-- > > 8
--
-- You can inspect the size and layout of your adaptive structures using two scripts, 
-- one for measuring the size of a closure, described in
-- <http://ghcmutterings.wordpress.com/2009/02/>, and vacuum-cairo, for rendering 
-- the heap structure explicitly
-- <http://hackage.haskell.org/cgi-bin/hackage-scripts/package/vacuum-cairo>
--
-- Types that instantiate the 'Adapt' class will self-adapt this way.
--
-- Self adaptive polymorphic containers are able to unpack their
-- components, something not possible with, for example, strict
-- polymorphic containers.
--

module Data.Adaptive.Tuple where

import Prelude hiding (curry, uncurry, fst, snd)
import qualified Prelude

import Data.Int
import Data.Word

data PUnit = PUnit
data PBool = PBool {-# UNPACK #-}!Int {-# UNPACK #-}!Int
data PairIntInt = PairIntInt {-# UNPACK #-}!Int {-# UNPACK #-}!Int
data PairIntInteger = PairIntInteger {-# UNPACK #-}!Int {-# UNPACK #-}!Integer
data PairIntInt8 = PairIntInt8 {-# UNPACK #-}!Int {-# UNPACK #-}!Int8
data PairIntInt16 = PairIntInt16 {-# UNPACK #-}!Int {-# UNPACK #-}!Int16
data PairIntInt32 = PairIntInt32 {-# UNPACK #-}!Int {-# UNPACK #-}!Int32
data PairIntInt64 = PairIntInt64 {-# UNPACK #-}!Int {-# UNPACK #-}!Int64
data PairIntWord = PairIntWord {-# UNPACK #-}!Int {-# UNPACK #-}!Word
data PairIntWord8 = PairIntWord8 {-# UNPACK #-}!Int {-# UNPACK #-}!Word8
data PairIntWord16 = PairIntWord16 {-# UNPACK #-}!Int {-# UNPACK #-}!Word16
data PairIntWord32 = PairIntWord32 {-# UNPACK #-}!Int {-# UNPACK #-}!Word32
data PairIntWord64 = PairIntWord64 {-# UNPACK #-}!Int {-# UNPACK #-}!Word64
data PairIntDouble = PairIntDouble {-# UNPACK #-}!Int {-# UNPACK #-}!Double
data PairIntFloat = PairIntFloat {-# UNPACK #-}!Int {-# UNPACK #-}!Float
data PairIntChar = PairIntChar {-# UNPACK #-}!Int {-# UNPACK #-}!Char
data PairIntegerInt = PairIntegerInt {-# UNPACK #-}!Integer {-# UNPACK #-}!Int
data PairIntegerInteger = PairIntegerInteger {-# UNPACK #-}!Integer {-# UNPACK #-}!Integer
data PairIntegerInt8 = PairIntegerInt8 {-# UNPACK #-}!Integer {-# UNPACK #-}!Int8
data PairIntegerInt16 = PairIntegerInt16 {-# UNPACK #-}!Integer {-# UNPACK #-}!Int16
data PairIntegerInt32 = PairIntegerInt32 {-# UNPACK #-}!Integer {-# UNPACK #-}!Int32
data PairIntegerInt64 = PairIntegerInt64 {-# UNPACK #-}!Integer {-# UNPACK #-}!Int64
data PairIntegerWord = PairIntegerWord {-# UNPACK #-}!Integer {-# UNPACK #-}!Word
data PairIntegerWord8 = PairIntegerWord8 {-# UNPACK #-}!Integer {-# UNPACK #-}!Word8
data PairIntegerWord16 = PairIntegerWord16 {-# UNPACK #-}!Integer {-# UNPACK #-}!Word16
data PairIntegerWord32 = PairIntegerWord32 {-# UNPACK #-}!Integer {-# UNPACK #-}!Word32
data PairIntegerWord64 = PairIntegerWord64 {-# UNPACK #-}!Integer {-# UNPACK #-}!Word64
data PairIntegerDouble = PairIntegerDouble {-# UNPACK #-}!Integer {-# UNPACK #-}!Double
data PairIntegerFloat = PairIntegerFloat {-# UNPACK #-}!Integer {-# UNPACK #-}!Float
data PairIntegerChar = PairIntegerChar {-# UNPACK #-}!Integer {-# UNPACK #-}!Char
data PairInt8Int = PairInt8Int {-# UNPACK #-}!Int8 {-# UNPACK #-}!Int
data PairInt8Integer = PairInt8Integer {-# UNPACK #-}!Int8 {-# UNPACK #-}!Integer
data PairInt8Int8 = PairInt8Int8 {-# UNPACK #-}!Int8 {-# UNPACK #-}!Int8
data PairInt8Int16 = PairInt8Int16 {-# UNPACK #-}!Int8 {-# UNPACK #-}!Int16
data PairInt8Int32 = PairInt8Int32 {-# UNPACK #-}!Int8 {-# UNPACK #-}!Int32
data PairInt8Int64 = PairInt8Int64 {-# UNPACK #-}!Int8 {-# UNPACK #-}!Int64
data PairInt8Word = PairInt8Word {-# UNPACK #-}!Int8 {-# UNPACK #-}!Word
data PairInt8Word8 = PairInt8Word8 {-# UNPACK #-}!Int8 {-# UNPACK #-}!Word8
data PairInt8Word16 = PairInt8Word16 {-# UNPACK #-}!Int8 {-# UNPACK #-}!Word16
data PairInt8Word32 = PairInt8Word32 {-# UNPACK #-}!Int8 {-# UNPACK #-}!Word32
data PairInt8Word64 = PairInt8Word64 {-# UNPACK #-}!Int8 {-# UNPACK #-}!Word64
data PairInt8Double = PairInt8Double {-# UNPACK #-}!Int8 {-# UNPACK #-}!Double
data PairInt8Float = PairInt8Float {-# UNPACK #-}!Int8 {-# UNPACK #-}!Float
data PairInt8Char = PairInt8Char {-# UNPACK #-}!Int8 {-# UNPACK #-}!Char
data PairInt16Int = PairInt16Int {-# UNPACK #-}!Int16 {-# UNPACK #-}!Int
data PairInt16Integer = PairInt16Integer {-# UNPACK #-}!Int16 {-# UNPACK #-}!Integer
data PairInt16Int8 = PairInt16Int8 {-# UNPACK #-}!Int16 {-# UNPACK #-}!Int8
data PairInt16Int16 = PairInt16Int16 {-# UNPACK #-}!Int16 {-# UNPACK #-}!Int16
data PairInt16Int32 = PairInt16Int32 {-# UNPACK #-}!Int16 {-# UNPACK #-}!Int32
data PairInt16Int64 = PairInt16Int64 {-# UNPACK #-}!Int16 {-# UNPACK #-}!Int64
data PairInt16Word = PairInt16Word {-# UNPACK #-}!Int16 {-# UNPACK #-}!Word
data PairInt16Word8 = PairInt16Word8 {-# UNPACK #-}!Int16 {-# UNPACK #-}!Word8
data PairInt16Word16 = PairInt16Word16 {-# UNPACK #-}!Int16 {-# UNPACK #-}!Word16
data PairInt16Word32 = PairInt16Word32 {-# UNPACK #-}!Int16 {-# UNPACK #-}!Word32
data PairInt16Word64 = PairInt16Word64 {-# UNPACK #-}!Int16 {-# UNPACK #-}!Word64
data PairInt16Double = PairInt16Double {-# UNPACK #-}!Int16 {-# UNPACK #-}!Double
data PairInt16Float = PairInt16Float {-# UNPACK #-}!Int16 {-# UNPACK #-}!Float
data PairInt16Char = PairInt16Char {-# UNPACK #-}!Int16 {-# UNPACK #-}!Char
data PairInt32Int = PairInt32Int {-# UNPACK #-}!Int32 {-# UNPACK #-}!Int
data PairInt32Integer = PairInt32Integer {-# UNPACK #-}!Int32 {-# UNPACK #-}!Integer
data PairInt32Int8 = PairInt32Int8 {-# UNPACK #-}!Int32 {-# UNPACK #-}!Int8
data PairInt32Int16 = PairInt32Int16 {-# UNPACK #-}!Int32 {-# UNPACK #-}!Int16
data PairInt32Int32 = PairInt32Int32 {-# UNPACK #-}!Int32 {-# UNPACK #-}!Int32
data PairInt32Int64 = PairInt32Int64 {-# UNPACK #-}!Int32 {-# UNPACK #-}!Int64
data PairInt32Word = PairInt32Word {-# UNPACK #-}!Int32 {-# UNPACK #-}!Word
data PairInt32Word8 = PairInt32Word8 {-# UNPACK #-}!Int32 {-# UNPACK #-}!Word8
data PairInt32Word16 = PairInt32Word16 {-# UNPACK #-}!Int32 {-# UNPACK #-}!Word16
data PairInt32Word32 = PairInt32Word32 {-# UNPACK #-}!Int32 {-# UNPACK #-}!Word32
data PairInt32Word64 = PairInt32Word64 {-# UNPACK #-}!Int32 {-# UNPACK #-}!Word64
data PairInt32Double = PairInt32Double {-# UNPACK #-}!Int32 {-# UNPACK #-}!Double
data PairInt32Float = PairInt32Float {-# UNPACK #-}!Int32 {-# UNPACK #-}!Float
data PairInt32Char = PairInt32Char {-# UNPACK #-}!Int32 {-# UNPACK #-}!Char
data PairInt64Int = PairInt64Int {-# UNPACK #-}!Int64 {-# UNPACK #-}!Int
data PairInt64Integer = PairInt64Integer {-# UNPACK #-}!Int64 {-# UNPACK #-}!Integer
data PairInt64Int8 = PairInt64Int8 {-# UNPACK #-}!Int64 {-# UNPACK #-}!Int8
data PairInt64Int16 = PairInt64Int16 {-# UNPACK #-}!Int64 {-# UNPACK #-}!Int16
data PairInt64Int32 = PairInt64Int32 {-# UNPACK #-}!Int64 {-# UNPACK #-}!Int32
data PairInt64Int64 = PairInt64Int64 {-# UNPACK #-}!Int64 {-# UNPACK #-}!Int64
data PairInt64Word = PairInt64Word {-# UNPACK #-}!Int64 {-# UNPACK #-}!Word
data PairInt64Word8 = PairInt64Word8 {-# UNPACK #-}!Int64 {-# UNPACK #-}!Word8
data PairInt64Word16 = PairInt64Word16 {-# UNPACK #-}!Int64 {-# UNPACK #-}!Word16
data PairInt64Word32 = PairInt64Word32 {-# UNPACK #-}!Int64 {-# UNPACK #-}!Word32
data PairInt64Word64 = PairInt64Word64 {-# UNPACK #-}!Int64 {-# UNPACK #-}!Word64
data PairInt64Double = PairInt64Double {-# UNPACK #-}!Int64 {-# UNPACK #-}!Double
data PairInt64Float = PairInt64Float {-# UNPACK #-}!Int64 {-# UNPACK #-}!Float
data PairInt64Char = PairInt64Char {-# UNPACK #-}!Int64 {-# UNPACK #-}!Char
data PairWordInt = PairWordInt {-# UNPACK #-}!Word {-# UNPACK #-}!Int
data PairWordInteger = PairWordInteger {-# UNPACK #-}!Word {-# UNPACK #-}!Integer
data PairWordInt8 = PairWordInt8 {-# UNPACK #-}!Word {-# UNPACK #-}!Int8
data PairWordInt16 = PairWordInt16 {-# UNPACK #-}!Word {-# UNPACK #-}!Int16
data PairWordInt32 = PairWordInt32 {-# UNPACK #-}!Word {-# UNPACK #-}!Int32
data PairWordInt64 = PairWordInt64 {-# UNPACK #-}!Word {-# UNPACK #-}!Int64
data PairWordWord = PairWordWord {-# UNPACK #-}!Word {-# UNPACK #-}!Word
data PairWordWord8 = PairWordWord8 {-# UNPACK #-}!Word {-# UNPACK #-}!Word8
data PairWordWord16 = PairWordWord16 {-# UNPACK #-}!Word {-# UNPACK #-}!Word16
data PairWordWord32 = PairWordWord32 {-# UNPACK #-}!Word {-# UNPACK #-}!Word32
data PairWordWord64 = PairWordWord64 {-# UNPACK #-}!Word {-# UNPACK #-}!Word64
data PairWordDouble = PairWordDouble {-# UNPACK #-}!Word {-# UNPACK #-}!Double
data PairWordFloat = PairWordFloat {-# UNPACK #-}!Word {-# UNPACK #-}!Float
data PairWordChar = PairWordChar {-# UNPACK #-}!Word {-# UNPACK #-}!Char
data PairWord8Int = PairWord8Int {-# UNPACK #-}!Word8 {-# UNPACK #-}!Int
data PairWord8Integer = PairWord8Integer {-# UNPACK #-}!Word8 {-# UNPACK #-}!Integer
data PairWord8Int8 = PairWord8Int8 {-# UNPACK #-}!Word8 {-# UNPACK #-}!Int8
data PairWord8Int16 = PairWord8Int16 {-# UNPACK #-}!Word8 {-# UNPACK #-}!Int16
data PairWord8Int32 = PairWord8Int32 {-# UNPACK #-}!Word8 {-# UNPACK #-}!Int32
data PairWord8Int64 = PairWord8Int64 {-# UNPACK #-}!Word8 {-# UNPACK #-}!Int64
data PairWord8Word = PairWord8Word {-# UNPACK #-}!Word8 {-# UNPACK #-}!Word
data PairWord8Word8 = PairWord8Word8 {-# UNPACK #-}!Word8 {-# UNPACK #-}!Word8
data PairWord8Word16 = PairWord8Word16 {-# UNPACK #-}!Word8 {-# UNPACK #-}!Word16
data PairWord8Word32 = PairWord8Word32 {-# UNPACK #-}!Word8 {-# UNPACK #-}!Word32
data PairWord8Word64 = PairWord8Word64 {-# UNPACK #-}!Word8 {-# UNPACK #-}!Word64
data PairWord8Double = PairWord8Double {-# UNPACK #-}!Word8 {-# UNPACK #-}!Double
data PairWord8Float = PairWord8Float {-# UNPACK #-}!Word8 {-# UNPACK #-}!Float
data PairWord8Char = PairWord8Char {-# UNPACK #-}!Word8 {-# UNPACK #-}!Char
data PairWord16Int = PairWord16Int {-# UNPACK #-}!Word16 {-# UNPACK #-}!Int
data PairWord16Integer = PairWord16Integer {-# UNPACK #-}!Word16 {-# UNPACK #-}!Integer
data PairWord16Int8 = PairWord16Int8 {-# UNPACK #-}!Word16 {-# UNPACK #-}!Int8
data PairWord16Int16 = PairWord16Int16 {-# UNPACK #-}!Word16 {-# UNPACK #-}!Int16
data PairWord16Int32 = PairWord16Int32 {-# UNPACK #-}!Word16 {-# UNPACK #-}!Int32
data PairWord16Int64 = PairWord16Int64 {-# UNPACK #-}!Word16 {-# UNPACK #-}!Int64
data PairWord16Word = PairWord16Word {-# UNPACK #-}!Word16 {-# UNPACK #-}!Word
data PairWord16Word8 = PairWord16Word8 {-# UNPACK #-}!Word16 {-# UNPACK #-}!Word8
data PairWord16Word16 = PairWord16Word16 {-# UNPACK #-}!Word16 {-# UNPACK #-}!Word16
data PairWord16Word32 = PairWord16Word32 {-# UNPACK #-}!Word16 {-# UNPACK #-}!Word32
data PairWord16Word64 = PairWord16Word64 {-# UNPACK #-}!Word16 {-# UNPACK #-}!Word64
data PairWord16Double = PairWord16Double {-# UNPACK #-}!Word16 {-# UNPACK #-}!Double
data PairWord16Float = PairWord16Float {-# UNPACK #-}!Word16 {-# UNPACK #-}!Float
data PairWord16Char = PairWord16Char {-# UNPACK #-}!Word16 {-# UNPACK #-}!Char
data PairWord32Int = PairWord32Int {-# UNPACK #-}!Word32 {-# UNPACK #-}!Int
data PairWord32Integer = PairWord32Integer {-# UNPACK #-}!Word32 {-# UNPACK #-}!Integer
data PairWord32Int8 = PairWord32Int8 {-# UNPACK #-}!Word32 {-# UNPACK #-}!Int8
data PairWord32Int16 = PairWord32Int16 {-# UNPACK #-}!Word32 {-# UNPACK #-}!Int16
data PairWord32Int32 = PairWord32Int32 {-# UNPACK #-}!Word32 {-# UNPACK #-}!Int32
data PairWord32Int64 = PairWord32Int64 {-# UNPACK #-}!Word32 {-# UNPACK #-}!Int64
data PairWord32Word = PairWord32Word {-# UNPACK #-}!Word32 {-# UNPACK #-}!Word
data PairWord32Word8 = PairWord32Word8 {-# UNPACK #-}!Word32 {-# UNPACK #-}!Word8
data PairWord32Word16 = PairWord32Word16 {-# UNPACK #-}!Word32 {-# UNPACK #-}!Word16
data PairWord32Word32 = PairWord32Word32 {-# UNPACK #-}!Word32 {-# UNPACK #-}!Word32
data PairWord32Word64 = PairWord32Word64 {-# UNPACK #-}!Word32 {-# UNPACK #-}!Word64
data PairWord32Double = PairWord32Double {-# UNPACK #-}!Word32 {-# UNPACK #-}!Double
data PairWord32Float = PairWord32Float {-# UNPACK #-}!Word32 {-# UNPACK #-}!Float
data PairWord32Char = PairWord32Char {-# UNPACK #-}!Word32 {-# UNPACK #-}!Char
data PairWord64Int = PairWord64Int {-# UNPACK #-}!Word64 {-# UNPACK #-}!Int
data PairWord64Integer = PairWord64Integer {-# UNPACK #-}!Word64 {-# UNPACK #-}!Integer
data PairWord64Int8 = PairWord64Int8 {-# UNPACK #-}!Word64 {-# UNPACK #-}!Int8
data PairWord64Int16 = PairWord64Int16 {-# UNPACK #-}!Word64 {-# UNPACK #-}!Int16
data PairWord64Int32 = PairWord64Int32 {-# UNPACK #-}!Word64 {-# UNPACK #-}!Int32
data PairWord64Int64 = PairWord64Int64 {-# UNPACK #-}!Word64 {-# UNPACK #-}!Int64
data PairWord64Word = PairWord64Word {-# UNPACK #-}!Word64 {-# UNPACK #-}!Word
data PairWord64Word8 = PairWord64Word8 {-# UNPACK #-}!Word64 {-# UNPACK #-}!Word8
data PairWord64Word16 = PairWord64Word16 {-# UNPACK #-}!Word64 {-# UNPACK #-}!Word16
data PairWord64Word32 = PairWord64Word32 {-# UNPACK #-}!Word64 {-# UNPACK #-}!Word32
data PairWord64Word64 = PairWord64Word64 {-# UNPACK #-}!Word64 {-# UNPACK #-}!Word64
data PairWord64Double = PairWord64Double {-# UNPACK #-}!Word64 {-# UNPACK #-}!Double
data PairWord64Float = PairWord64Float {-# UNPACK #-}!Word64 {-# UNPACK #-}!Float
data PairWord64Char = PairWord64Char {-# UNPACK #-}!Word64 {-# UNPACK #-}!Char
data PairDoubleInt = PairDoubleInt {-# UNPACK #-}!Double {-# UNPACK #-}!Int
data PairDoubleInteger = PairDoubleInteger {-# UNPACK #-}!Double {-# UNPACK #-}!Integer
data PairDoubleInt8 = PairDoubleInt8 {-# UNPACK #-}!Double {-# UNPACK #-}!Int8
data PairDoubleInt16 = PairDoubleInt16 {-# UNPACK #-}!Double {-# UNPACK #-}!Int16
data PairDoubleInt32 = PairDoubleInt32 {-# UNPACK #-}!Double {-# UNPACK #-}!Int32
data PairDoubleInt64 = PairDoubleInt64 {-# UNPACK #-}!Double {-# UNPACK #-}!Int64
data PairDoubleWord = PairDoubleWord {-# UNPACK #-}!Double {-# UNPACK #-}!Word
data PairDoubleWord8 = PairDoubleWord8 {-# UNPACK #-}!Double {-# UNPACK #-}!Word8
data PairDoubleWord16 = PairDoubleWord16 {-# UNPACK #-}!Double {-# UNPACK #-}!Word16
data PairDoubleWord32 = PairDoubleWord32 {-# UNPACK #-}!Double {-# UNPACK #-}!Word32
data PairDoubleWord64 = PairDoubleWord64 {-# UNPACK #-}!Double {-# UNPACK #-}!Word64
data PairDoubleDouble = PairDoubleDouble {-# UNPACK #-}!Double {-# UNPACK #-}!Double
data PairDoubleFloat = PairDoubleFloat {-# UNPACK #-}!Double {-# UNPACK #-}!Float
data PairDoubleChar = PairDoubleChar {-# UNPACK #-}!Double {-# UNPACK #-}!Char
data PairFloatInt = PairFloatInt {-# UNPACK #-}!Float {-# UNPACK #-}!Int
data PairFloatInteger = PairFloatInteger {-# UNPACK #-}!Float {-# UNPACK #-}!Integer
data PairFloatInt8 = PairFloatInt8 {-# UNPACK #-}!Float {-# UNPACK #-}!Int8
data PairFloatInt16 = PairFloatInt16 {-# UNPACK #-}!Float {-# UNPACK #-}!Int16
data PairFloatInt32 = PairFloatInt32 {-# UNPACK #-}!Float {-# UNPACK #-}!Int32
data PairFloatInt64 = PairFloatInt64 {-# UNPACK #-}!Float {-# UNPACK #-}!Int64
data PairFloatWord = PairFloatWord {-# UNPACK #-}!Float {-# UNPACK #-}!Word
data PairFloatWord8 = PairFloatWord8 {-# UNPACK #-}!Float {-# UNPACK #-}!Word8
data PairFloatWord16 = PairFloatWord16 {-# UNPACK #-}!Float {-# UNPACK #-}!Word16
data PairFloatWord32 = PairFloatWord32 {-# UNPACK #-}!Float {-# UNPACK #-}!Word32
data PairFloatWord64 = PairFloatWord64 {-# UNPACK #-}!Float {-# UNPACK #-}!Word64
data PairFloatDouble = PairFloatDouble {-# UNPACK #-}!Float {-# UNPACK #-}!Double
data PairFloatFloat = PairFloatFloat {-# UNPACK #-}!Float {-# UNPACK #-}!Float
data PairFloatChar = PairFloatChar {-# UNPACK #-}!Float {-# UNPACK #-}!Char
data PairCharInt = PairCharInt {-# UNPACK #-}!Char {-# UNPACK #-}!Int
data PairCharInteger = PairCharInteger {-# UNPACK #-}!Char {-# UNPACK #-}!Integer
data PairCharInt8 = PairCharInt8 {-# UNPACK #-}!Char {-# UNPACK #-}!Int8
data PairCharInt16 = PairCharInt16 {-# UNPACK #-}!Char {-# UNPACK #-}!Int16
data PairCharInt32 = PairCharInt32 {-# UNPACK #-}!Char {-# UNPACK #-}!Int32
data PairCharInt64 = PairCharInt64 {-# UNPACK #-}!Char {-# UNPACK #-}!Int64
data PairCharWord = PairCharWord {-# UNPACK #-}!Char {-# UNPACK #-}!Word
data PairCharWord8 = PairCharWord8 {-# UNPACK #-}!Char {-# UNPACK #-}!Word8
data PairCharWord16 = PairCharWord16 {-# UNPACK #-}!Char {-# UNPACK #-}!Word16
data PairCharWord32 = PairCharWord32 {-# UNPACK #-}!Char {-# UNPACK #-}!Word32
data PairCharWord64 = PairCharWord64 {-# UNPACK #-}!Char {-# UNPACK #-}!Word64
data PairCharDouble = PairCharDouble {-# UNPACK #-}!Char {-# UNPACK #-}!Double
data PairCharFloat = PairCharFloat {-# UNPACK #-}!Char {-# UNPACK #-}!Float
data PairCharChar = PairCharChar {-# UNPACK #-}!Char {-# UNPACK #-}!Char

--
-- | Representation-improving polymorphic tuples.
--
class AdaptPair a b where

  type Pair a b

  -- | Extract the first component of a pair.
  fst     :: Pair a b -> a

  -- | Extract the second component of a pair.
  snd     :: Pair a b -> b

  -- | 'curry' converts an uncurried function to a curried function.
  curry   :: (Pair a b -> c) -> a -> b -> c

------------------------------------------------------------------------

-- | Construct a new pair.
pair :: AdaptPair a b => a -> b -> Pair a b
pair = curry id

{-
-- | 'uncurry' converts a curried function to a function on pairs.
uncurry :: AdaptPair a b => (a -> b -> c) -> (Pair a b -> c)
uncurry f p =  f (fst p) (snd p)

-- | Convert an adaptive pair to a regular polymorphic tuple
fromPair :: AdaptPair a b => Pair a b -> (a, b)
fromPair = uncurry (,)
-}

-- | Convert a regular polymorphic tuple to an adaptive pair
toPair   :: AdaptPair a b => (a,b) -> Pair a b
toPair (a,b) = pair a b

------------------------------------------------------------------------
-- Methods

--
-- standalone deriving crashes here: do not attempt it.
--
-- deriving instance (Eq a, Eq b, AdaptPair a b) => Eq (Pair a b)
-- deriving instance (Ord a, Ord b, AdaptPair a b) => Ord (Pair a b)
-- deriving instance (Show a, Show b, AdaptPair a b) => Show (Pair a b)

{-
instance (Bounded a, Bounded b, AdaptPair a b) => Bounded (Pair a b) where
    minBound = pair minBound minBound
    maxBound = pair maxBound maxBound

instance (Eq a, Eq b, AdaptPair a b) => Eq (Pair a b) where
    p == q = fst p == fst q && snd p == snd q

instance (Ord a, Ord b, AdaptPair a b) => Ord (Pair a b) where
    compare p q =
        compare (fst p) (fst q)
      `compare`
        compare (snd p) (snd q)

instance (Show a, Show b, AdaptPair a b) => Show (Pair a b) where
    show p = "Pair " ++ show (fst p) ++ " "++ show (snd p)
-}

------------------------------------------------------------------------
--
-- Generic instance of pairs in terms of (,)
--
-- Courtesy sjanssen:
--
-- Suspicious on a number of levels:
--
-- 1 $ ghci-6.10.1
-- 2 GHCi, version 6.10.1: http://www.haskell.org/ghc/  :? for help
-- 3 Loading package ghc-prim ... linking ... done.
-- 4 Loading package integer ... linking ... done.
-- 5 Loading package base ... linking ... done.
-- 6 Prelude> :m +Data.Adaptive.Tuple
-- 7 Prelude Data.Adaptive.Tuple>  case PUnit of PairPair x -> x
-- 8 Loading package adaptive-containers-0.2 ... linking ... done.
-- 9 (Segmentation fault (core dumped) 
--
--
--
--instance AdaptPair a b where
--  newtype Pair a b = PairPair { unPair :: (,) a b }
--
--  fst     = Prelude.fst . unPair
--  snd     = Prelude.snd . unPair
--  curry f x y   = f (PairPair (x,y))

------------------------------------------------------------------------
--
-- Hand written instances:
--

instance AdaptPair () () where
  type Pair () () = PUnit

  fst PUnit = ()
  snd PUnit = ()
  curry f _ _    =  f PUnit

instance AdaptPair Bool Bool where
  type Pair Bool Bool = PBool

  fst (PBool x _) = Prelude.toEnum x
  snd (PBool _ x) = Prelude.toEnum x
  curry f x y    =  f (PBool (Prelude.fromEnum x) (Prelude.fromEnum y))

-- TODO: sums:     Bool
-- TODO: products: pairs

------------------------------------------------------------------------
--
-- Generated by scripts/derive-pair.hs
--

instance AdaptPair Int Int where
    type Pair Int Int = PairIntInt 
    fst (PairIntInt a _) = a
    snd (PairIntInt _ b) = b
    curry f x y = f (PairIntInt x y)

instance AdaptPair Int Integer where
    type Pair Int Integer = PairIntInteger 
    fst (PairIntInteger a _) = a
    snd (PairIntInteger _ b) = b
    curry f x y = f (PairIntInteger x y)

instance AdaptPair Int Int8 where
    type Pair Int Int8 = PairIntInt8 
    fst (PairIntInt8 a _) = a
    snd (PairIntInt8 _ b) = b
    curry f x y = f (PairIntInt8 x y)

instance AdaptPair Int Int16 where
    type Pair Int Int16 = PairIntInt16 
    fst (PairIntInt16 a _) = a
    snd (PairIntInt16 _ b) = b
    curry f x y = f (PairIntInt16 x y)

instance AdaptPair Int Int32 where
    type Pair Int Int32 = PairIntInt32 
    fst (PairIntInt32 a _) = a
    snd (PairIntInt32 _ b) = b
    curry f x y = f (PairIntInt32 x y)

instance AdaptPair Int Int64 where
    type Pair Int Int64 = PairIntInt64 
    fst (PairIntInt64 a _) = a
    snd (PairIntInt64 _ b) = b
    curry f x y = f (PairIntInt64 x y)

instance AdaptPair Int Word where
    type Pair Int Word = PairIntWord 
    fst (PairIntWord a _) = a
    snd (PairIntWord _ b) = b
    curry f x y = f (PairIntWord x y)

instance AdaptPair Int Word8 where
    type Pair Int Word8 = PairIntWord8 
    fst (PairIntWord8 a _) = a
    snd (PairIntWord8 _ b) = b
    curry f x y = f (PairIntWord8 x y)

instance AdaptPair Int Word16 where
    type Pair Int Word16 = PairIntWord16 
    fst (PairIntWord16 a _) = a
    snd (PairIntWord16 _ b) = b
    curry f x y = f (PairIntWord16 x y)

instance AdaptPair Int Word32 where
    type Pair Int Word32 = PairIntWord32 
    fst (PairIntWord32 a _) = a
    snd (PairIntWord32 _ b) = b
    curry f x y = f (PairIntWord32 x y)

instance AdaptPair Int Word64 where
    type Pair Int Word64 = PairIntWord64 
    fst (PairIntWord64 a _) = a
    snd (PairIntWord64 _ b) = b
    curry f x y = f (PairIntWord64 x y)

instance AdaptPair Int Double where
    type Pair Int Double = PairIntDouble 
    fst (PairIntDouble a _) = a
    snd (PairIntDouble _ b) = b
    curry f x y = f (PairIntDouble x y)

instance AdaptPair Int Float where
    type Pair Int Float = PairIntFloat 
    fst (PairIntFloat a _) = a
    snd (PairIntFloat _ b) = b
    curry f x y = f (PairIntFloat x y)

instance AdaptPair Int Char where
    type Pair Int Char = PairIntChar 
    fst (PairIntChar a _) = a
    snd (PairIntChar _ b) = b
    curry f x y = f (PairIntChar x y)

instance AdaptPair Integer Int where
    type Pair Integer Int = PairIntegerInt 
    fst (PairIntegerInt a _) = a
    snd (PairIntegerInt _ b) = b
    curry f x y = f (PairIntegerInt x y)

instance AdaptPair Integer Integer where
    type Pair Integer Integer = PairIntegerInteger 
    fst (PairIntegerInteger a _) = a
    snd (PairIntegerInteger _ b) = b
    curry f x y = f (PairIntegerInteger x y)

instance AdaptPair Integer Int8 where
    type Pair Integer Int8 = PairIntegerInt8 
    fst (PairIntegerInt8 a _) = a
    snd (PairIntegerInt8 _ b) = b
    curry f x y = f (PairIntegerInt8 x y)

instance AdaptPair Integer Int16 where
    type Pair Integer Int16 = PairIntegerInt16 
    fst (PairIntegerInt16 a _) = a
    snd (PairIntegerInt16 _ b) = b
    curry f x y = f (PairIntegerInt16 x y)

instance AdaptPair Integer Int32 where
    type Pair Integer Int32 = PairIntegerInt32 
    fst (PairIntegerInt32 a _) = a
    snd (PairIntegerInt32 _ b) = b
    curry f x y = f (PairIntegerInt32 x y)

instance AdaptPair Integer Int64 where
    type Pair Integer Int64 = PairIntegerInt64 
    fst (PairIntegerInt64 a _) = a
    snd (PairIntegerInt64 _ b) = b
    curry f x y = f (PairIntegerInt64 x y)

instance AdaptPair Integer Word where
    type Pair Integer Word = PairIntegerWord 
    fst (PairIntegerWord a _) = a
    snd (PairIntegerWord _ b) = b
    curry f x y = f (PairIntegerWord x y)

instance AdaptPair Integer Word8 where
    type Pair Integer Word8 = PairIntegerWord8 
    fst (PairIntegerWord8 a _) = a
    snd (PairIntegerWord8 _ b) = b
    curry f x y = f (PairIntegerWord8 x y)

instance AdaptPair Integer Word16 where
    type Pair Integer Word16 = PairIntegerWord16 
    fst (PairIntegerWord16 a _) = a
    snd (PairIntegerWord16 _ b) = b
    curry f x y = f (PairIntegerWord16 x y)

instance AdaptPair Integer Word32 where
    type Pair Integer Word32 = PairIntegerWord32 
    fst (PairIntegerWord32 a _) = a
    snd (PairIntegerWord32 _ b) = b
    curry f x y = f (PairIntegerWord32 x y)

instance AdaptPair Integer Word64 where
    type Pair Integer Word64 = PairIntegerWord64 
    fst (PairIntegerWord64 a _) = a
    snd (PairIntegerWord64 _ b) = b
    curry f x y = f (PairIntegerWord64 x y)

instance AdaptPair Integer Double where
    type Pair Integer Double = PairIntegerDouble 
    fst (PairIntegerDouble a _) = a
    snd (PairIntegerDouble _ b) = b
    curry f x y = f (PairIntegerDouble x y)

instance AdaptPair Integer Float where
    type Pair Integer Float = PairIntegerFloat 
    fst (PairIntegerFloat a _) = a
    snd (PairIntegerFloat _ b) = b
    curry f x y = f (PairIntegerFloat x y)

instance AdaptPair Integer Char where
    type Pair Integer Char = PairIntegerChar 
    fst (PairIntegerChar a _) = a
    snd (PairIntegerChar _ b) = b
    curry f x y = f (PairIntegerChar x y)

instance AdaptPair Int8 Int where
    type Pair Int8 Int = PairInt8Int 
    fst (PairInt8Int a _) = a
    snd (PairInt8Int _ b) = b
    curry f x y = f (PairInt8Int x y)

instance AdaptPair Int8 Integer where
    type Pair Int8 Integer = PairInt8Integer 
    fst (PairInt8Integer a _) = a
    snd (PairInt8Integer _ b) = b
    curry f x y = f (PairInt8Integer x y)

instance AdaptPair Int8 Int8 where
    type Pair Int8 Int8 = PairInt8Int8 
    fst (PairInt8Int8 a _) = a
    snd (PairInt8Int8 _ b) = b
    curry f x y = f (PairInt8Int8 x y)

instance AdaptPair Int8 Int16 where
    type Pair Int8 Int16 = PairInt8Int16 
    fst (PairInt8Int16 a _) = a
    snd (PairInt8Int16 _ b) = b
    curry f x y = f (PairInt8Int16 x y)

instance AdaptPair Int8 Int32 where
    type Pair Int8 Int32 = PairInt8Int32 
    fst (PairInt8Int32 a _) = a
    snd (PairInt8Int32 _ b) = b
    curry f x y = f (PairInt8Int32 x y)

instance AdaptPair Int8 Int64 where
    type Pair Int8 Int64 = PairInt8Int64 
    fst (PairInt8Int64 a _) = a
    snd (PairInt8Int64 _ b) = b
    curry f x y = f (PairInt8Int64 x y)

instance AdaptPair Int8 Word where
    type Pair Int8 Word = PairInt8Word 
    fst (PairInt8Word a _) = a
    snd (PairInt8Word _ b) = b
    curry f x y = f (PairInt8Word x y)

instance AdaptPair Int8 Word8 where
    type Pair Int8 Word8 = PairInt8Word8 
    fst (PairInt8Word8 a _) = a
    snd (PairInt8Word8 _ b) = b
    curry f x y = f (PairInt8Word8 x y)

instance AdaptPair Int8 Word16 where
    type Pair Int8 Word16 = PairInt8Word16 
    fst (PairInt8Word16 a _) = a
    snd (PairInt8Word16 _ b) = b
    curry f x y = f (PairInt8Word16 x y)

instance AdaptPair Int8 Word32 where
    type Pair Int8 Word32 = PairInt8Word32 
    fst (PairInt8Word32 a _) = a
    snd (PairInt8Word32 _ b) = b
    curry f x y = f (PairInt8Word32 x y)

instance AdaptPair Int8 Word64 where
    type Pair Int8 Word64 = PairInt8Word64 
    fst (PairInt8Word64 a _) = a
    snd (PairInt8Word64 _ b) = b
    curry f x y = f (PairInt8Word64 x y)

instance AdaptPair Int8 Double where
    type Pair Int8 Double = PairInt8Double 
    fst (PairInt8Double a _) = a
    snd (PairInt8Double _ b) = b
    curry f x y = f (PairInt8Double x y)

instance AdaptPair Int8 Float where
    type Pair Int8 Float = PairInt8Float 
    fst (PairInt8Float a _) = a
    snd (PairInt8Float _ b) = b
    curry f x y = f (PairInt8Float x y)

instance AdaptPair Int8 Char where
    type Pair Int8 Char = PairInt8Char 
    fst (PairInt8Char a _) = a
    snd (PairInt8Char _ b) = b
    curry f x y = f (PairInt8Char x y)

instance AdaptPair Int16 Int where
    type Pair Int16 Int = PairInt16Int 
    fst (PairInt16Int a _) = a
    snd (PairInt16Int _ b) = b
    curry f x y = f (PairInt16Int x y)

instance AdaptPair Int16 Integer where
    type Pair Int16 Integer = PairInt16Integer 
    fst (PairInt16Integer a _) = a
    snd (PairInt16Integer _ b) = b
    curry f x y = f (PairInt16Integer x y)

instance AdaptPair Int16 Int8 where
    type Pair Int16 Int8 = PairInt16Int8 
    fst (PairInt16Int8 a _) = a
    snd (PairInt16Int8 _ b) = b
    curry f x y = f (PairInt16Int8 x y)

instance AdaptPair Int16 Int16 where
    type Pair Int16 Int16 = PairInt16Int16 
    fst (PairInt16Int16 a _) = a
    snd (PairInt16Int16 _ b) = b
    curry f x y = f (PairInt16Int16 x y)

instance AdaptPair Int16 Int32 where
    type Pair Int16 Int32 = PairInt16Int32 
    fst (PairInt16Int32 a _) = a
    snd (PairInt16Int32 _ b) = b
    curry f x y = f (PairInt16Int32 x y)

instance AdaptPair Int16 Int64 where
    type Pair Int16 Int64 = PairInt16Int64 
    fst (PairInt16Int64 a _) = a
    snd (PairInt16Int64 _ b) = b
    curry f x y = f (PairInt16Int64 x y)

instance AdaptPair Int16 Word where
    type Pair Int16 Word = PairInt16Word 
    fst (PairInt16Word a _) = a
    snd (PairInt16Word _ b) = b
    curry f x y = f (PairInt16Word x y)

instance AdaptPair Int16 Word8 where
    type Pair Int16 Word8 = PairInt16Word8 
    fst (PairInt16Word8 a _) = a
    snd (PairInt16Word8 _ b) = b
    curry f x y = f (PairInt16Word8 x y)

instance AdaptPair Int16 Word16 where
    type Pair Int16 Word16 = PairInt16Word16 
    fst (PairInt16Word16 a _) = a
    snd (PairInt16Word16 _ b) = b
    curry f x y = f (PairInt16Word16 x y)

instance AdaptPair Int16 Word32 where
    type Pair Int16 Word32 = PairInt16Word32 
    fst (PairInt16Word32 a _) = a
    snd (PairInt16Word32 _ b) = b
    curry f x y = f (PairInt16Word32 x y)

instance AdaptPair Int16 Word64 where
    type Pair Int16 Word64 = PairInt16Word64 
    fst (PairInt16Word64 a _) = a
    snd (PairInt16Word64 _ b) = b
    curry f x y = f (PairInt16Word64 x y)

instance AdaptPair Int16 Double where
    type Pair Int16 Double = PairInt16Double 
    fst (PairInt16Double a _) = a
    snd (PairInt16Double _ b) = b
    curry f x y = f (PairInt16Double x y)

instance AdaptPair Int16 Float where
    type Pair Int16 Float = PairInt16Float 
    fst (PairInt16Float a _) = a
    snd (PairInt16Float _ b) = b
    curry f x y = f (PairInt16Float x y)

instance AdaptPair Int16 Char where
    type Pair Int16 Char = PairInt16Char 
    fst (PairInt16Char a _) = a
    snd (PairInt16Char _ b) = b
    curry f x y = f (PairInt16Char x y)

instance AdaptPair Int32 Int where
    type Pair Int32 Int = PairInt32Int 
    fst (PairInt32Int a _) = a
    snd (PairInt32Int _ b) = b
    curry f x y = f (PairInt32Int x y)

instance AdaptPair Int32 Integer where
    type Pair Int32 Integer = PairInt32Integer 
    fst (PairInt32Integer a _) = a
    snd (PairInt32Integer _ b) = b
    curry f x y = f (PairInt32Integer x y)

instance AdaptPair Int32 Int8 where
    type Pair Int32 Int8 = PairInt32Int8 
    fst (PairInt32Int8 a _) = a
    snd (PairInt32Int8 _ b) = b
    curry f x y = f (PairInt32Int8 x y)

instance AdaptPair Int32 Int16 where
    type Pair Int32 Int16 = PairInt32Int16 
    fst (PairInt32Int16 a _) = a
    snd (PairInt32Int16 _ b) = b
    curry f x y = f (PairInt32Int16 x y)

instance AdaptPair Int32 Int32 where
    type Pair Int32 Int32 = PairInt32Int32 
    fst (PairInt32Int32 a _) = a
    snd (PairInt32Int32 _ b) = b
    curry f x y = f (PairInt32Int32 x y)

instance AdaptPair Int32 Int64 where
    type Pair Int32 Int64 = PairInt32Int64 
    fst (PairInt32Int64 a _) = a
    snd (PairInt32Int64 _ b) = b
    curry f x y = f (PairInt32Int64 x y)

instance AdaptPair Int32 Word where
    type Pair Int32 Word = PairInt32Word 
    fst (PairInt32Word a _) = a
    snd (PairInt32Word _ b) = b
    curry f x y = f (PairInt32Word x y)

instance AdaptPair Int32 Word8 where
    type Pair Int32 Word8 = PairInt32Word8 
    fst (PairInt32Word8 a _) = a
    snd (PairInt32Word8 _ b) = b
    curry f x y = f (PairInt32Word8 x y)

instance AdaptPair Int32 Word16 where
    type Pair Int32 Word16 = PairInt32Word16 
    fst (PairInt32Word16 a _) = a
    snd (PairInt32Word16 _ b) = b
    curry f x y = f (PairInt32Word16 x y)

instance AdaptPair Int32 Word32 where
    type Pair Int32 Word32 = PairInt32Word32 
    fst (PairInt32Word32 a _) = a
    snd (PairInt32Word32 _ b) = b
    curry f x y = f (PairInt32Word32 x y)

instance AdaptPair Int32 Word64 where
    type Pair Int32 Word64 = PairInt32Word64 
    fst (PairInt32Word64 a _) = a
    snd (PairInt32Word64 _ b) = b
    curry f x y = f (PairInt32Word64 x y)

instance AdaptPair Int32 Double where
    type Pair Int32 Double = PairInt32Double 
    fst (PairInt32Double a _) = a
    snd (PairInt32Double _ b) = b
    curry f x y = f (PairInt32Double x y)

instance AdaptPair Int32 Float where
    type Pair Int32 Float = PairInt32Float 
    fst (PairInt32Float a _) = a
    snd (PairInt32Float _ b) = b
    curry f x y = f (PairInt32Float x y)

instance AdaptPair Int32 Char where
    type Pair Int32 Char = PairInt32Char 
    fst (PairInt32Char a _) = a
    snd (PairInt32Char _ b) = b
    curry f x y = f (PairInt32Char x y)

instance AdaptPair Int64 Int where
    type Pair Int64 Int = PairInt64Int 
    fst (PairInt64Int a _) = a
    snd (PairInt64Int _ b) = b
    curry f x y = f (PairInt64Int x y)

instance AdaptPair Int64 Integer where
    type Pair Int64 Integer = PairInt64Integer 
    fst (PairInt64Integer a _) = a
    snd (PairInt64Integer _ b) = b
    curry f x y = f (PairInt64Integer x y)

instance AdaptPair Int64 Int8 where
    type Pair Int64 Int8 = PairInt64Int8 
    fst (PairInt64Int8 a _) = a
    snd (PairInt64Int8 _ b) = b
    curry f x y = f (PairInt64Int8 x y)

instance AdaptPair Int64 Int16 where
    type Pair Int64 Int16 = PairInt64Int16 
    fst (PairInt64Int16 a _) = a
    snd (PairInt64Int16 _ b) = b
    curry f x y = f (PairInt64Int16 x y)

instance AdaptPair Int64 Int32 where
    type Pair Int64 Int32 = PairInt64Int32 
    fst (PairInt64Int32 a _) = a
    snd (PairInt64Int32 _ b) = b
    curry f x y = f (PairInt64Int32 x y)

instance AdaptPair Int64 Int64 where
    type Pair Int64 Int64 = PairInt64Int64 
    fst (PairInt64Int64 a _) = a
    snd (PairInt64Int64 _ b) = b
    curry f x y = f (PairInt64Int64 x y)

instance AdaptPair Int64 Word where
    type Pair Int64 Word = PairInt64Word 
    fst (PairInt64Word a _) = a
    snd (PairInt64Word _ b) = b
    curry f x y = f (PairInt64Word x y)

instance AdaptPair Int64 Word8 where
    type Pair Int64 Word8 = PairInt64Word8 
    fst (PairInt64Word8 a _) = a
    snd (PairInt64Word8 _ b) = b
    curry f x y = f (PairInt64Word8 x y)

instance AdaptPair Int64 Word16 where
    type Pair Int64 Word16 = PairInt64Word16 
    fst (PairInt64Word16 a _) = a
    snd (PairInt64Word16 _ b) = b
    curry f x y = f (PairInt64Word16 x y)

instance AdaptPair Int64 Word32 where
    type Pair Int64 Word32 = PairInt64Word32 
    fst (PairInt64Word32 a _) = a
    snd (PairInt64Word32 _ b) = b
    curry f x y = f (PairInt64Word32 x y)

instance AdaptPair Int64 Word64 where
    type Pair Int64 Word64 = PairInt64Word64 
    fst (PairInt64Word64 a _) = a
    snd (PairInt64Word64 _ b) = b
    curry f x y = f (PairInt64Word64 x y)

instance AdaptPair Int64 Double where
    type Pair Int64 Double = PairInt64Double 
    fst (PairInt64Double a _) = a
    snd (PairInt64Double _ b) = b
    curry f x y = f (PairInt64Double x y)

instance AdaptPair Int64 Float where
    type Pair Int64 Float = PairInt64Float 
    fst (PairInt64Float a _) = a
    snd (PairInt64Float _ b) = b
    curry f x y = f (PairInt64Float x y)

instance AdaptPair Int64 Char where
    type Pair Int64 Char = PairInt64Char 
    fst (PairInt64Char a _) = a
    snd (PairInt64Char _ b) = b
    curry f x y = f (PairInt64Char x y)

instance AdaptPair Word Int where
    type Pair Word Int = PairWordInt 
    fst (PairWordInt a _) = a
    snd (PairWordInt _ b) = b
    curry f x y = f (PairWordInt x y)

instance AdaptPair Word Integer where
    type Pair Word Integer = PairWordInteger 
    fst (PairWordInteger a _) = a
    snd (PairWordInteger _ b) = b
    curry f x y = f (PairWordInteger x y)

instance AdaptPair Word Int8 where
    type Pair Word Int8 = PairWordInt8 
    fst (PairWordInt8 a _) = a
    snd (PairWordInt8 _ b) = b
    curry f x y = f (PairWordInt8 x y)

instance AdaptPair Word Int16 where
    type Pair Word Int16 = PairWordInt16 
    fst (PairWordInt16 a _) = a
    snd (PairWordInt16 _ b) = b
    curry f x y = f (PairWordInt16 x y)

instance AdaptPair Word Int32 where
    type Pair Word Int32 = PairWordInt32 
    fst (PairWordInt32 a _) = a
    snd (PairWordInt32 _ b) = b
    curry f x y = f (PairWordInt32 x y)

instance AdaptPair Word Int64 where
    type Pair Word Int64 = PairWordInt64 
    fst (PairWordInt64 a _) = a
    snd (PairWordInt64 _ b) = b
    curry f x y = f (PairWordInt64 x y)

instance AdaptPair Word Word where
    type Pair Word Word = PairWordWord 
    fst (PairWordWord a _) = a
    snd (PairWordWord _ b) = b
    curry f x y = f (PairWordWord x y)

instance AdaptPair Word Word8 where
    type Pair Word Word8 = PairWordWord8 
    fst (PairWordWord8 a _) = a
    snd (PairWordWord8 _ b) = b
    curry f x y = f (PairWordWord8 x y)

instance AdaptPair Word Word16 where
    type Pair Word Word16 = PairWordWord16 
    fst (PairWordWord16 a _) = a
    snd (PairWordWord16 _ b) = b
    curry f x y = f (PairWordWord16 x y)

instance AdaptPair Word Word32 where
    type Pair Word Word32 = PairWordWord32 
    fst (PairWordWord32 a _) = a
    snd (PairWordWord32 _ b) = b
    curry f x y = f (PairWordWord32 x y)

instance AdaptPair Word Word64 where
    type Pair Word Word64 = PairWordWord64 
    fst (PairWordWord64 a _) = a
    snd (PairWordWord64 _ b) = b
    curry f x y = f (PairWordWord64 x y)

instance AdaptPair Word Double where
    type Pair Word Double = PairWordDouble 
    fst (PairWordDouble a _) = a
    snd (PairWordDouble _ b) = b
    curry f x y = f (PairWordDouble x y)

instance AdaptPair Word Float where
    type Pair Word Float = PairWordFloat 
    fst (PairWordFloat a _) = a
    snd (PairWordFloat _ b) = b
    curry f x y = f (PairWordFloat x y)

instance AdaptPair Word Char where
    type Pair Word Char = PairWordChar 
    fst (PairWordChar a _) = a
    snd (PairWordChar _ b) = b
    curry f x y = f (PairWordChar x y)

instance AdaptPair Word8 Int where
    type Pair Word8 Int = PairWord8Int 
    fst (PairWord8Int a _) = a
    snd (PairWord8Int _ b) = b
    curry f x y = f (PairWord8Int x y)

instance AdaptPair Word8 Integer where
    type Pair Word8 Integer = PairWord8Integer 
    fst (PairWord8Integer a _) = a
    snd (PairWord8Integer _ b) = b
    curry f x y = f (PairWord8Integer x y)

instance AdaptPair Word8 Int8 where
    type Pair Word8 Int8 = PairWord8Int8 
    fst (PairWord8Int8 a _) = a
    snd (PairWord8Int8 _ b) = b
    curry f x y = f (PairWord8Int8 x y)

instance AdaptPair Word8 Int16 where
    type Pair Word8 Int16 = PairWord8Int16 
    fst (PairWord8Int16 a _) = a
    snd (PairWord8Int16 _ b) = b
    curry f x y = f (PairWord8Int16 x y)

instance AdaptPair Word8 Int32 where
    type Pair Word8 Int32 = PairWord8Int32 
    fst (PairWord8Int32 a _) = a
    snd (PairWord8Int32 _ b) = b
    curry f x y = f (PairWord8Int32 x y)

instance AdaptPair Word8 Int64 where
    type Pair Word8 Int64 = PairWord8Int64 
    fst (PairWord8Int64 a _) = a
    snd (PairWord8Int64 _ b) = b
    curry f x y = f (PairWord8Int64 x y)

instance AdaptPair Word8 Word where
    type Pair Word8 Word = PairWord8Word 
    fst (PairWord8Word a _) = a
    snd (PairWord8Word _ b) = b
    curry f x y = f (PairWord8Word x y)

instance AdaptPair Word8 Word8 where
    type Pair Word8 Word8 = PairWord8Word8 
    fst (PairWord8Word8 a _) = a
    snd (PairWord8Word8 _ b) = b
    curry f x y = f (PairWord8Word8 x y)

instance AdaptPair Word8 Word16 where
    type Pair Word8 Word16 = PairWord8Word16 
    fst (PairWord8Word16 a _) = a
    snd (PairWord8Word16 _ b) = b
    curry f x y = f (PairWord8Word16 x y)

instance AdaptPair Word8 Word32 where
    type Pair Word8 Word32 = PairWord8Word32 
    fst (PairWord8Word32 a _) = a
    snd (PairWord8Word32 _ b) = b
    curry f x y = f (PairWord8Word32 x y)

instance AdaptPair Word8 Word64 where
    type Pair Word8 Word64 = PairWord8Word64 
    fst (PairWord8Word64 a _) = a
    snd (PairWord8Word64 _ b) = b
    curry f x y = f (PairWord8Word64 x y)

instance AdaptPair Word8 Double where
    type Pair Word8 Double = PairWord8Double 
    fst (PairWord8Double a _) = a
    snd (PairWord8Double _ b) = b
    curry f x y = f (PairWord8Double x y)

instance AdaptPair Word8 Float where
    type Pair Word8 Float = PairWord8Float 
    fst (PairWord8Float a _) = a
    snd (PairWord8Float _ b) = b
    curry f x y = f (PairWord8Float x y)

instance AdaptPair Word8 Char where
    type Pair Word8 Char = PairWord8Char 
    fst (PairWord8Char a _) = a
    snd (PairWord8Char _ b) = b
    curry f x y = f (PairWord8Char x y)

instance AdaptPair Word16 Int where
    type Pair Word16 Int = PairWord16Int 
    fst (PairWord16Int a _) = a
    snd (PairWord16Int _ b) = b
    curry f x y = f (PairWord16Int x y)

instance AdaptPair Word16 Integer where
    type Pair Word16 Integer = PairWord16Integer 
    fst (PairWord16Integer a _) = a
    snd (PairWord16Integer _ b) = b
    curry f x y = f (PairWord16Integer x y)

instance AdaptPair Word16 Int8 where
    type Pair Word16 Int8 = PairWord16Int8 
    fst (PairWord16Int8 a _) = a
    snd (PairWord16Int8 _ b) = b
    curry f x y = f (PairWord16Int8 x y)

instance AdaptPair Word16 Int16 where
    type Pair Word16 Int16 = PairWord16Int16 
    fst (PairWord16Int16 a _) = a
    snd (PairWord16Int16 _ b) = b
    curry f x y = f (PairWord16Int16 x y)

instance AdaptPair Word16 Int32 where
    type Pair Word16 Int32 = PairWord16Int32 
    fst (PairWord16Int32 a _) = a
    snd (PairWord16Int32 _ b) = b
    curry f x y = f (PairWord16Int32 x y)

instance AdaptPair Word16 Int64 where
    type Pair Word16 Int64 = PairWord16Int64 
    fst (PairWord16Int64 a _) = a
    snd (PairWord16Int64 _ b) = b
    curry f x y = f (PairWord16Int64 x y)

instance AdaptPair Word16 Word where
    type Pair Word16 Word = PairWord16Word 
    fst (PairWord16Word a _) = a
    snd (PairWord16Word _ b) = b
    curry f x y = f (PairWord16Word x y)

instance AdaptPair Word16 Word8 where
    type Pair Word16 Word8 = PairWord16Word8 
    fst (PairWord16Word8 a _) = a
    snd (PairWord16Word8 _ b) = b
    curry f x y = f (PairWord16Word8 x y)

instance AdaptPair Word16 Word16 where
    type Pair Word16 Word16 = PairWord16Word16 
    fst (PairWord16Word16 a _) = a
    snd (PairWord16Word16 _ b) = b
    curry f x y = f (PairWord16Word16 x y)

instance AdaptPair Word16 Word32 where
    type Pair Word16 Word32 = PairWord16Word32 
    fst (PairWord16Word32 a _) = a
    snd (PairWord16Word32 _ b) = b
    curry f x y = f (PairWord16Word32 x y)

instance AdaptPair Word16 Word64 where
    type Pair Word16 Word64 = PairWord16Word64 
    fst (PairWord16Word64 a _) = a
    snd (PairWord16Word64 _ b) = b
    curry f x y = f (PairWord16Word64 x y)

instance AdaptPair Word16 Double where
    type Pair Word16 Double = PairWord16Double 
    fst (PairWord16Double a _) = a
    snd (PairWord16Double _ b) = b
    curry f x y = f (PairWord16Double x y)

instance AdaptPair Word16 Float where
    type Pair Word16 Float = PairWord16Float 
    fst (PairWord16Float a _) = a
    snd (PairWord16Float _ b) = b
    curry f x y = f (PairWord16Float x y)

instance AdaptPair Word16 Char where
    type Pair Word16 Char = PairWord16Char 
    fst (PairWord16Char a _) = a
    snd (PairWord16Char _ b) = b
    curry f x y = f (PairWord16Char x y)

instance AdaptPair Word32 Int where
    type Pair Word32 Int = PairWord32Int 
    fst (PairWord32Int a _) = a
    snd (PairWord32Int _ b) = b
    curry f x y = f (PairWord32Int x y)

instance AdaptPair Word32 Integer where
    type Pair Word32 Integer = PairWord32Integer 
    fst (PairWord32Integer a _) = a
    snd (PairWord32Integer _ b) = b
    curry f x y = f (PairWord32Integer x y)

instance AdaptPair Word32 Int8 where
    type Pair Word32 Int8 = PairWord32Int8 
    fst (PairWord32Int8 a _) = a
    snd (PairWord32Int8 _ b) = b
    curry f x y = f (PairWord32Int8 x y)

instance AdaptPair Word32 Int16 where
    type Pair Word32 Int16 = PairWord32Int16 
    fst (PairWord32Int16 a _) = a
    snd (PairWord32Int16 _ b) = b
    curry f x y = f (PairWord32Int16 x y)

instance AdaptPair Word32 Int32 where
    type Pair Word32 Int32 = PairWord32Int32 
    fst (PairWord32Int32 a _) = a
    snd (PairWord32Int32 _ b) = b
    curry f x y = f (PairWord32Int32 x y)

instance AdaptPair Word32 Int64 where
    type Pair Word32 Int64 = PairWord32Int64 
    fst (PairWord32Int64 a _) = a
    snd (PairWord32Int64 _ b) = b
    curry f x y = f (PairWord32Int64 x y)

instance AdaptPair Word32 Word where
    type Pair Word32 Word = PairWord32Word 
    fst (PairWord32Word a _) = a
    snd (PairWord32Word _ b) = b
    curry f x y = f (PairWord32Word x y)

instance AdaptPair Word32 Word8 where
    type Pair Word32 Word8 = PairWord32Word8 
    fst (PairWord32Word8 a _) = a
    snd (PairWord32Word8 _ b) = b
    curry f x y = f (PairWord32Word8 x y)

instance AdaptPair Word32 Word16 where
    type Pair Word32 Word16 = PairWord32Word16 
    fst (PairWord32Word16 a _) = a
    snd (PairWord32Word16 _ b) = b
    curry f x y = f (PairWord32Word16 x y)

instance AdaptPair Word32 Word32 where
    type Pair Word32 Word32 = PairWord32Word32 
    fst (PairWord32Word32 a _) = a
    snd (PairWord32Word32 _ b) = b
    curry f x y = f (PairWord32Word32 x y)

instance AdaptPair Word32 Word64 where
    type Pair Word32 Word64 = PairWord32Word64 
    fst (PairWord32Word64 a _) = a
    snd (PairWord32Word64 _ b) = b
    curry f x y = f (PairWord32Word64 x y)

instance AdaptPair Word32 Double where
    type Pair Word32 Double = PairWord32Double 
    fst (PairWord32Double a _) = a
    snd (PairWord32Double _ b) = b
    curry f x y = f (PairWord32Double x y)

instance AdaptPair Word32 Float where
    type Pair Word32 Float = PairWord32Float 
    fst (PairWord32Float a _) = a
    snd (PairWord32Float _ b) = b
    curry f x y = f (PairWord32Float x y)

instance AdaptPair Word32 Char where
    type Pair Word32 Char = PairWord32Char 
    fst (PairWord32Char a _) = a
    snd (PairWord32Char _ b) = b
    curry f x y = f (PairWord32Char x y)

instance AdaptPair Word64 Int where
    type Pair Word64 Int = PairWord64Int 
    fst (PairWord64Int a _) = a
    snd (PairWord64Int _ b) = b
    curry f x y = f (PairWord64Int x y)

instance AdaptPair Word64 Integer where
    type Pair Word64 Integer = PairWord64Integer 
    fst (PairWord64Integer a _) = a
    snd (PairWord64Integer _ b) = b
    curry f x y = f (PairWord64Integer x y)

instance AdaptPair Word64 Int8 where
    type Pair Word64 Int8 = PairWord64Int8 
    fst (PairWord64Int8 a _) = a
    snd (PairWord64Int8 _ b) = b
    curry f x y = f (PairWord64Int8 x y)

instance AdaptPair Word64 Int16 where
    type Pair Word64 Int16 = PairWord64Int16 
    fst (PairWord64Int16 a _) = a
    snd (PairWord64Int16 _ b) = b
    curry f x y = f (PairWord64Int16 x y)

instance AdaptPair Word64 Int32 where
    type Pair Word64 Int32 = PairWord64Int32 
    fst (PairWord64Int32 a _) = a
    snd (PairWord64Int32 _ b) = b
    curry f x y = f (PairWord64Int32 x y)

instance AdaptPair Word64 Int64 where
    type Pair Word64 Int64 = PairWord64Int64 
    fst (PairWord64Int64 a _) = a
    snd (PairWord64Int64 _ b) = b
    curry f x y = f (PairWord64Int64 x y)

instance AdaptPair Word64 Word where
    type Pair Word64 Word = PairWord64Word 
    fst (PairWord64Word a _) = a
    snd (PairWord64Word _ b) = b
    curry f x y = f (PairWord64Word x y)

instance AdaptPair Word64 Word8 where
    type Pair Word64 Word8 = PairWord64Word8 
    fst (PairWord64Word8 a _) = a
    snd (PairWord64Word8 _ b) = b
    curry f x y = f (PairWord64Word8 x y)

instance AdaptPair Word64 Word16 where
    type Pair Word64 Word16 = PairWord64Word16 
    fst (PairWord64Word16 a _) = a
    snd (PairWord64Word16 _ b) = b
    curry f x y = f (PairWord64Word16 x y)

instance AdaptPair Word64 Word32 where
    type Pair Word64 Word32 = PairWord64Word32 
    fst (PairWord64Word32 a _) = a
    snd (PairWord64Word32 _ b) = b
    curry f x y = f (PairWord64Word32 x y)

instance AdaptPair Word64 Word64 where
    type Pair Word64 Word64 = PairWord64Word64 
    fst (PairWord64Word64 a _) = a
    snd (PairWord64Word64 _ b) = b
    curry f x y = f (PairWord64Word64 x y)

instance AdaptPair Word64 Double where
    type Pair Word64 Double = PairWord64Double 
    fst (PairWord64Double a _) = a
    snd (PairWord64Double _ b) = b
    curry f x y = f (PairWord64Double x y)

instance AdaptPair Word64 Float where
    type Pair Word64 Float = PairWord64Float 
    fst (PairWord64Float a _) = a
    snd (PairWord64Float _ b) = b
    curry f x y = f (PairWord64Float x y)

instance AdaptPair Word64 Char where
    type Pair Word64 Char = PairWord64Char 
    fst (PairWord64Char a _) = a
    snd (PairWord64Char _ b) = b
    curry f x y = f (PairWord64Char x y)

instance AdaptPair Double Int where
    type Pair Double Int = PairDoubleInt 
    fst (PairDoubleInt a _) = a
    snd (PairDoubleInt _ b) = b
    curry f x y = f (PairDoubleInt x y)

instance AdaptPair Double Integer where
    type Pair Double Integer = PairDoubleInteger 
    fst (PairDoubleInteger a _) = a
    snd (PairDoubleInteger _ b) = b
    curry f x y = f (PairDoubleInteger x y)

instance AdaptPair Double Int8 where
    type Pair Double Int8 = PairDoubleInt8 
    fst (PairDoubleInt8 a _) = a
    snd (PairDoubleInt8 _ b) = b
    curry f x y = f (PairDoubleInt8 x y)

instance AdaptPair Double Int16 where
    type Pair Double Int16 = PairDoubleInt16 
    fst (PairDoubleInt16 a _) = a
    snd (PairDoubleInt16 _ b) = b
    curry f x y = f (PairDoubleInt16 x y)

instance AdaptPair Double Int32 where
    type Pair Double Int32 = PairDoubleInt32 
    fst (PairDoubleInt32 a _) = a
    snd (PairDoubleInt32 _ b) = b
    curry f x y = f (PairDoubleInt32 x y)

instance AdaptPair Double Int64 where
    type Pair Double Int64 = PairDoubleInt64 
    fst (PairDoubleInt64 a _) = a
    snd (PairDoubleInt64 _ b) = b
    curry f x y = f (PairDoubleInt64 x y)

instance AdaptPair Double Word where
    type Pair Double Word = PairDoubleWord 
    fst (PairDoubleWord a _) = a
    snd (PairDoubleWord _ b) = b
    curry f x y = f (PairDoubleWord x y)

instance AdaptPair Double Word8 where
    type Pair Double Word8 = PairDoubleWord8 
    fst (PairDoubleWord8 a _) = a
    snd (PairDoubleWord8 _ b) = b
    curry f x y = f (PairDoubleWord8 x y)

instance AdaptPair Double Word16 where
    type Pair Double Word16 = PairDoubleWord16 
    fst (PairDoubleWord16 a _) = a
    snd (PairDoubleWord16 _ b) = b
    curry f x y = f (PairDoubleWord16 x y)

instance AdaptPair Double Word32 where
    type Pair Double Word32 = PairDoubleWord32 
    fst (PairDoubleWord32 a _) = a
    snd (PairDoubleWord32 _ b) = b
    curry f x y = f (PairDoubleWord32 x y)

instance AdaptPair Double Word64 where
    type Pair Double Word64 = PairDoubleWord64 
    fst (PairDoubleWord64 a _) = a
    snd (PairDoubleWord64 _ b) = b
    curry f x y = f (PairDoubleWord64 x y)

instance AdaptPair Double Double where
    type Pair Double Double = PairDoubleDouble 
    fst (PairDoubleDouble a _) = a
    snd (PairDoubleDouble _ b) = b
    curry f x y = f (PairDoubleDouble x y)

instance AdaptPair Double Float where
    type Pair Double Float = PairDoubleFloat 
    fst (PairDoubleFloat a _) = a
    snd (PairDoubleFloat _ b) = b
    curry f x y = f (PairDoubleFloat x y)

instance AdaptPair Double Char where
    type Pair Double Char = PairDoubleChar 
    fst (PairDoubleChar a _) = a
    snd (PairDoubleChar _ b) = b
    curry f x y = f (PairDoubleChar x y)

instance AdaptPair Float Int where
    type Pair Float Int = PairFloatInt 
    fst (PairFloatInt a _) = a
    snd (PairFloatInt _ b) = b
    curry f x y = f (PairFloatInt x y)

instance AdaptPair Float Integer where
    type Pair Float Integer = PairFloatInteger 
    fst (PairFloatInteger a _) = a
    snd (PairFloatInteger _ b) = b
    curry f x y = f (PairFloatInteger x y)

instance AdaptPair Float Int8 where
    type Pair Float Int8 = PairFloatInt8 
    fst (PairFloatInt8 a _) = a
    snd (PairFloatInt8 _ b) = b
    curry f x y = f (PairFloatInt8 x y)

instance AdaptPair Float Int16 where
    type Pair Float Int16 = PairFloatInt16 
    fst (PairFloatInt16 a _) = a
    snd (PairFloatInt16 _ b) = b
    curry f x y = f (PairFloatInt16 x y)

instance AdaptPair Float Int32 where
    type Pair Float Int32 = PairFloatInt32 
    fst (PairFloatInt32 a _) = a
    snd (PairFloatInt32 _ b) = b
    curry f x y = f (PairFloatInt32 x y)

instance AdaptPair Float Int64 where
    type Pair Float Int64 = PairFloatInt64 
    fst (PairFloatInt64 a _) = a
    snd (PairFloatInt64 _ b) = b
    curry f x y = f (PairFloatInt64 x y)

instance AdaptPair Float Word where
    type Pair Float Word = PairFloatWord 
    fst (PairFloatWord a _) = a
    snd (PairFloatWord _ b) = b
    curry f x y = f (PairFloatWord x y)

instance AdaptPair Float Word8 where
    type Pair Float Word8 = PairFloatWord8 
    fst (PairFloatWord8 a _) = a
    snd (PairFloatWord8 _ b) = b
    curry f x y = f (PairFloatWord8 x y)

instance AdaptPair Float Word16 where
    type Pair Float Word16 = PairFloatWord16 
    fst (PairFloatWord16 a _) = a
    snd (PairFloatWord16 _ b) = b
    curry f x y = f (PairFloatWord16 x y)

instance AdaptPair Float Word32 where
    type Pair Float Word32 = PairFloatWord32 
    fst (PairFloatWord32 a _) = a
    snd (PairFloatWord32 _ b) = b
    curry f x y = f (PairFloatWord32 x y)

instance AdaptPair Float Word64 where
    type Pair Float Word64 = PairFloatWord64 
    fst (PairFloatWord64 a _) = a
    snd (PairFloatWord64 _ b) = b
    curry f x y = f (PairFloatWord64 x y)

instance AdaptPair Float Double where
    type Pair Float Double = PairFloatDouble 
    fst (PairFloatDouble a _) = a
    snd (PairFloatDouble _ b) = b
    curry f x y = f (PairFloatDouble x y)

instance AdaptPair Float Float where
    type Pair Float Float = PairFloatFloat 
    fst (PairFloatFloat a _) = a
    snd (PairFloatFloat _ b) = b
    curry f x y = f (PairFloatFloat x y)

instance AdaptPair Float Char where
    type Pair Float Char = PairFloatChar 
    fst (PairFloatChar a _) = a
    snd (PairFloatChar _ b) = b
    curry f x y = f (PairFloatChar x y)

instance AdaptPair Char Int where
    type Pair Char Int = PairCharInt 
    fst (PairCharInt a _) = a
    snd (PairCharInt _ b) = b
    curry f x y = f (PairCharInt x y)

instance AdaptPair Char Integer where
    type Pair Char Integer = PairCharInteger 
    fst (PairCharInteger a _) = a
    snd (PairCharInteger _ b) = b
    curry f x y = f (PairCharInteger x y)

instance AdaptPair Char Int8 where
    type Pair Char Int8 = PairCharInt8 
    fst (PairCharInt8 a _) = a
    snd (PairCharInt8 _ b) = b
    curry f x y = f (PairCharInt8 x y)

instance AdaptPair Char Int16 where
    type Pair Char Int16 = PairCharInt16 
    fst (PairCharInt16 a _) = a
    snd (PairCharInt16 _ b) = b
    curry f x y = f (PairCharInt16 x y)

instance AdaptPair Char Int32 where
    type Pair Char Int32 = PairCharInt32 
    fst (PairCharInt32 a _) = a
    snd (PairCharInt32 _ b) = b
    curry f x y = f (PairCharInt32 x y)

instance AdaptPair Char Int64 where
    type Pair Char Int64 = PairCharInt64 
    fst (PairCharInt64 a _) = a
    snd (PairCharInt64 _ b) = b
    curry f x y = f (PairCharInt64 x y)

instance AdaptPair Char Word where
    type Pair Char Word = PairCharWord 
    fst (PairCharWord a _) = a
    snd (PairCharWord _ b) = b
    curry f x y = f (PairCharWord x y)

instance AdaptPair Char Word8 where
    type Pair Char Word8 = PairCharWord8 
    fst (PairCharWord8 a _) = a
    snd (PairCharWord8 _ b) = b
    curry f x y = f (PairCharWord8 x y)

instance AdaptPair Char Word16 where
    type Pair Char Word16 = PairCharWord16 
    fst (PairCharWord16 a _) = a
    snd (PairCharWord16 _ b) = b
    curry f x y = f (PairCharWord16 x y)

instance AdaptPair Char Word32 where
    type Pair Char Word32 = PairCharWord32 
    fst (PairCharWord32 a _) = a
    snd (PairCharWord32 _ b) = b
    curry f x y = f (PairCharWord32 x y)

instance AdaptPair Char Word64 where
    type Pair Char Word64 = PairCharWord64 
    fst (PairCharWord64 a _) = a
    snd (PairCharWord64 _ b) = b
    curry f x y = f (PairCharWord64 x y)

instance AdaptPair Char Double where
    type Pair Char Double = PairCharDouble 
    fst (PairCharDouble a _) = a
    snd (PairCharDouble _ b) = b
    curry f x y = f (PairCharDouble x y)

instance AdaptPair Char Float where
    type Pair Char Float = PairCharFloat 
    fst (PairCharFloat a _) = a
    snd (PairCharFloat _ b) = b
    curry f x y = f (PairCharFloat x y)

instance AdaptPair Char Char where
    type Pair Char Char = PairCharChar 
    fst (PairCharChar a _) = a
    snd (PairCharChar _ b) = b
    curry f x y = f (PairCharChar x y)

