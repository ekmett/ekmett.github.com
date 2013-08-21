{-# OPTIONS -fglasgow-exts #-}			-- MPTC, Fundeps
{-# OPTIONS -fallow-undecidable-instances #-}	-- needed for all type LHSs
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Type.Binary.Internals
-- Copyright   :  (C) 2006-2007 Edward Kmett
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (FD and MPTC)
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
-- Reuses T and F from the Type.Boolean as the infinite tail of the 2s
-- complement binary number. 
--
-- TODO: TDivMod, TGCD
----------------------------------------------------------------------------

module Data.Type.Binary.Internals (
	O,
	I,
	-- T, tT,
	-- F, tF,
	TSucc, tSucc, tPred,
	TCBinary, TBinary, fromTBinary,
	-- TNot,
	TNeg, tNeg,
	TIsNegative, tIsNegative,
	TIsPositive, tIsPositive,
	TIsZero, tIsZero,
	TEven, tEven,
	TOdd, tOdd,
	TAdd, tAdd, tSub,
	TMul, tMul,
	TPow, tPow,
	-- TAnd, TOr, TXOr, TImplies,
	-- tAnd, tOr, tXOr, tImplies,
	TShift, tShift,
	TGetBit, tGetBit,
	TSetBit, tSetBit,
	TChangeBit, tChangeBit,
	TUnSetBit, tUnSetBit,
	TComplementBit, tComplementBit,
	TCountBits, tCountBits,	
	{- TReverse, tReverse, -}
	TAbs, tAbs,
	TNF, tNF,		-- put a number into normal form
	t2n, t2np1,		-- prepend a 0 or 1
	-- TEq, TLt, tEq, tLt,

	-- from Type.Sign
	Negative, Positive, SignZero,
	-- internal interfaces below here
	-- these require knowledge of the internal representation
	TShift', 		-- non-normalizing shifter
	TNF', 			-- case tracking normalizer for a number.
	TAddC',  		-- non-normalizing full-adder
	TAdd', tAdd',  		-- semi-adder
	TSub', tSub',  		-- semi-sub
	TCountBits',		-- sign-tracking intermediary for TCountBits
	LSB, tLSB, tBSL,	-- extract the LSB and tail of a number
	XI, XO,			-- indicates that the number can be extended
				-- by a I or O without leaving normal form
) where

import Data.Type.Boolean
import Data.Type.Ord
import Data.Type.Sign

data O a
data I a

-- | Internal closure, not exposed
data Closure
class Closed a | -> a
instance Closed Closure

-- | Extracts the least significant bit of a as d and returns a'.
-- Can also be used to prepend bit d onto a' obtaining a.
class (TBool d) => LSB a d a' | a -> d a', d a' -> a
instance LSB F F F
instance LSB T T T
instance LSB (O T) F T
instance LSB (I F) T F
instance LSB (O (O n)) F (O n)
instance LSB (O (I n)) F (I n)
instance LSB (I (O n)) T (O n)
instance LSB (I (I n)) T (I n)
tLSB :: LSB a d a' => a -> d -> a'; tLSB = undefined
tBSL :: LSB a d a' => a' -> d -> a; tBSL = undefined

-- | extract the lsb and assert we aren't at the long tail
class LSB a d a' => X a d a' | a -> d a', d a' -> a, a a' -> d
instance (LSB (O a) F a) => X (O a) F a
instance (LSB (I a) T a) => X (I a) T a

-- | assert 2n != n
class LSB (O a) F a => XO a
instance (LSB (O a) F a) => XO a

-- | assert 2n+1 != n
class LSB (I a) T a => XI a
instance (LSB (I a) T a) => XI a

-- | Finds the unique successor for any normalized binary number
class TSucc n m | n -> m, m -> n
instance TSucc T F
instance TSucc F (I F)
instance TSucc (O T) T
instance TSucc (O (I n)) (I (I n))
instance TSucc (O (O n)) (I (O n))
instance (TSucc n m, XI n, XO m) => TSucc (I n) (O m)
tSucc :: TSucc n m => n -> m; tSucc = undefined
tPred :: TSucc n m => m -> n; tPred = undefined

-- | Our set of digits is closed to retain the properties needed for most of the classes herein
class TCBinary c a | a -> c
instance TCBinary Closure F
instance TCBinary Closure T
instance (TCBinary c a, XO a) => TCBinary c (O a)
instance (TCBinary c a, XI a) => TCBinary c (I a)

-- | We don't want to have to carry the closure parameter around explicitly everywhere, so we
--   shed it here.
class TCBinary Closure a => TBinary a 		where fromTBinary :: Integral b => a -> b
instance TBinary F 				where fromTBinary _ = fromInteger 0
instance TBinary T 				where fromTBinary _ = fromInteger (-1)
instance (TBinary a, XO a) => TBinary (O a) 	where fromTBinary _ = let x = fromTBinary (undefined::a) in x+x
instance (TBinary a, XI a) => TBinary (I a) 	where fromTBinary _ = let x = fromTBinary (undefined::a) in succ(x+x)

-- | Show should express a value as legal haskell.
instance TBinary (O a) => Show (O a) where show n = "$(binaryE "++(show $ fromTBinary n)++")"
instance TBinary (I a) => Show (I a) where show n = "$(binaryE "++(show $ fromTBinary n)++")"

{-
instance Show (O F) where show n = "({-error-} O F)";
instance Show (I T) where show n = "({-error-} I T)";
instance Show (I F) where show n = "I F";
instance Show (O T) where show n = "O T";
instance (Show (I t)) => Show (O (I t)) where show n = "O (" ++ show (undefined::I t) ++ ")"
instance (Show (I t)) => Show (I (I t)) where show n = "I (" ++ show (undefined::I t) ++ ")"
instance (Show (O t)) => Show (O (O t)) where show n = "O (" ++ show (undefined::O t) ++ ")"
instance (Show (O t)) => Show (I (O t)) where show n = "I (" ++ show (undefined::O t) ++ ")"
-}

-- | TNot preserves normalization trivially
instance (TNot a b) => TNot (O a) (I b)
instance (TNot a b) => TNot (I a) (O b)

-- | TNeg obtains the 2s complement of a number and is reversible
class TNeg a b | a -> b, b -> a
instance (TNot a b, TSucc b c) => TNeg a c
tNeg :: TNeg a b => a -> b; tNeg = undefined

-- | Express a corrolary to the trichotomy law, every number is either negative, positive or zero.
class Trichotomy n s | n -> s
instance Trichotomy T Negative
instance Trichotomy F SignZero
instance Trichotomy (I F) Positive
instance Trichotomy (O T) Negative
instance (Trichotomy a b, XI a) => Trichotomy (I (I a)) b
instance (Trichotomy a b, XI a) => Trichotomy (O (I a)) b
instance (Trichotomy a b, XO a) => Trichotomy (I (O a)) b
instance (Trichotomy a b, XO a) => Trichotomy (O (O a)) b

-- | Returns true if the number is greater than zero
class TIsPositive n b | n -> b
instance (Trichotomy n s, TEq s Positive b) => TIsPositive n b
tIsPositive :: TIsPositive n b => n -> b; tIsPositive = undefined

-- | Returns true if the number is less than zero
class TIsNegative n b | n -> b
instance (Trichotomy n s, TEq s Negative b) => TIsNegative n b
tIsNegative :: TIsNegative n b => n -> b; tIsNegative = undefined

-- | Returns true if the number is equal to zero
class TIsZero n b | n -> b
instance (Trichotomy n s, TEq s SignZero b) => TIsZero n b
tIsZero :: TIsZero n b => n -> b; tIsZero = undefined

-- | Returns true if the lsb of the number is true
class TEven a b | a -> b
instance LSB a b c => TEven a b
tEven :: (TEven a b) => a -> b; tEven = undefined

-- | Returns true if the lsb of the number if false
class TOdd a b | a -> b
instance (LSB a b c, TNot b b') => TOdd a b'
tOdd :: (TOdd a b) => a -> b; tOdd = undefined

-- | A symmetrical full adder, that does not yield normal form answers.
class TAddC' a b c d | a b c -> d
instance TAddC' F F F F
instance TAddC' T F T F
instance TAddC' F T F T
instance TAddC' T T T T
instance TAddC' T F F T
instance TAddC' F T T F
instance TAddC' F F T (I F)
instance TAddC' T T F (O T)
instance TAddC' F (O a) F (O a)
instance TAddC' T (O a) T (O a)
instance TAddC' F (I a) F (I a)
instance TAddC' T (I a) T (I a)
instance TAddC' (O a) F F (O a)
instance TAddC' (O a) T T (O a)
instance TAddC' (I a) F F (I a)
instance TAddC' (I a) T T (I a)
instance TAddC' F (O a) T (I a)
instance TAddC' T (I a) F (O a)
instance TAddC' (O a) F T (I a)
instance TAddC' (I a) T F (O a)
instance TSucc a b => TAddC' F (I a) T (O b)
instance TSucc b a => TAddC' T (O a) F (I b)
instance TSucc a b => TAddC' (I a) F T (O b)
instance TSucc b a => TAddC' (O a) T F (I b)
instance TAddC' a b F c => TAddC' (O a) (O b) F (O c)
instance TAddC' a b F c => TAddC' (O a) (O b) T (I c)
instance TAddC' a b F c => TAddC' (I a) (O b) F (I c)
instance TAddC' a b T c => TAddC' (I a) (O b) T (O c)
instance TAddC' a b F c => TAddC' (O a) (I b) F (I c)
instance TAddC' a b T c => TAddC' (O a) (I b) T (O c)
instance TAddC' a b T c => TAddC' (I a) (I b) F (O c)
instance TAddC' a b T c => TAddC' (I a) (I b) T (I c)

-- | Transform a number into normal form, but track whether further reductions
-- may be necessary if this number is extended for efficiency.
class TNF' a b c | a -> b c
instance TNF' F F F
instance TNF' T T F
instance TNF' (O F) F F
instance TNF' (I T) T F
instance TNF' (I F) (I F) T
instance TNF' (O T) (O T) T
instance (TNF' (O a) c b) => TNF' (I (O a)) (I c) T
instance (TNF' (I a) c b) => TNF' (O (I a)) (O c) T
instance (TNF' (I a) c b, TIf b (I c) T d) => TNF' (I (I a)) d b
instance (TNF' (O a) c b, TIf b (O c) F d) => TNF' (O (O a)) d b

-- | Shed the additional reduction parameter from TNF'
class TNF a b | a -> b
instance TNF' a b c => TNF a b
tNF   :: TNF a b => a -> b; tNF = undefined
t2n   :: TNF (O a) b => a -> b; t2n = undefined
t2np1 :: TNF (I a) b => a -> b; t2np1 = undefined

-- | Equality comparison. Note this does not equate numbers that
-- are non-normalized with their normalized kin.
instance TEq (I m) (O n) F
instance TEq (O m) (I n) F
instance TEq (O m) F F
instance TEq (O m) T F
instance TEq (I m) T F
instance TEq (I m) F F
instance (TEq m n b) => TEq (I m) (I n) b
instance (TEq m n b) => TEq (O m) (O n) b

-- | We have a total order.
instance (TBool d, TNeg b b', TAdd' a b' c, TIsNegative c d) => TLt a b d

-- | Non-reversible addition. Kept for efficiency purposes.
class TAdd' a b c | a b -> c
instance (TAddC' a b F d, TNF d d') => TAdd' a b d'
tAdd' :: (TAdd' a b c ) => a -> b -> c; tAdd' = undefined

-- | Non-reversible subtraction. Kept for efficiency purposes.
class TSub' a b c | a b -> c
instance (TNeg b b', TAdd' a b' c) => TSub' a b c
tSub' :: TSub' a b c => a -> b -> c; tSub' = undefined

-- | Reversible adder with extra fundeps.
class TAdd a b c | a b -> c, a c -> b, b c -> a
instance (TAdd' a b c, TNeg b b', TAdd' c b' a, TNeg a a', TAdd' c a' b) => TAdd a b c
tAdd :: (TAdd a b c) => a -> b -> c; tAdd = undefined
tSub :: (TAdd a b c) => c -> a -> b; tSub = undefined

-- | Multiplication: a * b = c
class TMul a b c | a b -> c
instance TMul a F F
instance TNeg a b => TMul a T b
instance (TMul (O a) b c) => TMul a (O b) c
instance (TMul (O a) b c, TAdd' a c d) => TMul a (I b) d
tMul :: TMul a b c => a -> b -> c; tMul = undefined

-- | Exponentiation: a^b = c (only defined for non-negative exponents)
class TPow a b c | a b -> c
instance TPow a F (I F)
instance (TPow a k c, TMul c c d) => TPow a (O k) d
instance (TPow a k c, TMul c c d, TMul a d e) => TPow a (I k) e
tPow :: TPow a b c => a -> b -> c; tPow = undefined
{-

-- | Reverse the finite head of the number. non-normalizing, needs seed sign
class TReverse'' a b c | a b -> b
instance TReverse'' F b b
instance TReverse'' T b b
instance TReverse'' a (O b) c => TReverse (O a) b c
instance TReverse'' a (I b) c => TReverse (I a) b c

-- | Reverse the finite head of a number yielding a normal form answer
class TReverse' a b | a -> b
instance (TIsNegative a b, TReverse' a b c, TNF c c') => TReverse' a c'

-- | Reverse the finite head of a number, invertably
class TReverse a b | a -> b, b -> a
instance (TReverse' a b, TReverse' b a) => TReverse a b
tReverse :: TReverse a b => a -> b; tReverse = undefined


-}
-- | Return the absolute value of a
class TAbs a b | a -> b
instance (TIsNegative a s, TNeg a a', TIf s a' a a'') => TAbs a a''
tAbs :: TAbs a b => a -> b; tAbs = undefined

instance TAnd F (I b) F
instance TAnd F (O b) F
instance TAnd (I a) F F
instance TAnd (O a) F F
instance TAnd T (I b) (I b)
instance TAnd T (O b) (O b)
instance TAnd (I a) T (I a)
instance TAnd (O a) T (O a)
instance (TAnd a b c, TNF (I c) c') => TAnd (I a) (I b) c'
instance (TAnd a b c, TNF (O c) c') => TAnd (O a) (I b) c'
instance (TAnd a b c, TNF (O c) c') => TAnd (I a) (O b) c'
instance (TAnd a b c, TNF (O c) c') => TAnd (O a) (O b) c'

instance TOr F (I b) (I b)
instance TOr F (O b) (O b)
instance TOr (I a) F (I a)
instance TOr (O a) F (I a)
instance TOr T (I b) T
instance TOr T (O b) T
instance TOr (I a) T T
instance TOr (O a) T T
instance (TOr a b c, TNF (I c) c') => TOr (I a) (I b) c'
instance (TOr a b c, TNF (I c) c') => TOr (O a) (I b) c'
instance (TOr a b c, TNF (I c) c') => TOr (I a) (O b) c'
instance (TOr a b c, TNF (O c) c') => TOr (O a) (O b) c'

instance TXOr' F (I b) (I b)
instance TXOr' F (O b) (O b)
instance TXOr' (I b) F (I b)
instance TXOr' (O b) F (O b)
instance TNot b c => TXOr' T (I b) (O c)
instance TNot b c => TXOr' T (O b) (I c)
instance TNot b c => TXOr' (I b) T (O c)
instance TNot b c => TXOr' (O b) T (I c)
instance (TXOr' a b c, TNF (O c) c') => TXOr' (I a) (I b) c'
instance (TXOr' a b c, TNF (I c) c') => TXOr' (I a) (O b) c'
instance (TXOr' a b c, TNF (I c) c') => TXOr' (O a) (I b) c'
instance (TXOr' a b c, TNF (O c) c') => TXOr' (O a) (O b) c'

instance TImplies F (I b) T
instance TImplies F (O b) T
instance TImplies (I a) F T
instance TImplies (O a) F T
instance TImplies T (I b) (I b)
instance TImplies T (O b) (O b)
instance TImplies (I a) T (I a)
instance TImplies (O a) T (O a)
instance (TImplies a b c, TNF (I c) c') => TImplies (I a) (I b) c'
instance (TImplies a b c, TNF (I c) c') => TImplies (O a) (I b) c'
instance (TImplies a b c, TNF (I c) c') => TImplies (I a) (O b) c'
instance (TImplies a b c, TNF (O c) c') => TImplies (O a) (O b) c'

-- | Shift a right b places obtaining c. If b is negative then we shift left.
-- | TShift' does not yield normal form answers.
class TShift' a b c | a b -> c
instance TShift' F F F
instance TShift' T F T
instance TShift' (I a) F (I a)
instance TShift' (O a) F (O a)
instance TShift' (I a) T a
instance TShift' (O a) T a
instance TShift' F T F
instance TShift' T T T
instance (TShift' a b c, TShift' c b d) => TShift' a (O b) d
instance (TShift' a b c, TShift' c b d) => TShift' a (I b) (O d)

-- | Shift a right b places obtaining c in normal form.
-- | If b is negative then we shift left.
class TShift a b c' | a b -> c'
instance (TShift' a b c, TNF c c') => TShift a b c'
tShift :: TShift a b c => a -> b -> c; tShift = undefined

-- | get bit #b in a as c in {T,F}
class TGetBit a b c | a b -> c
instance (TNeg b b', TShift a b' c, LSB c d e) => TGetBit a b d
tGetBit :: TGetBit a b c => a -> b -> c; tGetBit = undefined

-- | set bit #b in a to T, yielding c.
class TSetBit a b c | a b -> c
instance (TShift (I F) b c, TOr a c d) => TSetBit a b d
tSetBit :: TSetBit a b c => a -> b -> c; tSetBit = undefined

-- | set bit #b in a to F, yielding c
class TUnSetBit a b c | a b -> c
instance (TShift (O T) b c, TAnd a c d) => TUnSetBit a b d
tUnSetBit :: TUnSetBit a b c => a -> b -> c; tUnSetBit = undefined

-- | change bit #b in a to c in {T,F}, yielding d.
class TChangeBit a b c d | a b c -> d
instance (TSetBit a b d, TUnSetBit a b e, TIf c d e f) => TChangeBit a b c f
tChangeBit :: TChangeBit a b c d => a -> b -> c -> d; tChangeBit = undefined

-- | toggle the value of bit #b in a, yielding c
class TComplementBit a b c | a b -> c
instance (TShift (I F) b c, TXOr' a c d) => TComplementBit a b d
tComplementBit :: TComplementBit a b c => a -> b -> c; tComplementBit = undefined

-- | Count the number of bits set, but track whether the number is positive or negative
-- to simplify casing. Since we may have an infinite tail of 1s, we return a negative
-- number in such cases indicating how many bits are NOT set.
class TCountBits' a b t | a t -> b
instance TCountBits' T T T
instance TCountBits' F F F
instance TCountBits' a n F => TCountBits' (O a) n F
instance TCountBits' a m F => TCountBits' (I a) m T
instance (TCountBits' a n F,TSucc n m) => TCountBits' (I a) m F
instance (TCountBits' a n F,TSucc m n) => TCountBits' (O a) n T

-- | Count the number of bits set. Since we may have an infinite tail of 1s, we return
-- a negative number in such cases indicating how many bits are NOT set.
class TCountBits a b | a -> b
instance (TIsNegative a t, TCountBits' a b t) => TCountBits a b
tCountBits :: TCountBits a b => a -> b; tCountBits = undefined
