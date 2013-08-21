{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Type.Boolean
-- Copyright   :  (C) 2006 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (FD and MPTC. no constructor data types)
--
-- Simple closed type-level booleans.
----------------------------------------------------------------------------

module Type.Boolean (
    TBool,
    F, T,
    tT, tF,
    TAnd, TOr, TNot, TXOr, TXOr', TImplies, TIf,
    tAnd, tOr, tNot, tXOr, tXOr', tImplies, tIf,
) where

data Closure
class Closed a | -> a
instance Closed Closure

data T; tT :: T; tT = undefined
data F; tF :: F; tF = undefined

-- |We have a closed set of possible booleans
class (Closed c) => TCBool c x | x -> c
instance TCBool Closure T
instance TCBool Closure F

-- |...and every boolean is in that set.
-- This lets us avoid carrying the closure parameter around
class (TCBool Closure x) => TBool x where fromTBool :: x -> Bool
instance TBool T where fromTBool _ = True
instance TBool F where fromTBool _ = False

-- | Let them be shown
instance Show T where show _ = "tT"
instance Show F where show _ = "tF"

-- | Type-Level a `and` b = c
class TAnd a b c | a b -> c
instance TAnd F F F
instance TAnd T F F
instance TAnd F T F
instance TAnd T T T
tAnd :: TAnd a b c => a -> b -> c
tAnd = undefined

-- | Type-Level a `or` b = c
class TOr a b c | a b -> c
instance TOr F F F
instance TOr F T T
instance TOr T F T
instance TOr T T T
tOr :: TOr a b c => a -> b -> c
tOr = undefined

-- | Type-Level: a `xor` b = c
class TXOr' a b c | a b -> c
instance TXOr' F F F
instance TXOr' F T T
instance TXOr' T F T
instance TXOr' T T F
tXOr' :: TXOr' a b c => a -> b -> c
tXOr' = undefined


-- | implemented this way rather than directly so that Binary can extend it properly.
-- otherwise the normal form restriction makes that nigh impossible.
class (TXOr' a b c, TXOr' b c a, TXOr' c a b) => TXOr a b c | a b -> c, a c -> b, b c -> a
instance (TXOr' a b c, TXOr' b c a, TXOr' c a b) => TXOr a b c
tXOr :: TXOr a b c => a -> b -> c
tXOr = undefined

-- | Type-Level: a `implies` b = c
class TImplies a b c | a b -> c
instance TImplies F F T
instance TImplies F T T
instance TImplies T F F
instance TImplies T T T
tImplies :: TImplies a b c => a -> b -> c
tImplies = undefined

-- | Type-Level: not a
class TNot a b | a -> b, b -> a
instance TNot T F
instance TNot F T
tNot :: TNot a b => a -> b
tNot = undefined

-- | Type-Level: if t then x else y
class TIf t x y z | t x y -> z where tIf :: t -> x -> y -> z
instance TIf F x y y where tIf _ _ y = y
instance TIf T x y x where tIf _ x _ = x
