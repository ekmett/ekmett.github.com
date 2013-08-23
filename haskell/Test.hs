{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts, MagicHash, UndecidableInstances, GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Test where

import Prelude hiding (curry,fst,snd,uncurry)
import qualified Prelude

class Tuple p where
    type Fst p :: *
    type Snd p :: *
    fst :: p -> Fst p
    snd :: p -> Snd p
    curry :: (p -> c) -> Fst p -> Snd p -> c

-- | Construct a new pair.
pair :: Tuple p => Fst p -> Snd p -> p
pair = curry id

-- | 'uncurry' converts a curried function to a function on pairs.
uncurry :: Tuple p => (Fst p -> Snd p -> c) -> (p -> c)
uncurry f p =  f (fst p) (snd p)

-- | Convert an adaptive pair to a regular polymorphic tuple
fromPair :: Tuple p => p -> (Fst p, Snd p)
fromPair = uncurry (,)

-- | Convert a regular polymorphic tuple to an adaptive pair
toPair :: Tuple p => (Fst p,Snd p) -> p
toPair (a,b) = pair a b


-- newtype wrapper to allow uniform derivations
newtype P a = P a deriving (Tuple)

instance (Bounded (Fst (P p)), Bounded (Snd (P p)), Tuple (P p)) => Bounded (P p) where
    minBound = pair minBound minBound
    maxBound = pair maxBound maxBound

instance (Eq (Fst (P p)), Eq (Snd (P p)), Tuple (P p)) => Eq (P p) where
    p == q = fst p == fst q && snd p == snd q

instance Tuple (a,b) where
    type Fst (a,b) = a
    type Snd (a,b) = b
    fst = Prelude.fst
    snd = Prelude.snd
    curry = Prelude.curry

data PUnit = PUnit

instance Tuple PUnit where
    type Fst PUnit = ()
    type Snd PUnit = ()
    fst _ = ()
    snd _ = ()
    curry f _ _ = f PUnit

data PBool = FF | FT | TF | TT

instance Tuple PBool where
    type Fst PBool = Bool
    type Snd PBool = Bool
    fst TT = True
    fst TF = True
    fst _ = False
    snd FT = True
    snd TT = True
    snd _ = False
    curry f True True = f TT
    curry f True False = f TF
    curry f False True = f FT
    curry f False False = f FF

-- now the class for pairs
class (Tuple (Pair a b)) => AdaptPair a b where
    type Pair a b :: *

--instance AdaptPair a b where
--    type Pair a b = P (a,b)

instance AdaptPair () () where
    type Pair () () = P PUnit

instance AdaptPair Bool Bool where
    type Pair Bool Bool = P PBool
