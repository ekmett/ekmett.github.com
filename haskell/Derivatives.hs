{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-}
module Derivatives where
import Control.Monad.Identity
import Control.Arrow ((+++),(***),(&&&))
import Data.Monoid

infixl 9 :.:
infixl 7 :*:
infixl 6 :+: 

class Bifunctor f where
	bimap :: (a -> c) -> (b -> d) -> f a b -> f c d

instance Bifunctor (,) where
	bimap f g ~(a,b) = (f a, g b)

instance Bifunctor Either where
	bimap f _ (Left a) = Left (f a)
	bimap _ g (Right b) = Right (g b)

data Void 
instance Show Void where show _ = "Void"

newtype Const k a = Const { runConst :: k } deriving (Show)
type Zero = Const Void
type One = Const ()

newtype Lift p f g a = Lift { runLift ::  p (f a) (g a) }
type (:+:) = Lift Either
type (:*:) = Lift (,)

instance Show (p (f a) (g a)) => Show (Lift p f g a) where
	show (Lift x) = "(Lift (" ++ show x ++ "))"

newtype (f :.: g) a = Comp { runComp :: f (g a) } deriving (Show)

{-
type family D (f :: * -> *) :: * -> *
type instance D Identity = One
type instance D (Const k) = Zero
type instance D (f :+: g) = D f :+: D g
type instance D (f :*: g) = f :*: D g :+: D f :*: g
type instance D (f :.: g) = (D f :.: g) :*: D g
-}

class (Functor (D f), Functor f) => Differentiable (f :: * -> *) where
	type D f :: * -> *
	
instance Differentiable Identity where
	type D Identity = One

instance Differentiable (Const k) where
	type D (Const k) = Zero

instance (Differentiable f, Differentiable g) => Differentiable (f :+: g) where
	type D (f :+: g) = D f :+: D g 

instance (Differentiable f, Differentiable g) => Differentiable (f :*: g) where
	type D (f :*: g) = f :*: D g :+: D f :*: g

instance (Differentiable f, Differentiable g) => Differentiable (f :.: g) where
	type D (f :.: g) = (D f :.: g) :*: D g

instance Differentiable [] where
	type D [] = [] :*: []

instance Differentiable Maybe where
	type D Maybe = One -- D (Nothing | Just a) ~ D (Zero :+: Identity) = Zero :+: One = One

instance Differentiable (Either a) where
	type D (Either a) = One

instance Functor (Const k) where
	fmap f = Const . runConst

instance (Bifunctor p, Functor f, Functor g) => Functor (Lift p f g) where
	fmap f = Lift . bimap (fmap f) (fmap f) . runLift

instance (Functor f, Functor g) => Functor (f :.: g) where
	fmap f = Comp . fmap (fmap f) . runComp

newtype AD f a  = AD { runAD :: (f a,  AD (D f) a) } 

instance (Differentiable f, Functor (AD (D f))) => Functor (AD f) where
	fmap f = AD . bimap (fmap f) (fmap f) . runAD
	
