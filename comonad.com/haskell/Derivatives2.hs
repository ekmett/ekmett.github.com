{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-}
module Derivatives2 where
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
instance Functor (Const k) where
	fmap f = Const . runConst
type Zero = Const Void
zero :: Const Void a 
zero = Const undefined
newtype Succ f a = Succ { runSucc :: f a } 
suck = Succ
type One = Succ Zero

newtype Lift p f g a = Lift { runLift ::  p (f a) (g a) }
type (:+:) = Lift Either
type (:*:) = Lift (,)
instance Show (p (f a) (g a)) => Show (Lift p f g a) where
	show (Lift x) = "(Lift (" ++ show x ++ "))"
instance (Bifunctor p, Functor f, Functor g) => Functor (Lift p f g) where
	fmap f = Lift . bimap (fmap f) (fmap f) . runLift

newtype (f :.: g) a = Comp { runComp :: f (g a) } deriving (Show)
instance (Functor f, Functor g) => Functor (f :.: g) where
	fmap f = Comp . fmap (fmap f) . runComp

data D f g a = D (f a) (g a)

type family (:+>) (f :: * -> *) (g :: * -> *) :: * -> *
type family (:*>) (f :: * -> *) (g :: * -> *) :: * -> *
type family DLift (f :: (* -> *) -> * -> *) (f' :: (* -> *) -> * -> *) (g :: * -> *) :: * -> *
type instance (D x a :+> D y b) = D (x :+: y) (a :+: b)
type instance (D x a :*> D y b) = D (x :*: y) (x :*: b :+: a :*: y)
type instance DLift f f' (D x a) = D (f x) (a :+: f' x)

dConst :: a -> D Identity Zero a
dConst z = D (Identity z) zero

dVar :: a -> D Identity One a 
dVar z = D (Identity z) (suck zero)
