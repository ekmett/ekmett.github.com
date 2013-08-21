{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}
module Cube where

import Prelude hiding (pi)
import Exponential

data Sort = Star | Box deriving (Eq,Show,Read)
data F a = Lam a (a -> a) 
	 | Pi a (a -> a)
	 | App a a
	 | Sort Sort

instance ExpFunctor F where
	xmap f g (Lam t k) = Lam (f t) (f . k . g)
	xmap f g (Pi t k) = Pi (f t) (f . k . g)
	xmap f g (App a b) = App (f a) (f b)
	xmap f g (Sort k) = Sort k

instance Show (F a) where
	show (Lam a f) = "(LAM ...)"
	show (Pi a f) = "(PI ...)"
	show (App a b) = "(APP ...)"
	show (Sort Star) = "*"
	show (Sort Box) = "#"

lam :: Rollable F t => t -> (t -> t) -> t
lam t f = roll (Lam t f)

pi :: Rollable F t => t -> (t -> t) -> t
pi t f = roll (Pi t f)

app :: Rollable F t => t -> t -> t
app f a = roll (App f a)

star :: Rollable F t => t
star = roll (Sort Star)

box :: Rollable F t => t
box = roll (Sort Box)

-- Washburn/Weirich
safe :: (forall a. Elim F a) -> ForAll (Elim F)
safe = ForAll

-- Fegaras/Sheard
unsafe :: (forall a. Rec F a) -> ForAll (Rec F)
unsafe = ForAll

instance Cata F (ForAll t) => Show (ForAll t) where 
	show x = cata phi x vars where
		phi :: F ([String] -> String) -> [String] -> String
		phi (Lam t f) (v:vars) = "(lam " ++ v ++ " : " ++ t vars ++ ". " ++ f (const v) vars ++ ")"
		phi (Pi t f) (v:vars) = "(pi " ++ v ++ " : " ++ t vars ++ ". " ++ f (const v) vars ++ ")"
		phi (App a b) vars = "(" ++ a vars ++ " " ++ b vars ++ ")"
		phi (Sort Star) vars = "*"
		phi (Sort Box) vars = "#"
		vars :: [String] 
		vars = [ [i] | i <- ['a'..'z']] ++ [i : show j | j <- [1..], i <- ['a'..'z'] ]

-- polymorphic identity function
example_id :: Rollable F t => t
example_id = lam star $ \a -> lam a id

example_church_true :: Rollable F t => t
example_church_true  = lam star $ \a -> lam a $ \x -> lam a $ \y -> x

example_church_false :: Rollable F t => t
example_church_false = lam star $ \a -> lam a $ \x -> lam a $ \y -> y

-- cheezy pseudo alpha-equality until I define something better
instance Cata F (ForAll t) => Eq (ForAll t) where a == b = show a == show b

countBoundVars :: Cata F t => t -> Int
countBoundVars = cata phi where
	phi (Lam t f) = t + f 1
	phi (Pi t f) = t + f 1
	phi (Sort k) = 0
	phi (App a b) = a + b

-- a rather traditional evaluator, non-catamorphic, I don't seem to be able to derive this in the Weirich/Washburn setting.
-- maybe I am missing something, but it appears that this needs a paramorphism, and I can't build paramorphisms with just xmap!
eval :: (Unrollable F t, Rollable F t) => t -> t
eval x = case unroll x of 
	Lam t k -> lam t k
	Pi t k -> pi t k
	Sort Star -> star
	Sort Box -> box
	App f a -> 
		let f' = eval f in case unroll f' of
		Lam t k -> k a
		_ 	-> app f' a

