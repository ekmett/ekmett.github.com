{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}

-- Reflecting On Incremental Folds

module Incremental where

import Text.Show
import Text.Read
import Data.Reflection
import Data.Monoid.Reducer hiding (Sum,getSum,cons,ReducedBy(Reduction,getReduction))

class Functor f => Algebra f m where
    phi :: f m -> m

class Functor f => Coalgebra f m where
    psi :: m -> f m

instance Functor f => Algebra f () where
    phi _ = ()

-- F-Algebra product
instance (Algebra f m, Algebra f n) => Algebra f (m,n) where
    phi a = (phi (fmap fst a), phi (fmap snd a))

instance (Algebra f m, Algebra f n, Algebra f o) => Algebra f (m,n,o) where
    phi a = (phi (fmap f a),phi (fmap g a),phi (fmap h a)) where
        f (x,_,_) = x
        g (_,y,_) = y
        h (_,_,z) = z

instance (Algebra f m, Algebra f n, Algebra f o, Algebra f p) => Algebra f (m,n,o,p) where
    phi a = (phi (fmap f a),phi (fmap g a),phi (fmap h a), phi (fmap i a)) where
        f (x,_,_,_) = x
        g (_,y,_,_) = y
        h (_,_,z,_) = z
        i (_,_,_,w) = w

instance (Algebra f m, Algebra f n, Algebra f o, Algebra f p, Algebra f q) => Algebra f (m,n,o,p,q) where
    phi a = (phi (fmap f a),phi (fmap g a),phi (fmap h a), phi (fmap i a), phi (fmap j a)) where
        f (x,_,_,_,_) = x
        g (_,y,_,_,_) = y
        h (_,_,z,_,_) = z
        i (_,_,_,w,_) = w
        j (_,_,_,_,v) = v

newtype Mu f = In (f (Mu f))

instance (Eq (f (Mu f))) => Eq (Mu f) where
    In f == In g = f == g

instance (Ord (f (Mu f))) => Ord (Mu f) where
    In f `compare` In g = f `compare` g

instance (Show (f (Mu f))) => Show (Mu f) where
    showsPrec d (In f) = showParen (d > 10) $
        showString "In " . showsPrec 11 f

instance (Read (f (Mu f))) => Read (Mu f) where
    readPrec = parens . prec 10 $ do 
        Ident "In " <- lexP
        f <- step readPrec
        return (In f)

instance Functor f => Algebra f (Mu f) where
    phi = In

instance Functor f => Coalgebra f (Mu f) where
    psi (In x) = x

data (f :> m) = f (f :> m) :> m

instance (Eq m, Eq (f (f :> m))) => Eq (f :> m) where
    f :> m == g :> n = f == g && m == n
    f :> m /= g :> n = f /= g || m /= n
    
instance (Ord m, Ord (f (f :> m))) => Ord (f :> m) where
    (f :> m) `compare` (g :> n) | a == EQ = m `compare` n
                                | otherwise = a
        where a = f `compare` g

instance (Show m, Show (f (f :> m))) => Show (f :> m) where
    showsPrec d (f :> m) = showParen (d > 9) $
        showsPrec 10 f .
        showString " :> " .
        showsPrec 10 m

instance (Read m, Read (f (f :> m))) => Read (f :> m) where
    readPrec = parens $ prec 9 $ do
            f <- step readPrec
            Symbol ":>" <- lexP
            m <- step readPrec
            return (f :> m)

value :: (f :> m) -> m
value (_ :> m) = m

instance Algebra f m => Algebra f (f :> m) where
    phi x = x :> phi (fmap value x)

instance Functor f => Coalgebra f (f :> m) where
    psi (x :> _) = x

forget :: Functor f => (f :> m) -> f (f :> m)
forget = psi

remember :: Algebra f m => f (f :> m) -> f :> m
remember = phi

cata :: Algebra f a => (f :> m) -> a
cata = phi . fmap cata . forget

ana :: (Algebra f m, Coalgebra f a) => a -> (f :> m)
ana = remember . fmap ana . psi

tag :: Algebra f m => Mu f -> (f :> m)
tag = remember . fmap tag . psi

untag :: Functor f => (f :> m) -> Mu f 
untag = phi . fmap untag . forget

data Tree a r = Bin r a r | Tip
    deriving (Eq,Ord,Show,Read)

instance Functor (Tree a) where
    fmap f (Bin x a y) = Bin (f x) a (f y)
    fmap _ Tip = Tip

bin :: Algebra (Tree a) m => (Tree a :> m) -> a -> (Tree a :> m) -> (Tree a :> m)
bin a v b = remember (Bin a v b)

tip :: Algebra (Tree a) m => (Tree a :> m)
tip = remember Tip

testTree :: (Num a, Algebra (Tree a) m) => Tree a :> m
testTree = bin tip 2 (bin (bin tip 3 tip) 4 tip)

newtype Size = Size { getSize :: Int } deriving (Eq,Ord,Show,Read,Num)
newtype Sum = Sum { getSum :: Int } deriving (Eq,Ord,Show,Read,Num)

instance Algebra (Tree a) Size where
    phi (Bin x _ y) = x + 1 + y
    phi Tip = 0

instance Algebra (Tree Int) Sum where
    phi (Bin x y z) = x + Sum y + z
    phi Tip = 0

newtype Mon m = Mon { getMon :: m } deriving (Eq,Ord,Show,Read,Monoid)

instance (a `Reducer` m) => Algebra (Tree a) (Mon m) where
    phi (Bin x v y) = x `mappend` Mon (unit v) `mappend` y
    phi Tip = mempty

data List a r = Cons a r | Nil  
    deriving (Eq,Ord,Show,Read)

instance Functor (List a) where
    fmap f (Cons a x) = Cons a (f x)
    fmap _ Nil = Nil

instance Algebra (List a) Size where
    phi (Cons _ xs) = 1 + xs
    phi Nil = 0

instance Algebra (List Int) Sum where
    phi (Cons x xs) = fromIntegral x + xs
    phi Nil = 0

instance (a `Reducer` m) => Algebra (List a) (Mon m) where
    phi (Cons x xs) = Mon (unit x) `mappend` xs
    phi Nil = mempty

cons :: Algebra (List a) m => a -> (List a :> m) -> (List a :> m)
cons a b = remember (Cons a b)

nil :: Algebra (List a) m => List a :> m
nil = remember Nil

testList :: (Num a, Algebra (List a) m) => List a :> m
testList = cons 2 . cons 5 . cons 8 $ cons 27 nil

newtype (a `ReducedBy` s) = Reduction { getReduction :: a } 

instance (Functor f, s `Reflects` (f a -> a)) => Algebra f (a `ReducedBy` s) where
    phi = Reduction . reflect (undefined :: s) . fmap getReduction

filter_phi :: Algebra (List a) m => (a -> Bool) -> List a (List a :> m) -> List a :> m
filter_phi p Nil = nil
filter_phi p (Cons a as) | p a = cons a as
                         | otherwise = as

test :: List Int :> (Mon [Int],Size)
test = reify 
    (filter_phi (\x -> x `mod` 2 == 0)) 
    (\(_ :: s) -> getReduction (value (testList :: List Int :> ((List Int :> (Mon [Int],Size)) `ReducedBy` s))))

