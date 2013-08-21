{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}
module FreeExpression where

import Control.Monad
import Control.Monad.Identity
import Control.Arrow ((&&&), (***),(+++), (|||))

-- The cofree comonad of a functor
newtype Cofree f a = Cofree { runCofree :: (a, f (Cofree f a)) }

instance Functor f => Functor (Cofree f) where
        fmap f = Cofree . (f *** fmap (fmap f)) . runCofree

anaCofree :: Functor f => (a -> c) -> (a -> f a) -> a -> Cofree f c
anaCofree h t = Cofree . (h &&& fmap (anaCofree h t) . t)


-- | The free monad of a functor
newtype Free f a = Free { runFree :: Either a (f (Free f a)) }

instance Functor f => Functor (Free f) where
        fmap f = Free . (f +++ fmap (fmap f)) . runFree


instance Functor f => Monad (Free f) where
        return = Free . Left
        m >>= k = (k ||| (inFree . fmap (>>= k))) (runFree m)

inFree :: f (Free f a) -> Free f a
inFree = Free . Right

cataFree :: Functor f => (c -> a) -> (f a -> a) -> Free f c -> a
cataFree l r = (l ||| r . fmap (cataFree l r)) . runFree

-- | Functor Duality
class Dual f g | f -> g, g -> f where
	zap :: (a -> b -> c) -> f a -> g b -> c

(>$<) :: Dual f g => f (a -> b) -> g a -> b
(>$<) = zap id

instance Dual Identity Identity where
	zap f (Identity a) (Identity b) = f a b 

data (f :+: g) a = Inl (f a) | Inr (g a)
data (f :*: g) a = Prod (f a) (g a)

instance (Functor f, Functor g) => Functor (f :+: g) where
	fmap f (Inl x) = Inl (fmap f x)
	fmap f (Inr y) = Inr (fmap f y)
	
instance (Functor f, Functor g) => Functor (f :*: g) where
	fmap f (Prod x y) = Prod (fmap f x) (fmap f y)

instance (Dual f f', Dual g g') => Dual (f :+: g) (f' :*: g') where
	zap op (Inl f) (Prod a _) = zap op f a
	zap op (Inr f) (Prod _ b) = zap op f b

instance (Dual f f', Dual g g') => Dual (f :*: g) (f' :+: g') where
	zap op (Prod f _) (Inl a) = zap op f a
	zap op (Prod _ g) (Inr b) = zap op g b

-- | Bifunctor Duality
class BiDual p q | p -> q, q -> p where
	bizap :: (a -> c -> e) -> (b -> d -> e) -> p a b -> q c d -> e

(>>$<<):: BiDual p q => p (a -> c) (b -> c) -> q a b -> c
(>>$<<) = bizap id id

instance BiDual (,) Either where
	bizap l r (f,g) (Left a)  = l f a 
	bizap l r (f,g) (Right b) = r g b

instance BiDual Either (,) where
	bizap l r (Left f) (a,b)  = l f a
	bizap l r (Right g) (a,b) = r g b


-- Duality between Free and Cofree Functors
instance Dual f g => Dual (Cofree f) (Free g) where
	zap op (Cofree fs) (Free as) = bizap op (zap (zap op)) fs as

instance Dual f g => Dual (Free f) (Cofree g) where
	zap op (Free fs) (Cofree as) = bizap op (zap (zap op)) fs as


-- Examples
type Nat a = Free Identity a
type Stream a = Cofree Identity a

suck :: Nat a -> Nat a
suck = inFree . Identity

ints :: Stream Int
ints = anaCofree id (\x -> return (x + 1)) 0

ten :: Int
ten = ((*2),(+3)) >>$<< Left 5

two :: Int
two = suck (suck (return id)) >$< ints


four :: Float
four = Left (/2) >>$<< (8.0, True)


data Incr t = Incr Int t
data Recall t = Recall (Int -> t)

instance Functor Incr where
	fmap f (Incr i t) = Incr i (f t)

instance Functor Recall where
	fmap f (Recall g) = Recall (f . g)

(/+/) :: (Functor f, Functor g) => (f a -> a) -> (g a -> a) -> ((f :+: g) a -> a)
(f /+/ g) (Inl a) = f a
(f /+/ g) (Inr b) = g b 

newtype Mem = Mem Int
type RunAlg f a = f (Mem -> (a,Mem)) -> Mem -> (a,Mem)

incrRun :: RunAlg Incr a
incrRun (Incr k r) (Mem i) = r (Mem (i + k))

recallRun :: RunAlg Recall a
recallRun (Recall r) (Mem i) = r i (Mem i)

run :: Functor f => RunAlg f a -> Free f a -> Mem -> (a, Mem)
run = cataFree (,)

runMyExpr :: Free (Incr :+: Recall) a -> Mem -> (a, Mem)
runMyExpr = run (incrRun /+/ recallRun) 
