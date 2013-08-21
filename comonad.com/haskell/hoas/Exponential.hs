{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}
-- Control.Functor.Exponential
module Exponential where

import Control.Arrow ((&&&))
import Control.Monad.Cont (Cont(..), runCont)

-- | universally quantified $ so we can get away with fewer parentheses
($$) :: ((forall a. f a) -> b) -> (forall a. f a) -> b
f $$ x = f x
infixr 0 $$

newtype ForAll f = ForAll { runForAll :: forall a. f a } 

class ExpFunctor f where 
	xmap :: (a -> b) -> (b -> a) -> f a -> f b

-- | Every Functor is trivially an ExpFunctor
xmapDefault :: Functor f => (a -> b) -> (b -> a) -> f a -> f b 
xmapDefault = const . fmap

class Rollable f t | t -> f where 
	roll :: f t -> t

class Unrollable f t | t -> f where
	unroll :: t -> f t

class Placeable (r :: (* -> *) -> * -> *) where 
	place :: a -> r f a

class Cata f t | t -> f where 
	cata :: (f a -> a) -> t -> a

-- Meijer/Hutton
newtype Nu f = Nu { old :: f (Nu f) }
cataMH :: ExpFunctor f => (f a -> a) -> (a -> f a) -> Nu f -> a
cataMH f g = f . xmap (cataMH f g) (anaMH f g) . old
anaMH :: ExpFunctor f => (f a -> a) -> (a -> f a) -> a -> Nu f
anaMH f g = Nu . xmap (anaMH f g) (cataMH f g) . g

instance ExpFunctor f => Rollable f (Nu f) where 
	roll = Nu

instance Unrollable f (Nu f) where
	unroll = old

-- not Placeable

instance Functor f => Cata f (Nu f) where 
	cata f = f . fmap (cata f) . old

-- | Fegaras/Sheard
data Rec f a = Roll (f (Rec f a)) | Place a
cataFS :: ExpFunctor f => (f a -> a) -> Rec f a -> a
cataFS f (Roll x) = f (xmap (cataFS f) Place x)
cataFS f (Place x) = x

instance Rollable f (Rec f a) where 
	roll = Roll

instance Unrollable f (Rec f a) where
	unroll (Roll x) = x
	unroll (Place x) = undefined

instance Placeable Rec where 
	place = Place

instance ExpFunctor f => Cata f (ForAll (Rec f)) where
	cata f = cataFS f . runForAll

-- | Washburn/Weirich
newtype Elim f a = Elim { unElim :: Cont a (f a) } 
elim = Elim . Cont
runElim = runCont . unElim
cataWW :: ExpFunctor f => (f a -> a) -> Elim f a -> a
cataWW = flip runElim 

instance ExpFunctor f => Rollable f (Elim f a) where
	roll x = elim $ \f -> f $ xmap (cataWW f) place x
	
instance Placeable Elim where
	place = elim . const

instance Cata f (ForAll (Elim f)) where
	cata f x = runElim (runForAll x) f


-- | Change of representation
reroll :: (Cata f t, Rollable f t') => t -> t'
reroll = cata roll

-- to Meijer/Hutton
toMH :: (Cata f t, Functor f) => t -> Nu f
toMH = cata Nu

-- to Washburn/Weirich
toWW :: (Cata f t, ExpFunctor f) => t -> ForAll (Elim f)
toWW x = ForAll (reroll x)

-- to Fegaras/Sheard
toFS :: (Cata f t, ExpFunctor f) => t -> ForAll (Rec f)
toFS x = ForAll (reroll x)

safe :: (forall a. Elim f a) -> ForAll (Elim f)
safe = ForAll 

unsafe :: (forall a. Rec f a) -> ForAll (Rec f)
unsafe = ForAll 

-- | Washburn/Weirich safe open iteration for computing with 'holes'
class Iterable a f m n | m -> a n f where
	openiter :: (f a -> a) -> m -> n
	uniter   :: (f a -> a) -> n -> m

instance ExpFunctor f => Iterable a f (Rec f a) a where
	openiter = cataFS
	uniter = const place

instance Iterable a f (Elim f a) a where
	openiter = flip runElim
	uniter = const place

instance (Iterable a f m1 n1, Iterable a f m2 n2) => Iterable a f (m1 -> m2) (n1 -> n2) where
	openiter f x = openiter f . x . uniter f
	uniter f x = uniter f . x . openiter f

-- | exponential functor composition
newtype O f g e = Comp { deComp :: f (g e) }
instance (ExpFunctor f, ExpFunctor g) => ExpFunctor (f `O` g) where
	xmap f g = Comp . xmap (xmap f g) (xmap g f) . deComp

-- | paramorphism
para :: (Rollable f t, Cata f t, Functor f) => (f (t,a) -> a) -> t -> a
para = zygo roll

-- | zygomorphism
zygo :: (Cata f t, Functor f) => (f b -> b) -> (f (b,a) -> a) -> t -> a
zygo g f = snd . cata (g . fmap fst &&& f)
