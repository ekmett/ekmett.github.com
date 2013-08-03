{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}
module NarrowContextWithTypeFamilies where

import Control.Comonad
import Control.Functor.Pointed
import Control.Applicative
import Control.Comonad.Context
import Control.Arrow ((***),(+++))
import Data.Monoid


class Applicative ((:~*) e) => NarrowMemo e where
	data (:~*) e :: * -> *
	type D e :: *
	apply  :: (e :~* r) -> D e -> r
	memo   :: (D e -> r) -> (e :~* r) 
	domain :: D e -> e

instance NarrowMemo Bool where
	data Bool :~* a = MemoBool a a
	type D Bool = Bool
	apply (MemoBool o1 o2) True = o1 
	apply (MemoBool o1 o2) False = o2
	memo f = MemoBool (f True) (f False)
	domain = id

instance Functor ((:~*) Bool) where
	fmap f (MemoBool a b) = MemoBool (f a) (f b)

instance Applicative ((:~*) Bool) where
	pure x = MemoBool x x 
	MemoBool f g  <*> MemoBool a b = MemoBool (f a) (g b)

instance Pointed ((:~*) Bool) where
	point x = MemoBool x x

instance (NarrowMemo a, NarrowMemo b) => NarrowMemo (Either a b) where
	data Either a b :~* e = MemoEither (a :~* e) (b :~* e)
	type D (Either a b) = Either (D a) (D b)
	apply (MemoEither l _) (Left a) = apply l a
	apply (MemoEither _ r) (Right b) = apply r b
	memo f = MemoEither (memo (f . Left)) (memo (f . Right))
	domain = domain +++ domain

instance (NarrowMemo a, NarrowMemo b) => Functor ((:~*) (Either a b)) where
	fmap f (MemoEither a b) = MemoEither (fmap f a) (fmap f b)

instance (NarrowMemo a, NarrowMemo b) => Applicative ((:~*) (Either a b)) where
	pure f = MemoEither (pure f) (pure f)
	MemoEither f g <*> MemoEither a b = MemoEither (f <*> a) (g <*> b)


instance (NarrowMemo a, NarrowMemo b) => NarrowMemo (a,b) where
	data (a,b) :~* e = MemoBoth (a :~* (b :~* e))
	type D (a,b) = (D a, D b)
	apply (MemoBoth f) (a, b) = apply (apply f a) b
	memo f = MemoBoth $ memo $ \a -> memo (f . (,) a)
	domain = domain *** domain

instance (NarrowMemo a, NarrowMemo b) => Functor ((:~*) (a,b)) where
	fmap f (MemoBoth g) = MemoBoth (fmap (fmap f) g)

instance (NarrowMemo a, NarrowMemo b) => Applicative ((:~*) (a,b)) where
	pure = MemoBoth . pure . pure 
	f <*> a = MemoBoth $ memo $ \da -> memo $ \db -> let e = (da, db) in apply f e (apply a e)

data NarrowContext e a = NarrowContext (e :~* a) (D e)

instance NarrowMemo e => Functor (NarrowContext e) where
	fmap f (NarrowContext t d) = NarrowContext (memo (f . apply t)) d

instance NarrowMemo e => Copointed (NarrowContext e) where
	extract (NarrowContext t d) = apply t d

instance NarrowMemo e => Comonad (NarrowContext e) where
        duplicate (NarrowContext f a) = NarrowContext (memo (NarrowContext f)) a

instance NarrowMemo e => ComonadContext (D e) (NarrowContext e) where
	getC (NarrowContext _ e) = e
	modifyC f (NarrowContext g e) = apply g (f e)

instance (NarrowMemo e, Monoid (D e)) => Copointed ((:~*) e) where
	extract t = apply t mempty

instance (NarrowMemo e, Monoid (D e)) => Comonad ((:~*) e) where
	duplicate f = memo $ \m -> memo (apply f . mappend m)

instance NarrowMemo e => Pointed ((:~*) e) where
	point = pure

instance NarrowMemo e => Monad ((:~*) e) where
	return = pure
	m >>= k = memo $ apply (fmap k m) >>= apply
