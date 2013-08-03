{-# OPTIONS_GHC -fglasgow-exts #-}
module NarrowContext where

import Control.Comonad
import Control.Functor.Pointed
import Control.Applicative
import Control.Comonad.Context



class Applicative ((:~*) e) => NarrowMemo e where
	data (:~*) e :: * -> *
	data D e :: *
	apply  :: (e :~* r) -> D e -> r
	memo   :: (D e -> r) -> (e :~* r) 
	domain :: D e -> e

instance NarrowMemo Bool where
	data Bool :~* a = MemoBool a a
	data D Bool = DTrue | DFalse
	apply (MemoBool o1 o2) DTrue = o1 
	apply (MemoBool o1 o2) DFalse = o2
	memo f = MemoBool (f DTrue) (f DFalse)
	domain DTrue = True
	domain DFalse = False

instance Functor ((:~*) Bool) where
	fmap f (MemoBool a b) = MemoBool (f a) (f b)

instance Applicative ((:~*) Bool) where
	pure x = MemoBool x x 
	MemoBool f g  <*> MemoBool a b = MemoBool (f a) (g b)

instance Pointed ((:~*) Bool) where
	point x = MemoBool x x




instance (NarrowMemo a, NarrowMemo b) => NarrowMemo (Either a b) where
	data Either a b :~* e = MemoEither (a :~* e) (b :~* e)
	data D (Either a b) = DLeft (D a) | DRight (D b)
	apply (MemoEither l _) (DLeft a) = apply l a
	apply (MemoEither _ r) (DRight b) = apply r b
	memo f = MemoEither (memo (f . DLeft)) (memo (f . DRight))
	domain (DLeft a) = Left (domain a)
	domain (DRight b) = Right (domain b)

instance (NarrowMemo a, NarrowMemo b) => Functor ((:~*) (Either a b)) where
	fmap f (MemoEither a b) = MemoEither (fmap f a) (fmap f b)

instance (NarrowMemo a, NarrowMemo b) => Applicative ((:~*) (Either a b)) where
	pure f = MemoEither (pure f) (pure f)
	MemoEither f g <*> MemoEither a b = MemoEither (f <*> a) (g <*> b)



instance (NarrowMemo a, NarrowMemo b) => NarrowMemo (a,b) where
	data (a,b) :~* e = MemoBoth (a :~* (b :~* e))
	data D (a,b) = DBoth (D a) (D b)
	apply (MemoBoth f) (DBoth a b) = apply (apply f a) b
	memo f = MemoBoth $ memo $ \a -> memo (f . DBoth a)
	domain (DBoth a b) = (domain a, domain b)

instance (NarrowMemo a, NarrowMemo b) => Functor ((:~*) (a,b)) where
	fmap f (MemoBoth g) = MemoBoth (fmap (fmap f) g)

instance (NarrowMemo a, NarrowMemo b) => Applicative ((:~*) (a,b)) where
	pure = MemoBoth . pure . pure 
	f <*> a = MemoBoth $ memo $ \da -> memo $ \db -> let e = DBoth da db in apply f e (apply a e)



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
