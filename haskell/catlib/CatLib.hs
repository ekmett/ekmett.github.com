{-# OPTIONS_GHC -fglasgow-exts #-}
import Prelude hiding (id,(.), Functor,fmap,map,return,(>>=),Monad)

data Op k a b = Op { runOp :: k b a } 

class Category (Cod f) => HasCod (f :: * -> *) where
	type Cod f :: * -> * -> *

class Category (~>) where
	id :: a ~> a
	(.) :: (b ~> c) -> (a ~> b) -> (a ~> c)

instance Category k => Category (Op k) where
	id = id
	Op f . Op g = Op (g . f)

-- the product category of two categories
data P k1 k2 a b where
	IdP :: P k1 k2 a a 
	P :: (a `k1` b) -> (c `k2` d) -> P k1 k2 (a,c) (b,d)

type family FstP (k :: * -> * -> *) :: * -> * -> * 
type instance FstP (P k1 k2) = k1
type family SndP (k :: * -> * -> *) :: * -> * -> * 
type instance SndP (P k1 k2) = k2

instance (Category k1, Category k2) => Category (P k1 k2) where
	id = IdP
	IdP . IdP = IdP
	IdP . P f g = P f g 
	P f g . IdP = P f g 
	P f g . P f' g' = P (f . f') (g . g')

class (Category (DomP f), HasCod f) => PFunctor f where
	type DomP :: * -> * -> * 
	first :: DomP f a b -> Cod f (f (a,c)) (f (b,c))

instance (Category k1, Category k2) => PFunctor (P k1 

class 

fstP :: Category k1 => P k1 k2 (a,b) (c,d) -> k1 a c
fstP IdP = id
fstP (P f _) = f

sndP :: Category k2 => P k1 k2 (a,b) (c,d) -> k2 b d
sndP IdP = id
sndP (P _ g) = g

diag :: k a b -> P k k (a,a) (b,b)
diag f = P f f 


first :: (P k1 k2 (a, c) (b, c) ~ Dom f a1 b1, Functor f) => k1 a b -> Cod f (f a1) (f b1)
--first :: (Functor f, Dom f ~ P k1 k2) => k1 a b -> Cod f (f (a,c)) (f (b,c))
first f = map (P f id)

--second :: (Functor f, Dom f ~ P k1 k2) => k2 a b -> Cod f (f (c,a)) (f (b,a))
--second f = map (P id f)

--bimap :: (Functor f, Dom f ~ P k1 k2) => k1 a b -> k2 c d -> Cod f (f (a,c)) (f (b,d))
--bimap f g = map (P f g)

class Category k => Cartesian k where
	type Prod k :: * -> * -> * 

class CCC k where
	type Exp k  :: * -> * -> *
	apply :: Prod k (Exp k a b) a `k` b

class (Category (Dom f), HasCod f) => Functor f where
	type Dom f :: * -> * -> * 
	map :: Dom f a b -> Cod f (f a) (f b)

class (Cod f ~ Dom f, Functor f) => Pointed f where
	return :: Cod f a (f a)
	
class Pointed m => Monad m where
	bind :: Dom m a (m b) -> Dom m (m a) (m b)

