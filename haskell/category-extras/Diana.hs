{-# OPTIONS_GHC -fglasgow-exts #-}
module Diana where

newtype Join f a = Join (f a a)
data Void 
data Coend f = forall a. Coend (f a a)
type End f = forall a. f a a
type Dialgebra f g a = f a -> g a

newtype Fix f = In { out :: f (Fix f) }

toNu :: Fix f -> Nu Identity f 
toNu = Nu (out . runIdentity) . Identity 

fromNu :: Nu Identity f -> Fix f
fromNu (Nu f a) = f (Identity a)

data Nu f g = forall a. Nu (f a -> g a) a

stripD :: (Functor f, Functor g) => Nu f g -> Colimit f -> g (Nu f g)
stripD (Nu f a) (Colim bs) = fmap (Nu f) (f (fmap (const a) bs))

class FinalDialgebra f g where
	outD :: f (Nu f g) -> g (Nu f g)

class InitialDialgebra f g where
	inD :: f (Mu f g) -> g (Mu f g)

-- lift the final coalgebra
outDIF :: Functor f => Identity (Nu Identity f) -> f (Nu Identity f) 
outDIF = fmap toNu . out . fromNu . runIdentity

-- lift the initial algebra
inDFI :: Functor f => f (Mu f Identity) -> Identity (Mu f Identity)
inDFI = Identity . toMu . In . fmap fromMu 

diana :: Dialgebra f g a -> a -> Nu f g
diana = Nu

postOut :: Dialgebra f g a -> f a -> g (Nu f g)
postOut f = fmap (diana f) . f

newtype Mu f g = Mu (forall a. (f a -> g a) -> a)

dicata :: Dialgebra f g a -> Mu f g -> a
dicata = Mu

preIn :: Dialgebra f g a -> f (Mu f g) -> g a
preIn f = f . fmap (dicata f)
