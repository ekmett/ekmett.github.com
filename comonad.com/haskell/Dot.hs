module Dot where

import Prelude hiding ((.),id,fmap)
import qualified Prelude
import Unsafe.Coerce

newtype Op p a b = Op { op :: p b a } 
newtype EP k a b = EP { embed :: k a b; project :: k b a } 

class (Category r, Category (~>) => PFunctor p r (~>) | p r -> (~>), p (~>) -> r where
	first :: r a b -> (p a c ~> p b c)
class (Category s, Category (~>) ) => QFunctor q s (~>) | q s -> (~>), q (~>) -> s where 
	second :: s a b -> (q c a ~> q c b)
class (Category r, Category s, Category (~>)) => Bifunctor q r s (~>) | k r -> s (~>), k s -> r (~>), k (~>) -> r s where
	bimap :: r a b -> s c d -> (q a c ~> q b d)
class QFunctor (~>) (~>) (->) => Category (~>) where 
	id :: a ~> a

(>>>) :: PFunctor p (Op (~>)) (~>) => (b ~> a) -> (p a c ~> p a b)
(>>>) = first . Op

contramap :: QFunctor p (Op (~>)) (~>) => (b ~> a) -> (p c a ~> p c b)
contramap = second . Op

xmap :: QFunctor p (EP (~>)) (~>) => (a ~> b) -> (b ~> a) -> (p c a ~> p c b)
xmap f g = second (EP f g)

fmap :: QFunctor q s (~>) => s a b -> (q c a ~> q c b)
fmap = second

type Hask = (->)
instance PFunctor (->) (Op Hask) Hask where
	first f h x = h (op f x)
instance QFunctor (->) Hask Hask where
	second g h x = g (h x)
instance Bifunctor (->) (Op Hask) Hask Hask where
	bimap f g h x = g (h (op f x))
instance Category Hask where
	id x = x

instance PFunctor (,) Hask Hask where
	first f ~(a,b) = (f a, b)
instance QFunctor (,) Hask Hask where
	second g ~(a,b) = (a, g b)
instance Bifunctor (,) Hask Hask Hask where
	bimap f g ~(a,b) = (f a, g b)

instance PFunctor Either Hask Hask where
	first f (Left a) = Left (f a)
	first _ (Right a) = Right a
instance QFunctor Either Hask Hask where
	second _ (Left a) = Left a
	second g (Right a) = Right (g a)
instance Bifunctor Either Hask Hask Hask where
	bimap f _ (Left a) = Left (f a)
	bimap _ g (Right a) = Right (g a)
	
newtype Fix p a b = Fix { break :: p b (Fix p a b) } 
instance (Bifunctor p r t t, Arr t) => QFunctor (Fix p) t t where
	second g = arr Fix . bimap g (second g) . arr break 
{-
instance PFunctor (Fix p) t t where
	first = const unsafeCoerce
instance (Bifunctor p r t t, Arr t) => Bifunctor (Fix p) t t t where
	bimap = const second 
-}

newtype Comp p q r a b = Comp { runComp :: p (q a b) (r a b) }
instance (Bifunctor p qt rt v, PFunctor q t qt, PFunctor r t rt, Arr v) => PFunctor (Comp p q r) t v where
	first f = arr Comp . bimap (first f) (first f) . arr runComp 
instance (Bifunctor p qt rt v, QFunctor q t qt, PFunctor r t rt, Arr v) => QFunctor (Comp p q r) t v where
	second g = arr Comp . bimap (second g) (second g) . arr runComp
	
newtype Const k a b = Const { runConst :: k }
type One = Const ()
one = Const ()

instance PFunctor (Const k) (->) (->) where
	first :: (a -> b) -> Const k a c -> Const k a b
	
type ListF = Comp Either (,) One
type List = forall a. Fix ListF a

cons a x = Comp $ Left (a, x)
nil = Comp $ Right one

foldr :: (a -> b -> b) -> b -> List a -> b



