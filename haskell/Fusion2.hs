{-# OPTIONS -fglasgow-exts #-}

module Fusion where

import Control.Arrow ((&&&))
import Control.Monad.Identity

data Fix f = InF { outF :: f (Fix f) } 
data Build f  = Build  { runBuild  :: forall a. Algebra f a -> a }
data GBuild f = GBuild { runGBuild :: forall w a. Comonad w => Dist f w -> GAlgebra f w a -> w a}

class CanCata h where
	cata :: Functor f => Algebra f a -> h f -> a

class CanCata h => CanGCata h where
	gcata :: (Comonad w, Functor f) => Dist f w -> GAlgebra f w a -> h f -> a

class CanInF h where
	inF :: Functor f => f (h f) -> h f 

class (CanCata h, CanInF h) => CanPara h where
	para :: Functor f => GAlgebra f (Para h f) a -> h f -> a

class CanZygo h where
	zygo :: Functor f => Algebra f b -> GAlgebra f (Zygo b) a -> h f -> a

instance CanInF   Fix where inF = InF
instance CanCata  Fix where cata phi      = c where c = phi . fmap c . outF
instance CanPara  Fix where para phi   = snd . p where p = (inF . fmap fst &&& phi) . fmap p . outF
instance CanZygo  Fix where zygo g phi = snd . z where z = (g . fmap fst &&& phi) . fmap z . outF
instance CanGCata Fix where gcata k phi   = extract . c where c = liftW phi . k . fmap (duplicate . c) . outF

instance CanInF   Build where inF f       = Build (\phi -> phi $ fmap (cata phi) f)
instance CanCata  Build where cata        = flip runBuild
instance CanPara  Build where para phi    = snd . cata (inF . fmap fst &&& phi)
instance CanZygo  Build where zygo g phi  = snd . cata (g . fmap fst &&& phi)
instance CanGCata Build where gcata k phi = extract . cata (lowerGAlgebra k phi)

instance CanInF   GBuild where inF f = GBuild (\k phi -> liftW phi . k $ fmap (\x -> duplicate (runGBuild x k phi)) f)
instance CanCata  GBuild where cata = gcata distCata . liftAlgebra
instance CanPara  GBuild where para = gcata distPara
instance CanZygo  GBuild where zygo f = gcata (distZygo f)
instance CanGCata GBuild where gcata k phi x = extract $ runGBuild x k phi

distCata :: Functor f => Dist f Identity
distCata = Identity . fmap runIdentity

distPara :: (Functor f, CanInF h) => Dist f (Para h f)
distPara = distZygo inF

distZygo :: Functor f => Algebra f b -> Dist f (Zygo b) 
distZygo f = f . fmap fst &&& fmap snd

instance Functor (L a) where
	fmap f Nil = Nil
	fmap f (Cons a as) = Cons a (f as)

class CanList h where
	cons :: a -> h (L a) -> h (L a)
	nil :: h (L a)
	append :: h (L a) -> h (L a) -> h (L a) 

instance CanList Fix where
	nil = InF Nil
	cons a x = InF $ Cons a x
	append (InF Nil) bs = bs
	append (InF (Cons a as)) bs = InF . Cons a $ append as bs

instance CanList Build where 
	nil = Build (\phi -> phi Nil) 
	cons a x = Build (\phi -> phi . Cons a $ cata phi x)
	append as bs = Build (\phi -> cata (phi `withBase` cata phi bs) as) where
		withBase phi z Nil = z
		withBase phi _ x   = phi x

instance CanList GBuild where
	nil = GBuild (\k phi -> liftW phi (k Nil))
	cons a x = GBuild (\k phi -> lowerGAlgebra k phi . Cons a $ runGBuild x k phi)
	append as bs = GBuild (\k phi -> extract $ gcata k (phi `liftBase` runGBuild as k phi) bs) 
	    where
		liftBase _   w Nil = w
		liftBase phi _ x   = liftW phi x

data L a x = Cons a x | Nil
type BList a = Build (L a)
type GBList a = GBuild (L a)
type FList a = Fix (L a)

------- extras
newtype Yoneda f a = Yoneda (forall b. (a -> b) -> f b)
type Algebra f a = f a -> a
type GAlgebra f w a = f (w a) -> a
type Dist f w = forall a. f (w a) -> w (f a)
type Nat f g = forall a. f a -> g a 
type Para h f = (,) (h f)
type Zygo = (,)

class Functor w => Comonad w where
	extract :: w a -> a
	extend :: (w a -> b) -> w a -> w b
	duplicate :: w a -> w (w a)
	duplicate = extend id

instance Comonad Identity where
	extract = runIdentity	
	extend f = Identity . f
	duplicate = Identity

liftW :: Comonad w => (a -> b) -> w a -> w b
liftW f = extend (f . extract)

-- instance Functor ((,)e) where fmap f (e,a) = (e, f a)

instance Comonad ((,)e) where
	extract = snd
	extend f ea@(e,a) = (e, f ea)

liftAlgebra :: (Comonad w, Functor f) => Algebra f a -> GAlgebra f w a
liftAlgebra phi = phi . fmap extract

lowerGAlgebra :: (Comonad w, Functor f) => Dist f w -> GAlgebra f a -> Algebra f (w a)
lowerGAlgebra k phi = liftW phi . k . fmap duplicate

type (f :: * -> *) :<=: (g :: * -> *) = forall a b. (a -> b) -> f a -> g b
class HFunctor (h :: (* -> *) -> * -> *) where
	hmap :: (f :<=: g)  -> h f :<=: h g
