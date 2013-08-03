{-# OPTIONS -fglasgow-exts #-}

module Fusion where

import Control.Functor.Fix
import Control.Functor.Extras
import Control.Functor.Algebra
import Control.Arrow ((&&&))
import Control.Monad.Identity
import Control.Monad.HigherOrder
import Control.Comonad.HigherOrder
import Control.Functor.Yoneda
import Control.Comonad
import Control.Comonad.Density
import Control.Monad.Codensity

type CYD w = CoYoneda (Density w)

data Build f  = Build  
    { runBuild  :: forall a. Algebra f a -> a }

data YBuild f  = YBuild  
    { runYBuild  :: forall a. Algebra (Yoneda f) a -> a }

data GBuild f = GBuild 
    { runGBuild :: forall w a. Comonad w => Dist f w -> GAlgebra f w a -> w a}

data YDBuild f = YDBuild { runYDBuild :: 
	forall w a. Dist (Yoneda f) (Density w) -> GAlgebra (Yoneda f) (Density w) a -> Density w a }
-- Dist (Y f) (D w)
-- forall a. Y f (D w a) -> D w (Y f a)
-- forall a. (forall b. (D w a -> b) -> f b) -> D w (forall c. (a -> c) -> f c)
-- forall a. (forall b. ((exists d. (w d -> a, w d)) -> b) -> f b) -> exists e. (w e -> forall c. (a -> c) -> f c, w e)
data YCYDBuild f = YCYDBuild { runYCYDBuild :: 
	forall w a. Dist (Yoneda f) (CYD w) -> GAlgebra (Yoneda f) (CYD w) a -> CYD w a }

-- Dist (Y f) (CYD w)
-- forall a. Y f (CYD w a) -> CYD w (Y f a)
-- forall a. (forall b. (CoYoneda (Density w) a -> b) -> f b) -> CoYoneda (Density w) (forall c. (a -> c) -> f c)
-- forall a. (forall b. ((exists d. (d -> a, Density w d)) -> b) -> f b) -> exists e. (e -> forall c. (a -> c) -> f c, Density w e)
-- forall a. (forall b. ((exists d. (d -> a, exists e. (w e -> d, w e))) -> b) -> f b) -> exists e. (e -> forall c. (a -> c) -> f c, exists g. (w g -> e, w g))


liftYoneda :: Functor f => f :~> Yoneda f
liftYoneda = hreturn

lowerCoYoneda :: Functor f => CoYoneda f :~> f
lowerCoYoneda = hextract

ayd :: Comonad w => (f (w a) -> b) -> Yoneda f (Density w a) -> b
ayd f y = f (runYoneda y lowerDensity)

dyd :: (Functor f, Comonad w) => Dist f w -> Dist (Yoneda f) (Density w)
dyd f x = Density (liftYoneda . extract) (ayd f x)
-- dyd f = distYoneda (distDensity f)

aycd :: Comonad w => (f (w a) -> b) -> Yoneda f (CYD w a) -> b
aycd f y = f (runYoneda y (lowerDensity . lowerCoYoneda))

dycd :: (Functor f, Comonad w) => Dist f w -> Dist (Yoneda f) (CYD w)
dycd f x = liftCoYoneda (Density (liftYoneda . extract) (aycd f x))

distYoneda :: (Functor w, Functor f) => Dist f w -> Dist (Yoneda f) w
distYoneda k = fmap liftYoneda . k . lowerYoneda

distCoYoneda :: (Functor m, Functor f) => Dist m f -> Dist m (CoYoneda f)
distCoYoneda k = liftCoYoneda . k . fmap lowerCoYoneda

distDensity :: (Functor f, Comonad w) => Dist f w -> Dist f (Density w)
distDensity k = liftDensity . k . fmap lowerDensity

distCodensity :: (Functor f, Monad m) => Dist m f -> Dist (Codensity m) f
distCodensity k = fmap liftCodensity . k . lowerCodensity

class CanCata h where
	cata :: Functor f => Algebra f a -> h f -> a

class CanCata h => CanGCata h where
	gcata :: (Comonad w, Functor f) => Dist f w -> GAlgebra f w a -> h f -> a
	gcata' :: (Comonad w, Functor f) => Dist f w -> GAlgebra f w a -> h f -> w a

	gcata k phi = extract . gcata' k phi

class CanInF h where
	inF :: Functor f => f (h f) -> h f 

class (CanCata h, CanInF h) => CanPara h where
	para :: Functor f => GAlgebra f (Para h f) a -> h f -> a

class CanZygo h where
	zygo :: Functor f => Algebra f b -> GAlgebra f (Zygo b) a -> h f -> a

instance CanInF   FixF where inF = InF
instance CanCata  FixF where cata phi      = c where c = phi . fmap c . outF
instance CanPara  FixF where para phi      = snd . p where p = (inF . fmap fst &&& phi) . fmap p . outF
instance CanZygo  FixF where zygo g phi    = snd . z where z = (g . fmap fst &&& phi) . fmap z . outF
instance CanGCata FixF where gcata' k phi  = c where c = fmap phi . k . fmap (duplicate . c) . outF

instance CanInF   Build where inF f        = Build (\phi -> phi $ fmap (cata phi) f)
instance CanCata  Build where cata         = flip runBuild
instance CanPara  Build where para phi     = snd . cata (inF . fmap fst &&& phi)
instance CanZygo  Build where zygo g phi   = snd . cata (g . fmap fst &&& phi)
instance CanGCata Build where gcata' k phi = cata (lowerAlgebra k phi)

-- instance CanInF   YBuild where inF f        = Build (\phi -> phi $ fmap (cata phi) f)
-- instance CanCata  YBuild where cata         = flip runBuild
-- instance CanPara  YBuild where para phi     = snd . cata (inF . fmap fst &&& phi)
-- instance CanZygo  YBuild where zygo g phi   = snd . cata (g . fmap fst &&& phi)
-- instance CanGCata YBuild where gcata' k phi = cata (lowerAlgebra k phi)

instance CanInF   GBuild where inF f          = GBuild (\k phi -> lowerAlgebra k phi $ fmap (gcata' k phi) f)
instance CanCata  GBuild where cata           = gcata distCata . liftAlgebra
instance CanPara  GBuild where para           = gcata distPara
instance CanZygo  GBuild where zygo f         = gcata (distZygo f)
instance CanGCata GBuild where gcata' k phi x = runGBuild x k phi

instance CanInF   YDBuild where inF f          = YDBuild (\k phi -> lowerAlgebra k phi . fmap (\x -> runYDBuild x k phi) $ hreturn f)
instance CanCata  YDBuild where cata           = gcata distCata . liftAlgebra
instance CanPara  YDBuild where para           = gcata distPara
instance CanZygo  YDBuild where zygo f         = gcata (distZygo f)
instance CanGCata YDBuild where 
	gcata' k phi x = lowerDensity $ runYDBuild x (dyd k) (ayd phi)
	gcata k phi x = extract $ runYDBuild x (dyd k) (ayd phi)

instance CanInF   YCYDBuild where inF f       = YCYDBuild (\k phi -> lowerAlgebra k phi . fmap (\x -> runYCYDBuild x k phi) $ hreturn f)
instance CanCata  YCYDBuild where cata        = gcata distCata . liftAlgebra
instance CanPara  YCYDBuild where para        = gcata distPara
instance CanZygo  YCYDBuild where zygo f      = gcata (distZygo f)
instance CanGCata YCYDBuild where 
	gcata' k phi x = lowerDensity $ lowerCoYoneda $ runYCYDBuild x (dycd k) (aycd phi)
	gcata k phi x = extract $ runYCYDBuild x (dycd k) (aycd phi)

distCata :: Functor f => Dist f Identity
distCata = Identity . fmap runIdentity

distPara :: (Functor f, CanInF h) => Dist f (Para h f)
distPara = distZygo inF

distZygo :: Functor f => Algebra f b -> Dist f (Zygo b) 
distZygo f = f . fmap fst &&& fmap snd

instance Functor (L a) where
	fmap f Nil = Nil
	fmap f (Cons a as) = Cons a (f as)

class CanCata h => CanList h where
	cons :: a -> h (L a) -> h (L a)
	nil :: h (L a)
	append :: h (L a) -> h (L a) -> h (L a) 

	head :: h (L a) -> a
	head = cata phi where phi (Cons a _) = a

instance CanList FixF where
	nil = InF Nil
	cons a x = InF $ Cons a x
	append (InF Nil) bs = bs
	append (InF (Cons a as)) bs = InF . Cons a $ append as bs
	head (InF (Cons a _)) = a

tail (InF (Cons _ as)) = as

instance CanList Build where 
	nil = Build (\phi -> phi Nil) 
	cons a x = Build (\phi -> phi . Cons a $ cata phi x)
	append as bs = Build (\phi -> cata (phi `withBase` cata phi bs) as) 

instance CanList GBuild where
	nil = GBuild (\k phi -> fmap phi (k Nil))
	cons a x = GBuild (\k phi -> lowerAlgebra k phi . Cons a $ runGBuild x k phi)
	append as bs = GBuild (\k phi -> let phi' = lowerAlgebra k phi in cata (phi' `withBase` gcata' k phi bs) as)

instance CanList YDBuild where
	nil = YDBuild (\k phi -> fmap phi (k (Yoneda (const Nil))))
	cons a x = YDBuild (\k phi -> lowerAlgebra k phi $ Yoneda (\f -> Cons a . f $ runYDBuild x k phi))
	-- append as bs = YDBuild (\k phi -> let phi' = lowerAlgebra k phi in cata (phi' `withBase'` runYDBuild bs k phi) as)

-- instance CanList PBuild where
--	nil = PBuild (\p phi -> fmap phi . p $ Nil)
--	cons a x = PBuild (\p phi -> fmap phi . p . Cons a $ runPBuild x k phi)
--	append as bs = PBuild (\p phi -> let 
--				phi' = (fmap phi . p) `withBase` base
--				p' Nil = base
--				base = runPBuild bs p phi
--			in runPBuild as p' phi' as)

data L a x = Cons a x | Nil
type BList a = Build (L a)
type GBList a = GBuild (L a)
type FList a = FixF (L a)

------- extras
type Zygo = (,)
type Para h f = Zygo (h f)

withBase phi z Nil = z
withBase phi _ x   = phi x

withBase' phi z x = case lowerYoneda x of Nil -> z; _ -> phi x

lowerAlgebra :: (Functor f, Comonad w) => Dist f w -> GAlgebra f w a -> Algebra f (w a)
lowerAlgebra k phi = fmap phi . k . fmap duplicate

type (f :: * -> *) :<=: (g :: * -> *) = forall a b. (a -> b) -> f a -> g b
class HFunctor (h :: (* -> *) -> * -> *) where
	hmap :: (f :<=: g)  -> h f :<=: h g
