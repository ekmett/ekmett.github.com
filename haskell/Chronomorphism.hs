{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Chronomorphism
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
--
-- Defines generalized hylomorphisms, chronomorphisms, futumorphisms and 
-- dynamorphisms
----------------------------------------------------------------------------
module Chronomorphism where

import Control.Monad.Error
import Control.Monad
import Control.Arrow ((|||), (&&&), (+++), (***))

class Functor w => Comonad w where
	duplicate :: w a -> w (w a)
	extend :: (w a -> b) -> w a -> w b
	extract :: w a -> a
	extend f = fmap f . duplicate
	duplicate = extend id

-- distributie law
type Dist f g = forall a. f (g a) -> g (f a)

-- natural transformation
type Natural f g = forall a. f a -> g a

liftW :: Comonad w => (a -> b) -> w a -> w b
liftW f = extend (f . extract)

newtype Mu f = InF { outF :: f (Mu f) }
type Nu f = Mu f

-- | hylomorphism
hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo f g = f . fmap (hylo f g) . g

-- | generalized hylomorphism
g_hylo :: (Comonad w, Functor f, Monad m) =>
          (forall a. f (w a) -> w (g a)) -> 
	  (forall a. m (e a) -> f (m a)) -> 
	  (g (w b) -> b) -> 
	  (a -> e (m a)) -> 
          (a -> b)
g_hylo w m f g = extract . g_hylo' w m f g . return

-- | the kernel of the generalized hylomorphism
g_hylo' :: (Comonad w, Functor f, Monad m) =>
           (forall a. f (w a) -> w (g a)) -> 
	   (forall a. m (e a) -> f (m a)) -> 
	   (g (w b) -> b) -> 
	   (a -> e (m a)) -> 
           (m a -> w b)
g_hylo' w m f g = liftW f . w . fmap (duplicate . g_hylo' w m f g . join) . m . liftM g


-- | A naive metamorphism
meta :: (Functor f, Functor g) => (a -> f a) -> (g a -> a) -> Mu g -> Nu f
meta f g = ana f . cata g

-- | generalized metamorphism
g_meta :: (Monad m, Functor f, Comonad w, Functor g) =>
          Dist m f ->
          Dist g w ->
          (a -> f (m a)) ->
          (g (w a) -> a) ->
          Mu g -> Nu f
g_meta m w f g = g_ana m f . g_cata w g


-- | catamorphism
cata :: Functor f => (f a -> a) -> Mu f -> a
cata f = hylo f outF

-- | generalized catamorphism
g_cata :: (Functor f, Comonad w) => 
	  Dist f w ->
	  (f (w a) -> a) ->
	  Mu f -> a
g_cata k f = extract . g_cata' k f 
-- g_cata k f = g_hylo k distId f (fmap Id . outF)

-- | the kernel of a generalized catamorphism
g_cata' :: (Functor f, Comonad w) => 
	  Dist f w ->
	  (f (w a) -> a) ->
	  Mu f -> w a
g_cata' k f = liftW f . k . fmap (duplicate . g_cata' k f) . outF  
-- g_cata' k f = cata (liftW f . k . fmap duplicate)

zygo :: Functor f => (f b -> b) -> (f (b,a) -> a) -> Mu f -> a
zygo g = g_cata (g . fmap fst &&& fmap snd)

-- generalized zygomorphism
g_zygo :: (Comonad w, Functor f) => 
	  Dist f w ->
	  (f (w b) -> b) -> 
	  (f (w (b,a)) -> a) -> 
          Mu f -> a
g_zygo k g f = snd . g_cata k (g . fmap (liftW fst) &&& f)

para :: Functor f => (f (Mu f,a) -> a) -> Mu f -> a
para = zygo InF

-- | anamorphism
ana :: Functor f => (a -> f a) -> a -> Nu f
ana g = hylo InF g

-- | generalized anamorphism
g_ana :: (Functor f, Monad m) =>
	 Dist m f -> 
	 (a -> f (m a)) ->
	 a -> Nu f
g_ana k f = g_ana' k f . return
-- g_ana k f = g_hylo undistId k (InF . fmap runId) f

-- | the kernel of a generalized anamorphism
g_ana' :: (Functor f, Monad m) =>
	 Dist m f ->
	 (a -> f (m a)) ->
	 m a -> Nu f
g_ana' k f = ana (fmap join . k . liftM f)

apo :: Functor f => (a -> f (Either (Nu f) a)) -> a -> Nu f 
apo = g_apo outF

g_apo :: Functor f => (b -> f b) -> (a -> f (Either b a)) -> a -> Nu f
g_apo g f = ana (fmap Left . g ||| f) . Right
-- g_apo g = g_ana (fmap Left . g ||| fmap Right) -- not valid Haskell, Either is only a monad if left side is Error instance

-- a generalized generalized apomorphism. 
-- an example would be a futumorphism that may also return an apomorphic infinite tail
-- does anyone else feel like we're doing a lot of boilerplate here?
gg_apo :: (Monad m, Functor f) => 
	  Dist m f ->
	  (b -> f (m b)) -> 
	  (a -> f (m (Either b a))) -> 
          a -> Nu f
gg_apo k g f = g_ana k (fmap (liftM Left) . g ||| f) . Right

-- | The free monad of a functor
data Free f a = Free { runFree :: Either a (f (Free f a)) }

instance Functor f => Functor (Free f) where
        fmap f = Free . (f +++ fmap (fmap f)) . runFree

instance Functor f => Monad (Free f) where
        return = Free . Left
        m >>= k = (k ||| (inFree . fmap (>>= k))) (runFree m)

inFree :: f (Free f a) -> Free f a
inFree = Free . Right

-- | Fegaras/Sheard catamorphism with a fused fmap 
cataFree :: Functor f => (c -> a) -> (f a -> a) -> Free f c -> a
cataFree l r = (l ||| r . fmap (cataFree l r)) . runFree

-- | lift a distributive law for a functor to the free monad of the functor
distFree :: 	(Functor f, Functor h) => Dist h f -> Dist (Free h) f
distFree k = cataFree (fmap return) (fmap inFree . k)

-- | futumorphism: cocourse of argument coiteration
futu :: Functor f => (a -> f (Free f a)) -> a -> Nu f
futu f = futu' f . return 

-- | the kernel of a futumorphism
futu' :: Functor f => (a -> f (Free f a)) -> Free f a -> Nu f
futu' f = ana ((f ||| id) . runFree)

-- | generalized futumorphism: an obvious generalization that doesn't appear in literature
g_futu :: (Functor f, Functor h) => 
	  Dist h f ->
	  (a -> f (Free h a)) ->
	  a -> Nu f
g_futu k = g_ana (distFree k)

-- | the kernel of a generalized futumorphism
g_futu' :: (Functor f, Functor h) => 
	   Dist h f ->
	   (a -> f (Free h a)) ->
	   Free h a -> Nu f
g_futu' k = g_ana' (distFree k)

-- | The cofree comonad of a functor (aka the branching stream comonad)
data Cofree f a = Cofree { runCofree :: (a, f (Cofree f a)) }

instance Functor f => Functor (Cofree f) where
	fmap f = Cofree . (f *** fmap (fmap f)) . runCofree

instance Functor f => Comonad (Cofree f) where
	extract = fst . runCofree
	extend f = Cofree . (f &&& (fmap (extend f) . outCofree))

outCofree :: Cofree f a -> f (Cofree f a)
outCofree = snd . runCofree

-- | Fegaras/Sheard anamorphism with an fused fmap aka genStrf
anaCofree :: Functor f => (a -> c) -> (a -> f a) -> a -> Cofree f c
anaCofree h t = Cofree . (h &&& fmap (anaCofree h t) . t)

-- | lift a distributive law for a functor to the cofree comonad of the functor
distCofree :: (Functor f, Functor h) => Dist f h -> Dist f (Cofree h)
distCofree k = anaCofree (fmap extract) (k . fmap outCofree)


cofreeToList :: Cofree Maybe a -> [a]
cofreeToList (Cofree (x, xs)) = x : maybe [] cofreeToList xs

-- | histomorphism: course of value coiteration
histo :: Functor f => (f (Cofree f a) -> a) -> Mu f -> a
histo f = extract . histo' f 

-- | histomorphism: course of value coiteration
histo' :: Functor f => (f (Cofree f a) -> a) -> Mu f -> Cofree f a
histo' f = cata (Cofree . (f &&& id))

-- | generalized histomorphism
g_histo :: (Functor f, Functor h) => 
           Dist f h ->
	   (f (Cofree h a) -> a) ->
	   Mu f -> a
g_histo k = g_cata (distCofree k) 

-- | generalized histomorphism kernel
g_histo' :: (Functor f, Functor h) => 
           Dist f h ->
	   (f (Cofree h a) -> a) ->
	   Mu f -> Cofree h a
g_histo' k = g_cata' (distCofree k) 

-- | dynamorphism
dyna :: Functor f => 
	(f (Cofree f b) -> b) -> 
	(a -> f a) -> 
	(a -> b)
dyna f g = extract . dyna' f g 

-- | dynamorphism kernel
dyna' :: Functor f => 
	(f (Cofree f b) -> b) -> 
	(a -> f a) -> 
	(a -> Cofree f b)
--dyna' f g = hylo (Cofree . (f &&& id)) g
dyna' f g = chrono' f (fmap return . g) . return

-- | generalized dynamorphism
g_dyna :: (Functor f, Functor h, Monad m) => 
	Dist f h ->
	Dist m f ->
	(f (Cofree h b) -> b) -> 
	(a -> f (m a)) -> 
	(a -> b)
g_dyna k = g_hylo (distCofree k)

-- | generalized dynamorphism kernel
g_dyna' :: (Functor f, Functor h, Monad m) => 
	Dist f h -> 
	Dist m f -> 
	(f (Cofree h b) -> b) -> 
	(a -> f (m a)) -> 
	(m a -> Cofree h b)
g_dyna' k = g_hylo' (distCofree k)

-- | subsumes histo-, futu-, and dyna- morphisms. I'm calling it a chronomorphism.
chrono :: Functor f => 
	  (f (Cofree f b) -> b) -> 
          (a -> f (Free f a)) -> 
          a -> b
chrono = g_chrono id id

-- | chronomorphism kernel
chrono' :: Functor f => 
	  (f (Cofree f b) -> b) -> 
          (a -> f (Free f a)) -> 
          Free f a -> Cofree f b
chrono' = g_chrono' id id

-- | Generalized chronomorphism
g_chrono :: (Functor f, Functor m, Functor w) =>
	    Dist f w ->
	    Dist m f ->
	    (f (Cofree w b) -> b) ->
	    (a -> f (Free m a)) ->
	    a -> b
g_chrono w m = g_hylo (distCofree w) (distFree m)

-- | Generalized chronomorphism kernel
g_chrono' :: (Functor f, Functor m, Functor w) =>
	    Dist f w ->
	    Dist m f ->
	    (f (Cofree w b) -> b) ->
	    (a -> f (Free m a)) ->
	    Free m a -> Cofree w b
g_chrono' w m = g_hylo' (distCofree w) (distFree m)

-- | Generalized chronomorphism with a natural transformation
g_chronoEta :: (Functor f, Functor g, Functor m, Functor w) =>
	    Dist g w ->
	    Dist m f ->
	    (g (Cofree w b) -> b) ->
	    Natural f g ->
	    (a -> f (Free m a)) ->
	    a -> b
g_chronoEta w m f eta g = g_hylo (distCofree w . eta) (distFree m) f g

-- | Generalized chronomorphism with a natural transformation kernel
g_chronoEta' :: (Functor f, Functor g, Functor m, Functor w) =>
	    Dist g w ->
	    Dist m f ->
	    (g (Cofree w b) -> b) ->
	    Natural f g -> 
	    (a -> f (Free m a)) ->
	    Free m a -> Cofree w b
g_chronoEta' w m f eta g = g_hylo' (distCofree w . eta) (distFree m) f g 

-- | The identity monad/comonad
data Id a = Id { runId :: a }
instance Functor Id where
	fmap f = Id . f . runId

instance Monad Id where
	return = Id
	Id a >>= f = f a

instance Comonad Id where
	extract = runId
	extend f x = Id (f x)
	duplicate = Id

distId :: Functor f => Id (f a) -> f (Id a)
distId = fmap Id . runId

undistId :: Functor f => f (Id a) -> Id (f a)
undistId = Id . fmap runId 

-- one in Control.Monad.Instances
-- instance Functor ((,)e) where
--       fmap f (e,a) = (e,f a)

instance Comonad ((,)e) where
        extract = snd
        duplicate (e,a) = (e,(e,a))

instance Error (Nu f) where
	noMsg = error "Nu"
	strMsg = error

{- 
-- utilities, requires -fallow-undecidable-instances
instance (Show a, Show (f (Free f a))) => Show (Free f a) where
    show (Free x) = "(Free " ++ show x ++ ")"

instance (Show a, Show (f (Cofree f a))) => Show (Cofree f a) where
    show (Cofree x) = "(Cofree (" ++ show x ++ "))"

instance (Eq a, Eq (f (Free f a))) => Eq (Free f a) where
    Free x == Free y = x == y 

instance (Eq a, Eq (f (Cofree f a))) => Eq (Cofree f a) where
    Cofree x == Cofree y = x == y

instance Show (f (Mu f)) => Show (Mu f) where
    show (InF x) = "(InF (" ++ show x ++ "))"
-}
