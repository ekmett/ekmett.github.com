{-# OPTIONS_GHC -fglasgow-exts #-}
module Toys
	( module Control.Functor.Adjunction
	, module Control.Functor.Strong
	, module Control.Category.Braided
	, module Control.Monad.Free
	, module Control.Comonad.Cofree
	, module Control.Category.Cartesian
	) where

import Control.Functor.Adjunction
import Control.Functor.Strong
import Control.Functor.Extras
import Control.Category.Braided
import Control.Category.Cartesian
import Control.Monad
import Control.Monad.Free
import Control.Comonad.Cofree
import Control.Morphism.Histo
import Control.Morphism.Futu
import Data.Traversable

zapWith :: Adjunction g f => (a -> b -> c) -> f a -> g b -> c
zapWith f a b = uncurry (flip f) . counit . fmap (uncurry (flip strength)) $ strength a b

zap :: Adjunction g f => f (a -> b) -> g a -> b
zap = zapWith id

ftw :: Adjunction g f => f a -> g b -> (a,b)
ftw a b = swap . counit . fmap (uncurry strength . swap) $ strength a b

wtf :: (Adjunction f g, Traversable f, Traversable g) => Either a b -> Either (f a) (g b) 
wtf = costrength . fmap (swap . costrength) . unit . swap 

cospin :: (RunComonadCofree f w, MonadFree f m) => w (m a) -> w (m a) 
cospin = fmap inFree . distHisto id . outCofree 

spin :: (ComonadCofree f w, RunMonadFree f m) => m (w a) -> m (w a)
spin = inFree . distFutu id . liftM outCofree
