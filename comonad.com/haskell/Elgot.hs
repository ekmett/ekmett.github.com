module Elgot where

import Control.Arrow ((|||),(&&&),left, first)

newtype Mu f = InF { outF :: f (Mu f) }

hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo phi psi = h where h = phi . fmap h . psi

elgot :: Functor f => (f b -> b) -> (a -> Either b (f a)) -> a -> b
elgot phi psi = h where h = (id ||| phi . fmap h) . psi

coelgot :: Functor f => ((a, f b) -> b) -> (a -> f a) -> a -> b
coelgot phi psi = h where h = phi . (id &&& fmap h . psi)

apo :: Functor f => (a -> f (Either (Mu f) a)) -> a -> Mu f
apo psi = h . Right where h = InF . fmap h . (fmap Left . outF ||| psi)

g_apo :: Functor f => (b -> f b) -> (a -> f (Either b a)) -> a -> Mu f
g_apo g f = h . Right where h = InF . fmap h . (fmap Left . g ||| f)

elgot_apo :: Functor f => (a -> Either (Mu f) (f a)) -> a -> Mu f
elgot_apo psi = h where h = (id ||| InF . fmap h) . psi
