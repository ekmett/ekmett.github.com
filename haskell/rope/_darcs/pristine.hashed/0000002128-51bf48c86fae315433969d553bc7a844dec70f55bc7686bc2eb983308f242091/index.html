{-# LANGUAGE TypeOperators, FlexibleInstances, MultiParamTypeClasses #-}
module Data.Rope.Util.Product
    ( (:*:)(..)
    , Product(..), bothC
    , unzip
    ) where

import Data.Monoid hiding (Product(..))
import Data.Rope.Util.Comonad
import Data.Rope.Util.Bifunctor
import Data.Rope.Util.Reducer
import Prelude hiding (fst, snd, curry, uncurry, either, unzip)
import qualified Prelude as P

-- a product that is strict in both arguments
data a :*: b = !a :*: !b

instance Bifunctor (:*:) where
    first f (a :*: b) = f a :*: b
    second f (a :*: b) = a :*: f b
    bimap f g (a :*: b) = f a :*: g b

instance Functor ((:*:) a) where
    fmap f (a :*: b) = a :*: f b

instance Comonad ((:*:) a) where
    extract (_ :*: b) = b
    extend f ab@(a :*: _) = a :*: f ab
    duplicate ab@(a :*: _) = a :*: ab

instance (Monoid a, Monoid b) => Monoid (a :*: b) where
    mempty = mempty :*: mempty
    (a :*: b) `mappend` (c :*: d) = mappend a c :*: mappend b d

instance (Reducer c a, Reducer c b) => Reducer c (a :*: b) where
    unit c = unit c :*: unit c
    cons c (a :*: b) = cons c a :*: cons c b
    snoc (a :*: b) c = snoc a c :*: snoc b c 

class Bifunctor p => Product p where
    fst :: p a b -> a
    snd :: p a b -> b
    pair :: a -> b -> p a b
    curry :: (p a b -> c) -> a -> b -> c
    uncurry :: (a -> b -> c) -> p a b -> c
    both :: (a -> b) -> (a -> c) -> a -> p b c
    diag :: a -> p a a

    diag = both id id
    both f g = bimap f g . diag

instance Product (:*:) where
    fst (a :*: _) = a
    snd (_ :*: b) = b
    pair = (:*:)
    curry f a b = f (a :*: b)
    uncurry f (a :*: b) = f a b
    both f g a = f a :*: g a
    diag a = a :*: a

instance Product (,) where
    fst = P.fst
    snd = P.snd
    pair = (,)
    curry = P.curry
    uncurry = P.uncurry
    both f g a = (f a, g a)
    diag a = (a, a)

unzip :: (Product p, Functor f) => f (p a b) -> p (f a) (f b)
unzip = both (fmap fst) (fmap snd)
{-# INLINE unzip #-}

-- CoKleisli (&&&)
bothC :: (Product p, Functor f) => (f a -> b) -> (f c -> d) -> f (p a c) -> p b d
bothC f g = both (f . fmap fst) (g . fmap snd)
{-# INLINE bothC #-}
