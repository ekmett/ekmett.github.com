{-# LANGUAGE TypeOperators #-}
module Data.Rope.Annotation.Product
    ( (:*:)(..)
    , fstF
    , sndF
    ) where

import Control.Applicative hiding (empty)

import Data.Monoid (mappend)
import Data.Foldable (Foldable, foldMap)
import qualified Data.Foldable
import Data.Traversable (Traversable(traverse))

import Data.Rope.Annotation

infixr 5 :*:

-- | A 'Rope' 'Annotation' product.
data (f :*: g) a = f a :*: g a

fstF :: (f :*: g) a -> f a 
fstF ~(f :*: _) = f

sndF :: (f :*: g) a -> g a
sndF ~(_ :*: g) = g

instance (Functor f, Functor g)  => Functor (f :*: g) where
    fmap f (a :*: b) = fmap f a :*: fmap f b

instance (Applicative f, Applicative g) => Applicative (f :*: g) where
    pure a = pure a :*: pure a
    (f :*: g) <*> (a :*: b) = (f <*> a) :*: (g <*> b)

instance (Foldable f, Foldable g) => Foldable (f :*: g) where
    foldMap f (a :*: b) = foldMap f a `mappend` foldMap f b
    
instance (Traversable f, Traversable g) => Traversable (f :*: g) where
    traverse f (a :*: b) = (:*:) <$> traverse f a <*> traverse f b

instance (MonoidA f, MonoidA g) => MonoidA (f :*: g) where
    emptyA = emptyA :*: emptyA
    appendA r (a :*: a') s (b :*: b') = 
        appendA r a s b :*: appendA r a' s b'

instance (ReducerA f, ReducerA g) => ReducerA (f :*: g) where
    unitA r = unitA r :*: unitA r
    snocA r n (f :*: g) = snocA r n f :*: snocA r n g
    consA n r (f :*: g) = consA n r f :*: consA n r g

instance (BreakableA f, BreakableA g) => BreakableA (f :*: g) where
    dropA n r (f :*: g) = dropA n r f :*: dropA n r g
    takeA n r (f :*: g) = takeA n r f :*: takeA n r g
    splitAtA n r (f :*: g) = (f' :*: g' , f'' :*: g'') where
        (f',f'') = splitAtA n r f
        (g',g'') = splitAtA n r g
