{-# LANGUAGE TypeOperators #-}
module Mealy where

import Control.Applicative
import Control.Comonad
import Control.Arrow ((&&&),(***))

data Lead a b   = b :>- Follow a b
newtype Follow a b = Follow { runFollow :: a -> Lead a b } 
type (:>-) = Lead 
type (:->) = Follow

instance Show b => Show (Lead a b) where
    show (b :>- c) = show b ++ ":>- ..."

instance Functor (Lead a) where
    fmap f (a :>- b) = f a :>- fmap f b

instance Applicative (Lead a) where
    pure a = a :>- pure a
    (f :>- pf) <*> (a :>- pa) = f a :>- (pf <*> pa)

instance Functor (Follow a) where
    fmap f (Follow h) = Follow (fmap f . h)

instance Applicative (Follow a) where
    pure = Follow . const . pure
    h <*> k = Follow $ \a -> runFollow h a <*> runFollow k a

instance Copointed (Lead a) where
    extract (a :>- _) = a

instance Comonad (Lead a) where
    duplicate (a :>- f) = (a :>- f) :>- Follow (duplicate . runFollow f)

dance :: (a :>- b) -> (b :-> a) -> [(b, a)]
dance (b :>- Follow f) g = (b,c) : dance (f c) h where
       c :>- h = runFollow g b

lead :: (b :-> a) -> [b] -> [a]
lead _ [] = []
lead (Follow f) (x:xs) = y: lead f' xs
	where y :>- f' = f x

from :: Enum a => a -> Lead b a
from n = n :>- (Follow $ const $ from $ succ n)

echo :: Follow a a
echo = Follow (:>- echo)
