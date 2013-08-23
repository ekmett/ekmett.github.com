{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Control.Monad.Ran.Writer 
    ( module Control.Monad.Writer.Class
    , Writer(Writer, getWriter)
    , runWriter
    , Writer'
    , runWriter'
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer.Class
import Data.Monoid (Monoid(..))

-- Ran transformed writer, with all of the traditional mempty mappend baggage
newtype Writer w a = Writer { getWriter :: forall o. (a -> w -> o) -> o }

instance Functor (Writer w) where
    fmap f (Writer g) = Writer (\k -> g (\a w -> k (f a) w))

instance Monoid w => Applicative (Writer w) where
    pure = return
    (<*>) = ap 

instance Monoid w => Monad (Writer w) where
    return a = Writer (\k -> k a mempty)
    Writer g >>= f = Writer (\k -> g (\a w -> getWriter (f a) (\a w' -> k a (w `mappend` w'))))

runWriter :: Writer w a -> (a, w)
runWriter (Writer g) = g (,)

instance Monoid w => MonadWriter w (Writer w) where
    tell w = Writer (\k -> k () w)
    listen (Writer f) = Writer (\g -> f (\a w -> g (a,w) w))
    pass (Writer f) = Writer (\g -> f (\(a,p) w -> g a (p w)))

-- writer as Ran transformed state, only pays for mappend when actually writing
newtype Writer' w a = Writer' { getWriter' :: forall o. (a -> w -> o) -> w -> o }

instance Monoid w => Functor (Writer' w) where
  fmap f (Writer' g) = Writer' (\k -> g (\a -> k (f a)))

instance Monoid w => Applicative (Writer' w) where
    pure = return
    (<*>) = ap 

instance Monoid w => Monad (Writer' w) where
  return a = Writer' (\k -> k a)
  Writer' g >>= f = Writer' (\k -> g (\a -> getWriter' (f a) k))

runWriter' :: Monoid w => Writer' w a -> (a, w)
runWriter' (Writer' g) = g (,) mempty

instance Monoid w => MonadWriter w (Writer' w) where
    tell w' = Writer' (\k w -> k () (w `mappend` w')) 
    listen (Writer' f) = Writer' (\k -> f (\a w -> k (a,w) w))
    pass (Writer' f) = Writer' (\k -> f (\(a,p) w -> k a (p w)))
