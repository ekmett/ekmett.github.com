module Control.Monad.CPS.Identity 
    ( Identity(Identity, getIdentity)
    , runIdentity
    ) where

import Control.Monad
import Control.Applicative

newtype Identity a = Identity {getIdentity :: forall o. (a -> o) -> o}

instance Functor Identity where
--  fmap f (Identity g) = Identity (\k -> g (\a -> k (f a)))
    fmap f (Identity g) = Identity (\k -> g (k . f))

instance Applicative Identity where
    pure = return
    (<*>) = ap

instance Monad Identity where
    return a = Identity (\k -> k a)
    Identity g >>= f = Identity (\k -> g (\a -> getIdentity (f a) k))

runIdentity :: Identity a -> a
runIdentity (Identity f) = f id
