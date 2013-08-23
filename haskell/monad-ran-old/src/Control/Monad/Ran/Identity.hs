module Control.Monad.Ran.Identity 
    ( Identity'(Identity', getIdentity')
    , runIdentity'
    ) where

import Control.Monad
import Control.Monad.Identity
import Control.Applicative
import Control.Monad.Ran

newtype Identity' a = Identity' {getIdentity' :: forall o. (a -> o) -> o}

instance Functor Identity' where
    fmap f (Identity' g) = Identity' (\k -> g (k . f))

instance Applicative Identity' where
    pure = return
    (<*>) = ap

instance Monad Identity' where
    return a = Identity' (\k -> k a)
    Identity' g >>= f = Identity' (\k -> g (\a -> getIdentity' (f a) k))

runIdentity' :: Identity' a -> a
runIdentity' (Identity' f) = f id

-- Identity' is Ran Identity Identity
instance RanIso Identity Identity Identity' where
    toRan (Identity' f) = Ran (\b -> Identity (f (runIdentity . b)))
    fromRan (Ran f) = Identity' (\b -> runIdentity (f (Identity . b)))
