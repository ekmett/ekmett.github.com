module Control.Monad.CPS.Reader 
    ( module Control.Monad.Reader.Class
    , Reader(Reader, getReader)
    , runReader 
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader.Class

newtype Reader r a = Reader { getReader :: forall o. (a -> o) -> r -> o }

instance Functor (Reader r) where
    fmap f (Reader g) = Reader (\k -> g (\a -> k (f a)))

instance Monad (Reader r) where
    return a = Reader (\k _ -> k a)
    Reader g >>= f = Reader (\k r -> g (\a -> getReader (f a) k r) r)

runReader :: Reader r a -> r -> a
runReader (Reader g) = g id

instance MonadReader r (Reader r) where
    ask = Reader id
    local f (Reader g) = Reader (\k -> g k . f)
