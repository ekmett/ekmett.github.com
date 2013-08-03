{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Control.Monad.Ran.State 
    ( module Control.Monad.State.Class
    , State
    , runState
    , State'
    , runState'
    ) where

import Control.Monad
import Control.Monad.Ran.Codensity
import Control.Monad.State.Class
import Control.Applicative

newtype State s a = State { getState :: forall o. (a -> s -> o) -> s -> o }

instance Functor (State s) where
  fmap f (State g) = State (\k -> g (\a -> k (f a)))

instance Applicative (State s) where
  pure = return
  (<*>) = ap

instance Monad (State s) where
  return a = State (\k -> k a)
  State g >>= f = State (\k -> g (\a -> getState (f a) k))

runState :: State s a -> s -> (a, s)
runState (State g) = g (,)

instance MonadState s (State s) where
    get   = State (\k s -> k s s)
    put s = State (\k _ -> k () s)

newtype State' s a = State' { getState' :: Codensity ((->)s) a }
    deriving (Functor,Applicative,Monad)

instance MonadState s (State' s) where
    get = State' (Codensity (\k s -> k s s))
    put s = State' (Codensity (\k s -> k () s))

runState' :: State' s a -> s -> (a, s)
runState' = flip getCodensity (,) . getState'
