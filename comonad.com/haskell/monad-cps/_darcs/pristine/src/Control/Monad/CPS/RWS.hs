{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Control.Monad.CPS.RWS 
    ( RWS(RWS,getRWS)
    , module Control.Monad.RWS.Class
    , runRWS
    ) where

import Data.Monoid (Monoid(..))
import Control.Applicative
import Control.Monad
import Control.Monad.RWS.Class

newtype RWS r w s a = RWS { getRWS :: forall o. (a -> w -> s -> o) -> r -> s -> o }

instance Monoid w => Functor (RWS r w s) where
    fmap f (RWS g) = RWS (\k -> g (\a -> k (f a)))

instance Monoid w => Applicative (RWS r w s) where
    pure = return
    (<*>) = ap

instance Monoid w => Monad (RWS r w s) where
    return a = RWS (\k _ -> k a mempty)
    RWS g >>= f = RWS (\k r -> g (\a w -> getRWS (f a) (\b w' -> k b (w `mappend` w')) r) r)

runRWS :: RWS r w s a -> r -> s -> (a, w, s)
runRWS (RWS f) = f (,,)

instance Monoid w => MonadState s (RWS r w s) where
    get = RWS (\ k r s -> k s mempty s)
    put s = RWS (\k _ _ -> k () mempty s)

instance Monoid w => MonadWriter w (RWS r w s) where
    tell w = RWS (\k _ -> k () w)
    listen (RWS f) = RWS (\k -> f (\a w -> k (a,w) w))
    pass (RWS f) = RWS (\k -> f (\(a,p) w -> k a (p w)))

instance Monoid w => MonadReader r (RWS r w s) where
    ask = RWS (\k r -> k r mempty)
    local f (RWS g) = RWS (\k r -> g k (f r))

instance Monoid w => MonadRWS r w s (RWS r w s)
