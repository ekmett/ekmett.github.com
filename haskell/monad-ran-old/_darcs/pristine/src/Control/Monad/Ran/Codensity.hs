module Control.Monad.Ran.Codensity
    ( Codensity(Codensity, getCodensity)
    , runCodensity, runCodensityApp
    ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Ran
import Control.Applicative

newtype Codensity f a = Codensity { getCodensity :: forall o. (a -> f o) -> f o }

instance Functor (Codensity f) where
    fmap f (Codensity g) = Codensity (\k -> g (\a -> k (f a)))

instance Applicative (Codensity f) where
    pure = return
    (<*>) = ap 

instance Monad (Codensity f) where
    return a = Codensity (\k -> k a)
    Codensity g >>= f = Codensity (\k -> g (\a -> getCodensity (f a) k))

runCodensity :: Monad f => Codensity f a -> f a
runCodensity (Codensity f) = f return

runCodensityApp :: Applicative f => Codensity f a -> f a
runCodensityApp (Codensity f) = f pure

instance MonadTrans Codensity where
    lift m = Codensity (m >>=)

instance RanIso f f (Codensity f) where
    toRan x = Ran (getCodensity x)
    fromRan x = Codensity (getRan x)
