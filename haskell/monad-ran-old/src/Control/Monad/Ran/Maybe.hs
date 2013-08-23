module Control.Monad.Ran.Maybe
    ( Maybe'(Maybe',getMaybe')
    , runMaybe'
    ) where

import Control.Applicative
import Control.Monad

newtype Maybe' a = Maybe' { getMaybe' :: forall o. (a -> o) -> o -> o } 

runMaybe' :: Maybe' a -> (a -> b) -> b -> b
runMaybe' = getMaybe'

instance Functor Maybe' where
    fmap f (Maybe' g) = Maybe' (\k -> g (k . f))

instance Applicative Maybe' where
    pure = return
    (<*>) = ap

instance Monad Maybe' where
    return a = Maybe' (\k _ -> k a)
    Maybe' f >>= g = Maybe' (\k z -> f (\a -> getMaybe' (g a) k z) z)

-- TODO: Ran Identity Endo, a right Kan extension/Yoneda lemma of a non-Hask Functor!
