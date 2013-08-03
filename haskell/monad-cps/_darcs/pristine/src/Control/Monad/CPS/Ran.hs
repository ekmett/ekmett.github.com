module Control.Monad.CPS.Ran
    ( Ran(Ran,getRan)
    ) where

newtype Ran g h a = Ran { getRan :: forall b. (a -> g b) -> h b } 

instance Functor (Ran g h) where
    fmap f m = Ran (\k -> getRan m (k . f))
