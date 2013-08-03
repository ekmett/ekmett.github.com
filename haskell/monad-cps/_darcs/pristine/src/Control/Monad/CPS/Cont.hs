module Control.Monad.CPS.Cont 
    ( Cont
    , runCont
    , idCont
    , mapCont
    , withCont
    , shift
    , reset
    , Cont'(..)
    , runCont'
    , module Control.Monad.Cont.Class
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.CPS.Codensity
import Unsafe.Coerce
import Control.Monad.Cont.Class

newtype Cont r a = Cont {getCont :: forall o. (a -> r) -> (r -> o) -> o}

instance Functor (Cont r) where
    fmap f (Cont g) = Cont (\k -> g (k . f))

instance Applicative (Cont r) where
    pure = return
    (<*>) = ap

instance Monad (Cont r) where
    return a = Cont (\k z -> z (k a))
    Cont g >>= f = Cont (\k -> g (\a -> getCont (f a) k id))

runCont :: Cont r a -> (a -> r) -> r
runCont (Cont g) = flip g id

idCont :: Cont a a -> a
idCont = flip runCont id

mapCont :: (r -> r) -> Cont r a -> Cont r a
mapCont f (Cont g) = Cont (\k -> g (f . k))
-- mapCont f (Cont g) = Cont (\k z -> g k (z . f))

withCont :: ((b -> r) -> (a -> r)) -> Cont r a -> Cont r b
withCont f (Cont g) = Cont (g . f)
-- withCont f (Cont g) = Cont (\k -> g (f k))

instance MonadCont (Cont r) where
    callCC f = Cont (\k -> getCont (f (\a -> Cont (\_ h -> (h . k) a))) k)

shift  :: ((a -> Cont s r) -> Cont r r) -> Cont r a
shift f = Cont (\k -> getCont (f (\a -> Cont (\e h -> (h . e . k) a))) id)

reset :: Cont a a -> Cont r a
reset m = Cont (\k z -> (z . k) (runCont m id))

-- traditional Cont derived as a codensity monad showing why the above is unnecessary
newtype Cont' r a = Cont' { getCont' :: Codensity (Const r) a } 
    deriving (Functor,Applicative,Monad)

instance MonadCont (Cont' r) where
     callCC f =  Cont' (Codensity (\k -> getCodensity (getCont' (f (\a -> Cont' (Codensity (\_ -> Const (getConst ((k a)))))))) k))

runCont' :: Cont' r a -> (a -> r) -> r
runCont' (Cont' f) k = getConst (getCodensity f (Const . k))
