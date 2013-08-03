module Control.Monad.Ran.Cont 
    ( Cont(..)
    , runCont
    , module Control.Monad.Cont.Class
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Cont.Class
import Control.Monad.Ran
import Control.Monad.Ran.Codensity

-- traditional Cont derived as a codensity monad showing why the above is unnecessary
newtype Cont r a = Cont { getCont :: Codensity (Const r) a } 
    deriving (Functor,Applicative,Monad,RanIso (Const r) (Const r))

instance MonadCont (Cont r) where
     callCC f =  Cont (Codensity (\k -> getCodensity (getCont (f (\a -> Cont (Codensity (\_ -> Const (getConst ((k a)))))))) k))

runCont :: Cont r a -> (a -> r) -> r
runCont (Cont f) k = getConst (getCodensity f (Const . k))
