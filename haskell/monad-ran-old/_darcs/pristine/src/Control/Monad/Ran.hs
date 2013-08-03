module Control.Monad.Ran
    ( Ran(Ran,getRan)
    , RanIso, toRan, fromRan
    ) where

-- | the right Kan extension of @h@ along @g@
newtype Ran g h a = Ran { getRan :: forall b. (a -> g b) -> h b } 

-- A right Kan extension is always a haskell Functor, even if @g@ and @h@ are not, by parametricity
instance Functor (Ran g h) where
    fmap f m = Ran (\k -> getRan m (k . f))

runRan :: Ran g h a -> (a -> g b) -> h b
runRan = getRan


class RanIso g h m | m -> g h where
    toRan :: m a -> Ran g h a 
    fromRan :: Ran g h a -> m a

{-
 -
class RanTrans g h where
    liftRan :: RanIso m => m a -> RanT g h m a
newtype RanT g h m a = RanT { getRanT :: Ran (g `O` G m) (h `O` H m) a } 
-- | @m@ is isomorphic to a right Kan extension of @h@ along @g@


newtype (g `O` f) a = Compose { decompose :: g (f a) }

instance (Functor g, Functor f) => Functor (g `O` f) where
    fmap f = Compose . fmap (fmap f) . decompose

inO :: (g (f a) -> g' (f' a')) -> ((g `O` f) a -> (g' `O` f') a')
inO = (Compose .).(. decompose)

inO2 :: (g (f a) -> g' (f' a') -> g'' (f'' a'')) -> ((g `O` f) a -> (g' `O` f') a' -> (g'' `O` f'') a'')
inO2 h = inO . h . decompose

instance (Applicative f, Applicative g) => Applicative (f `O` g) where
    pure = Compose . pure . pure
    (<*>) = inO2 (liftA2 (<*>)) 

instance (Monad m, RanIso m) => Monad (Ran (G m) (H m)

instance RanIso m => RanIso (RanT g h m) where
    type G (RanT g h m) = g `O` G m
    type H (RanT g h m) = h `O` H m
    toRan = getRanT
    fromRan = RanT

instance (Monad m, RanTrans g h) => Monad (RanT g h m) where
    return = liftRan . return
    m >>= k = close (open m >>= \x -> open (k x)
-}
