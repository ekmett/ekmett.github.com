{-# LANGUAGE ExistentialQuantification, ViewPatterns, FlexibleInstances, PatternGuards #-}

module Origami where

import Prelude hiding ((.),id)
import Control.Arrow
import Control.Comonad
import Control.Category 
import Control.Comonad.Density
import Control.Functor hiding (first,second)
import Control.Functor.Extras
import Control.Functor.Pointed
import Control.Applicative
import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
import Data.Traversable (Traversable)
import qualified Data.Traversable as Traversable
import Data.Monoid.Reducer

class Fold f where
    fold :: Foldable g => f a b -> g a -> b
    scan :: Traversable g => f a b -> g a -> g b
    comp :: f b c -> f a b -> f a c
    mapAccum :: Traversable g => f a b -> g a -> (b, g b)

    foldf :: Foldable g => f a b -> g a -> f a b
    scanf :: Traversable g => f a b -> g a -> (f a b, g b)
    
    reducer :: (c `Reducer` m) => f c m

    scan f = snd . mapAccum f
    -- horrible definition, avoid it!
    mapAccum f g = (fold f g, scan f g)

-- Foldl
newtype Moore a b = Moore { runMoore :: (b, a -> Moore a b) }

instance Functor (Moore a) where
    fmap f = Moore . (f *** fmap (fmap f)) . runMoore

stepMoore :: Moore a b -> a -> Moore a b
stepMoore = snd . runMoore

moore :: b -> (a -> Moore a b) -> Moore a b
moore a b = Moore (a,b)

instance Copointed (Moore a) where
    extract = fst . runMoore

instance Comonad (Moore a) where
    duplicate m = Moore (m, duplicate . stepMoore m)
    extend f m = Moore (f m, extend f . stepMoore m)

instance Fold Moore where
    fold m = extract . foldf m

    foldf = Foldable.foldl stepMoore

    m `comp` n = extract m `moore` \(stepMoore n -> n') -> stepMoore m (extract n') `comp` n'

    scan m = snd . scanf m

    scanf = Traversable.mapAccumL scan' where
        scan' n (stepMoore n -> n') = (n', extract n')

    mapAccum m = first extract . Traversable.mapAccumL scan' m where
        scan' n (stepMoore n -> n') = (n', extract n')

    reducer = step mempty where
        step e = Moore (e, step . snoc e)

instance Pointed (Moore a) where
    point x = p where p = Moore (x, const p)

instance Applicative (Moore a) where
    pure = point
    Moore (z,f) <*> Moore (z',f') = Moore (z z', liftA2 (<*>) f f')

-- Foldl1
newtype Mealy a b = Mealy { runMealy :: a -> (Mealy a b, b) } 

instance Functor (Mealy a) where
    fmap f (Mealy g) = Mealy $ (fmap f *** f) . g

instance Fold Mealy where
    fold m = snd . Foldable.foldl (runMealy . fst) (m, error "Mealy.fold: empty")
    comp = (.)
    scan m = snd . Traversable.mapAccumL runMealy m
    reducer = Mealy $ \(unit -> e') -> (step e', e') where
        step e = Mealy $ \(snoc e -> e') -> (step e', e')

instance Pointed (Mealy a) where
    point x = p where p = Mealy (const (p,x))

instance Applicative (Mealy a) where
    pure = point
    -- pointfree view-patterns!
    -- f <*> x = Mealy $ \(runMealy f &&& runMealy x -> ((f',f''),(x',x''))) -> (f' <*> x', f'' x'')
    Mealy f <*> Mealy x = Mealy $ \a -> 
        let (f', f'') = f a 
            (x', x'') = x a 
        in  (f' <*> x', f'' x'')

instance Category Mealy where
    id = Mealy ((,) id)
    g . f = Mealy $ \a -> 
        let (f', b) = runMealy f a
            (g', c) = runMealy g b
        in  (g' . f', c)

instance Monoid (Mealy a a) where
    mempty = id
    mappend = (.)

instance Arrow Mealy where
    arr f = f' where f' = Mealy $ \a -> (f', f a)
    first (Mealy f) = Mealy $ \(a,b) -> 
        let (f', f'') = f a 
        in  (first f', (f'', b))
    second (Mealy f) = Mealy $ \(a,b) -> 
        let (f', f'') = f b 
        in  (second f', (a, f''))
    Mealy f &&& Mealy g = Mealy $ \ a ->
        let (f', f'') = f a
            (g', g'') = g a
        in  (f' &&& g', (f'',g''))
    Mealy f *** Mealy g = Mealy $ \(a,b) ->
        let (f', f'') = f a
            (g', g'') = g b
        in  (f' *** g', (f'',g''))

instance ArrowChoice Mealy where
    left m = Mealy $ \a -> case a of 
        Left x  -> (left *** Left) $ runMealy m x
        Right y -> (left m, Right y)
    right m = Mealy $ \a -> case a of
        Left x  -> (right m, Left x)
        Right y -> (right *** Right) $ runMealy m y
    f ||| g = Mealy $ \a -> case a of
        Left x  | (f',x') <- runMealy f x -> (f' ||| g, x')
        Right y | (g',y') <- runMealy g y -> (f ||| g', y')
    f +++ g = Mealy $ \a -> case a of
        Left x  | (f',x') <- runMealy f x -> (f' +++ g, Left x')
        Right y | (g',y') <- runMealy g y -> (f +++ g', Right y')

        
-- contravariant Yoneda lemma applied to a non-"Functor" functor F a where F a c = F (a -> c -> c) c
-- an explicit Moore machine
data Foldr a b = forall c. Foldr (c -> b) (a -> c -> c) c

instance Functor (Foldr a) where
    fmap g (Foldr m f z) = Foldr (g . m) f z

instance Pointed (Foldr a) where
    point x = Foldr (const x) undefined undefined

instance Applicative (Foldr a) where
    pure = point
    Foldr m f z <*> Foldr m' f' z' = Foldr m'' f'' (z,z') where
        m'' (c,c') = m c (m' c')
        f'' a (c,c') = (f a c, f' a c')

instance Fold Foldr where
    fold (Foldr m f z) = m . Foldable.foldr f z
    Foldr m f z `comp` Foldr m' f' z' = Foldr (m . fst) f'' (z,z') where
        f'' a (c,d) | d' <- f' a d = (f (m' d') c, d')
    mapAccum (Foldr m f z) = first m . Traversable.mapAccumR scan' z where 
        scan' c a | c' <- f a c = (c',m c')
    reducer = Foldr id cons mempty

instance Copointed (Foldr a) where
    extract (Foldr m _ z) = m z

instance Comonad (Foldr a) where
    extend = flip Foldr stepFoldr

stepFoldr :: a -> Foldr a b -> Foldr a b
stepFoldr a (Foldr m f z) = Foldr m f (f a z)

-- contravaraiant Yoneda lemma applied to a non-"Functor" functor F a where F a c = F (a -> c -> c) (a -> c)
data Foldr1 a b = forall c. Foldr1 (c -> b) (a -> c -> c) (a -> c)

instance Functor (Foldr1 a) where
    fmap g (Foldr1 m f z) = Foldr1 (g . m) f z
    
instance Pointed (Foldr1 a) where
    point x = Foldr1 (const x) undefined undefined

instance Category Foldr1 where
    id = Foldr1 id const id
    Foldr1 m f z . Foldr1 m' f' z' = Foldr1 (m . fst) f'' z'' where
        z'' (z' -> b) = (z (m' b), b)
        f'' a (c,d) | d' <- f' a d = (f (m' d') c, d')

instance Arrow Foldr1 where
    arr f = f' where f' = Foldr1 f const id
    first (Foldr1 m f z) = Foldr1 (first m) f' (first z) where
        f' (a,b) (c,_) = (f a c, b)
    second (Foldr1 m f z) = Foldr1 (second m) f' (second z) where
        f' (a,b) (_,c) = (a, f b c)
    Foldr1 m f z *** Foldr1 m' f' z' = Foldr1 (m *** m') f'' (z *** z') where 
        f'' (a,b) (c,d) = (f a c, f' b d)
    Foldr1 m f z &&& Foldr1 m' f' z' = Foldr1 (m *** m') f'' (z &&& z') where
        f'' a (c,d) = (f a c, f' a d)

toFoldr1 :: Foldr a b -> Foldr1 a b 
toFoldr1 (Foldr m f z) = Foldr1 m f (flip f z)

runFoldr1 :: Foldr1 a b -> a -> (Foldr1 a b, b)
runFoldr1 (Foldr1 m f z) (z -> c) = (Foldr1 m f (flip f c), m c)

instance Fold Foldr1 where
    fold (Foldr1 m f z) = m' . Foldable.foldr f' Nothing where
        f' a Nothing = Just (z a)
        f' a (Just b) = Just (f a b)
        m' Nothing = error "Foldr1.fold: empty"
        m' (Just a) = m a
    comp = (.) 
    reducer = Foldr1 id cons unit
    scan m = snd . Traversable.mapAccumR runFoldr1 m
    
-- instance Applicative Foldr1
-- instance ArrowChoice Foldr1

-- Density comonad based Foldr
data DFoldr a b = forall c. DFoldr ((a -> c -> c) -> c -> b) (a -> c -> c) c

stepDFoldr :: a -> DFoldr a b -> DFoldr a b
stepDFoldr a (DFoldr m f z) = DFoldr m f (f a z)

instance Functor (DFoldr a) where
    fmap g (DFoldr m f z) = DFoldr (fmap g . m) f z 

instance Pointed (DFoldr a) where
    point x = DFoldr (\_ _ -> x) undefined undefined

instance Applicative (DFoldr a) where
    pure = point
    DFoldr m f z <*> DFoldr m' f' z' = DFoldr m'' f'' (z,z') where
        m'' _ (c,c') = m f c (m' f' c')
        f'' a (c,c') = (f a c, f' a c')

instance Fold DFoldr where
    fold (DFoldr m f z) = m f . Foldable.foldr f z
    DFoldr m f z `comp` DFoldr m' f' z' = DFoldr (const (m f . fst)) f'' (z,z') where
        f'' a (c,d) | d' <- f' a d = (f (m' f' d') c, d')
    mapAccum (DFoldr m f z) = first (m f) . Traversable.mapAccumR scan' z where 
        scan' c a | c' <- f a c = (c',m f c')
    reducer = DFoldr (const id) cons mempty

instance Copointed (DFoldr a) where
    extract (DFoldr m f z) = m f z

instance Comonad (DFoldr a) where
    extend = flip DFoldr stepDFoldr . const 


type Algebra f c = f c -> c

-- A density comonad of an arbitrary f-Algebra
data FoldF f b = forall c. FoldF (Algebra f c -> b) (Algebra f c)

instance Functor (FoldF f) where
    fmap g (FoldF m f) = FoldF (g . m) f
  
instance Pointed (FoldF f) where
    point x = FoldF (const x) (const x)

instance Copointed (FoldF f) where
    extract (FoldF m f) = m f

instance Comonad (FoldF f) where
    duplicate (FoldF m f) = FoldF (FoldF m) f
    extend g (FoldF m f) = FoldF (g . FoldF m) f
