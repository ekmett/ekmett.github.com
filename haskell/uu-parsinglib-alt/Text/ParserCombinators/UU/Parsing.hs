{-# LANGUAGE  RankNTypes, 
              GADTs,
              MultiParamTypeClasses,
              FunctionalDependencies, 
              FlexibleInstances, 
              FlexibleContexts, 
              UndecidableInstances,
              NoMonomorphismRestriction,
              PatternGuards
    #-}

--------------------------------------------------------------------
-- |
-- Module      : Text.ParserCombinators.UU.Parsing
-- Copyright   : 2001-2009 Doaitse Swierstra, 2009 Edward Kmett
-- License     : LGPL 
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GADTs, MPTCs, Fundeps, RankNTypes)
--
--------------------------------------------------------------------
 
module Text.ParserCombinators.UU.Parsing 
    ( 
      -- Functor
      (<$>)
      -- Applicative
    , Applicative, (<*>), pure
    , pReturn
      -- Alternative
    , Alternative, (<|>), empty
    , pFail
      -- Symbol
    , Symbol, pSym
    , Provides, splitState
    , Eof, eof, deleteAtEnd
    , Parser, parse
    , History(..)
    , Future(..)
    , P(..)
    , fail
    , best
    , Steps(..)
    , Cost
    , Progress
    , noAlts
    , eval, push, apply, normalize
    , GenMonad, (>>>=)
    , Greedy, (<<|>)
    , Ambiguous, amb
    , getCheapest
    , Stores, getErrors
    , AsksFor, pErrors, pEnd
    , Switch, switch
    , R(..)
    , ExtApplicative, (<*), (*>), (<$)
    ) where

import Prelude hiding (fail)

import Control.Applicative (Applicative, Alternative, (<*>), (<|>), (<$>), pure, empty, liftA)
import Control.Monad hiding (fail)
import Data.Char
import Data.Maybe

-- Classes

pReturn :: Applicative f => a -> f a
pReturn = pure

pFail :: Alternative f => f a 
pFail = empty

class Symbol p symbol token | symbol -> token where
    pSym :: symbol -> p token

type Cost     = Int
type Progress = Int

class Provides state symbol token | state symbol -> token where
    splitState  :: symbol -> (token -> state -> Steps a) -> state -> Steps a

class Eof state where
    eof         :: state -> Bool
    deleteAtEnd :: state -> Maybe (Cost, state)

class Parser p where
    parse :: Eof state => p state a -> state -> a

-- Steps

data Steps a where
    Step  :: Progress -> Steps a -> Steps a
    Fail  :: [String] -> [[String] -> (Int, Steps a)] -> Steps a
    Apply :: forall b. (b -> a) -> Steps b -> Steps a
    EndH :: [a] -> ([a] -> Steps r) -> Steps (a, r) -> Steps (a, r)
    EndF :: [Steps a] -> Steps a -> Steps a

fail :: Steps a 
fail = Fail [] [const ((0, fail))]

noAlts :: Steps a
noAlts = Fail [] []

eval :: Steps a -> a
eval (Step _ l)   = eval l
eval (Fail ss ls) = eval (getCheapest 3 [f ss | f <- ls]) 
eval (Apply f l)  = f (eval l)
eval (EndF _ _)   = error "eval: dangling EndF constructor"
eval (EndH _ _ _) = error "eval: dangling EndH constructor"

push :: v -> Steps r -> Steps (v, r)
push v = Apply ((,) v)

apply :: Steps (b -> a, (b, r)) -> Steps (a, r)
apply = Apply (\(b2a, ~(b, r)) -> (b2a b, r))  

normalize :: Steps a -> Steps a
normalize (Apply f x) = case x of
    Step p l   -> Step p (Apply f l)
    Fail ss ls -> fail' ss (Apply f) ls
    Apply g l  -> normalize (Apply (f . g) l)
    EndF ss l  -> EndF (map (Apply f) ss) (Apply f l)
    EndH _ _ _ -> error "normalize: Apply before EndH"
normalize steps = steps

fail' :: [String] -> (Steps b -> Steps a) -> [[String] -> (Int, Steps b)] -> Steps a
fail' m f ls = Fail m (map (\g ex -> let (c,l) = g ex in (c, f l)) ls)

best :: Steps a -> Steps a -> Steps a
x `best` y = normalize x `best'` normalize y

best' :: Steps a -> Steps a -> Steps a
Fail sl ll  `best'` Fail sr rr = Fail (sl ++ sr) (ll ++ rr)
Fail _  _   `best'` r          = r
l           `best'` Fail _  _  = l

Step n l    `best'` Step m r = case compare n m of
    EQ -> Step n (l `best'` r)     
    LT -> Step n (l `best'` Step (m - n) r)
    GT -> Step m (Step (n - m) l `best'` r)

EndF as l   `best'` EndF bs r   = EndF (as++bs) (l `best` r)
EndF as l   `best'` r           = EndF as (l `best` r)
l           `best'` EndF bs r   = EndF bs (l `best` r)

EndH as k l `best'` EndH bs _ r = EndH (as++bs) k (l `best` r)
EndH as k l `best'` r           = EndH as k (l `best` r)
l           `best'` EndH bs k r = EndH bs k (l `best` r)

l `best'` r = l `best` r 

-- History

-- do not change into data !!
newtype History state a = History 
    { unHistory :: (forall r. (a -> state -> Steps r) -> state -> Steps r) } 

instance Functor (History state) where
    fmap f p = History (\k -> unHistory p (k . f))

instance Applicative (History  state) where
    pure a  = History (\k -> k a)
    p <*> q = History (\k -> unHistory p (\f -> unHistory q (k . f))) 

instance Alternative (History state) where
    empty   = History (\_ _   -> noAlts) 
    p <|> q = History (\k inp -> unHistory p k inp `best` unHistory q k inp) 

instance Monad (History state) where
    p >>= f = History (\k -> unHistory p (\a -> unHistory (f a) k))
    return  = pure

instance MonadPlus (History state) where
    mzero = empty
    mplus = (<|>) 

instance (Provides state symbol token) => Symbol (History state) symbol token where
    pSym a = History (splitState a)

instance Parser History where
    parse p =  
        fst . eval . unHistory p (\a rest -> if eof rest then push a fail else error "History.parse: pEnd missing?") 

-- Future

-- do not change into data !!
newtype Future state a = Future 
    { unFuture :: forall r . (state -> Steps r) -> state -> Steps (a, r) } 

instance Functor (Future state) where
    fmap = liftA
 
instance Applicative (Future state) where
    pure a  = Future (push a .)
    p <*> q = Future ((apply .) . unFuture p . unFuture q)

instance Alternative (Future state) where
    empty   = Future (\_ _   -> noAlts)
    p <|> q = Future (\k inp -> unFuture p k inp `best` unFuture q k inp)  

instance (Provides state symbol token) => Symbol (Future state) symbol token where
    pSym a = Future (\k -> splitState a (\t -> push t . k))

instance Parser Future where
    parse p = 
        fst . eval . unFuture p (\rest -> if eof rest then fail else error "Future.parse: pEnd missing")

-- Monads

infixr 1 >>>=
class GenMonad m n where
    (>>>=) :: m b -> (b -> n a) -> n a

instance Monad (History s) => GenMonad (History s) (History s) where
    (>>>=) = (>>=) --  the monadic bind defined before

instance GenMonad (History s) (Future s) where
    h >>>= f = Future (\k -> unHistory h (\p -> unFuture (f p) k))

data P s a = P (History s a) (Future s a)

getHistory :: P state a -> (a -> state -> Steps r) -> state -> Steps r
getHistory (P (History h) _) = h

getFuture :: P state a -> (state -> Steps r) -> state -> Steps (a, r) 
getFuture (P _ (Future f)) = f

instance Functor (P state) where
    fmap f (P x y) = P (fmap f x) (fmap f y)

instance Applicative (P state) where
    pure a = P (pure a) (pure a) 
    P hp fp <*> ~(P hq fq) = P (hp <*> hq) (fp <*> fq) 

instance Alternative (P state) where
    P hp fp <|> P hq fq = P (hp <|> hq) (fp <|> fq)
    empty = P empty empty
 
instance (Provides state symbol token) => Symbol (P state) symbol token where
    pSym a = P (pSym a) (pSym a)

instance Parser P where
    parse p = 
        fst . eval . getFuture p (\rest -> if eof rest then fail else error "P.parse: EndF missing?") 

instance Monad (P state) where
    p >>= f = P 
        (History (getHistory p . flip (getHistory . f)))
        (Future  (getHistory p . flip (getFuture . f)))
    return = pure

instance MonadPlus (P state) where
    mzero = empty
    mplus = (<|>)

-- Greedy

bestGreedy :: Steps a -> Steps a -> Steps a
bestGreedy l@(Step _ _) = const l 
bestGreedy l            = best l 

class  Greedy p where 
  (<<|>) :: p a -> p a -> p a

instance Greedy (History state)  where
    p <<|> q = History (\k st -> normalize (unHistory p k st) `bestGreedy` normalize (unHistory q k st))

instance Greedy (Future state)  where
    p <<|> q = Future (\k st -> normalize (unFuture p k st) `bestGreedy` normalize (unFuture q k st))

instance Greedy (P state) where
    P hp fp <<|> P hq fq = P (hp <<|> hq) (fp <<|> fq) 

-- Ambiguous

class Ambiguous p where
    amb :: p a -> p [a]

instance Ambiguous (History state) where
    amb p = History (\k -> removeEndH . unHistory p (\a st' -> EndH [a] (\as -> k as st') noAlts)) where

removeEndH :: Steps (a, r) -> Steps r
removeEndH (Fail m ls)   = fail' m removeEndH ls
removeEndH (Step ps l)   = Step ps (removeEndH l)
removeEndH (EndH as k r) = k as `best` removeEndH r 
removeEndH (Apply _ _)   = error "removeEndH: unexpected Apply"
removeEndH (EndF _ _)    = error "removeEndH: unexpected EndF"

instance Ambiguous (Future state) where
    amb p = Future (\k -> combineValues . removeEndF . unFuture p (\st -> EndF [k st] noAlts)) where

        combineValues :: Steps [(a,r)] -> Steps ([a],r)
        combineValues = Apply (map fst &&& snd . head)

        infixr 3 &&&
        (&&&) :: (a -> b) -> (a -> c) -> a -> (b,c)
        (f &&& g) a = (f a, g a)

        removeEndF :: Steps r -> Steps [r]
        removeEndF (Fail m ls)     = fail' m removeEndF ls
        removeEndF (Step ps l)     = Step ps (removeEndF l)
        removeEndF (Apply f l)     = Apply (map' f) (removeEndF l) where
            map' f ~(x:xs) = f x : map f xs
        removeEndF (EndF (s:ss) r) = Apply (: map eval ss) s `best` removeEndF r
        removeEndF (EndH _ _ _)    = error "removeEndF: unexpected EndH"

instance Ambiguous (P state) where
    amb (P hp fp) = P (amb hp) (amb fp)
       
-- getCheapest

getCheapest :: Int -> [(Int, Steps a)] -> Steps a 
getCheapest _ [] = error "getCheapest: no correcting alternative found"
getCheapest i ls = snd $ foldr step (maxBound, error "getCheapest: logic error") ls where        
    step (w,ll) btf@(c,l) 
        | w < c, new <- getCheapest' i ll w c, new < c = (new, ll)
        | otherwise = btf 

    -- arguments:
    -- 1.) # of tree levels to inspect, 
    -- 2.) the tree
    -- 3.) accumulated cost from root
    -- 4.) best value found for a tree thus far as bound
    getCheapest' :: Int -> Steps a -> Int -> Int -> Int
    getCheapest' 0 _ = const
    getCheapest' n (Step ps l) = getCheapest' (n-1) l
    getCheapest' n (Apply _ l) = getCheapest' n l
    getCheapest' n (Fail m m2ls) = \v c -> 
        let f (w,l) c' 
                | v + w < c' = getCheapest' (n-1) l (v+w) c'
                | otherwise  = c'
        in foldr (\m2l -> f (m2l m)) c m2ls where
    getCheapest' n (EndH a lf r) = getCheapest' n (lf a `best` removeEndH r)
    getCheapest' n (EndF (l:_) r) = getCheapest' n (l `best` r)

-- pErrors

class state `Stores` pErrors where
    getErrors :: state -> (pErrors, state)

class p `AsksFor` errors where
    pErrors :: p errors
    pEnd    :: p errors

instance (Eof state, state `Stores` errors) => AsksFor (History state) errors where
    pErrors = History (\k -> uncurry k . getErrors)
    pEnd    = History pEnd' where
        pEnd' k inp = case deleteAtEnd inp of
            Nothing        -> uncurry k (getErrors inp)
            Just (i, inp') -> Fail [] [const (i, pEnd' k inp')]

pushErrors :: (state `Stores` errors) => (state -> Steps r) -> state -> Steps (errors, r)
pushErrors k inp = let (errors, inp') = getErrors inp in push errors (k inp')

instance (Eof state, state `Stores` errors) => AsksFor (Future state) errors where
    pErrors = Future pushErrors
    pEnd    = Future pEnd' where
        pEnd' k inp = case deleteAtEnd inp of
            Nothing        -> pushErrors k inp
            Just (i, inp') -> Fail [] [const (i, pEnd' k inp')]

instance (Eof state, state `Stores` errors) => AsksFor (P state) errors where
    pErrors = P pErrors pErrors
    pEnd    = P pEnd pEnd

-- State Change

class Switch p where
    switch :: (st1 -> (st2, st2 -> st1)) -> p st2 a -> p st1 a

instance Switch History where
    split `switch` History p = History (\k st1 -> 
        let (st2, back) = split st1
        in p (\a -> k a . back) st2)

instance Switch Future where
    split `switch` Future p = Future (\k st1 -> 
        let (st2, back) = split st1
        in p (k . back) st2)

instance Switch P where
    split `switch` P p q = P (split `switch` p) (split `switch` q)

-- Recognisers

newtype R state a = R 
    { unR :: forall r. (state -> Steps r) -> state -> Steps r } 

instance Functor (R state) where
    fmap _ (R r) = R r 

instance Applicative (R state) where
    pure _      = R id
    R p <*> R q = R (p . q)

instance Alternative (R state) where
    empty       = R (\_ _   -> noAlts)
    R p <|> R q = R (\k inp -> p k inp `best` q k inp)  

instance (Provides state symbol token) => Symbol (R state) symbol token where
    pSym a = R (splitState a . const) 

class  Alternative p => ExtApplicative p st | p -> st where
    (<*) :: p    a -> R st b -> p a
    (*>) :: R st b -> p    a -> p a
    (<$) :: a      -> R st b -> p a

instance ExtApplicative (History st) st where
    History p <* R r       = History (\k -> p (r . k)) 
    R r       *> History p = History (r . p)
    f         <$ R r       = History (\k -> r (k f))

instance ExtApplicative (Future st) st where
    Future p <* R r      = Future (p . r)
    R r      *> Future p = Future (r . p)
    f        <$ R r      = Future (\k -> push f . r k)

instance  ExtApplicative (P st) st where
    P h f <* r     = P (h <* r) (f <* r) 
    r     *> P h f = P (r *> h) (r *> f)
    f     <$ r     = P (f <$ r) (f <$ r)       
