{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}
module Free where

import Data.Map (Map,(!))
import qualified Data.Map as Map
import Control.Arrow ((***))
import Prelude hiding (pi,abs)
import Control.Monad.Error
import Control.Exception

class ExpFunctor f where
    xmap :: (a -> b) -> (b -> a) -> f a -> f b

-- * The free monad
data Free f a = Roll (f (Free f a)) | Return a 
-- todo: showsprec
instance (Show a, Show (f (Free f a))) => Show (Free f a) where
	show (Roll x) = "Roll (" ++ show x ++ ")"
	show (Return x) = "Return (" ++ show x ++ ")"

instance (Eq a, Eq (f (Free f a))) => Eq (Free f a) where
	Roll x    == Roll x' 	= x == x'
	Return x  == Return x' 	= x == x'
	_         == _ 		= False

instance Functor f => Functor (Free f) where
	fmap f (Roll x) = Roll $ fmap (fmap f) x
	fmap f (Return x) = Return (f x)

instance Functor f => Monad (Free f) where
	return = Return
	Return m >>= k = k m 
	Roll m >>= k = Roll $ fmap (>>= k) m

instance ExpFunctor f => ExpFunctor (Free f) where
	xmap f g (Roll x) = Roll $ xmap (xmap f g) (xmap g f) x
	xmap f g (Return x) = Return (f x)


-- * The cofree comonad
data Cofree f a = Cofree a (f (Cofree f a))

instance (Show a, Show (f (Cofree f a))) => Show (Cofree f a) where
	show (Cofree a fa) = "Cofree (" ++ show a ++ ") (" ++ show fa ++ ")"

instance (Eq a, Eq (f (Cofree f a))) => Eq (Cofree f a) where
	Cofree a b == Cofree a' b' = a == a' && b == b'

instance Functor f => Functor (Cofree f) where
	fmap f (Cofree x y) = Cofree (f x) (fmap (fmap f) y)

instance ExpFunctor f => ExpFunctor (Cofree f) where
	xmap f g (Cofree x y) = Cofree (f x) (xmap (xmap f g) (xmap g f) y)

-- * Universal quantification

newtype All f = All { unall :: forall a. f a }
liftAll :: (forall a. Free f a -> Free f a) -> All (Free f) -> All (Free f)
liftAll f (All x) = All (f x)
cata :: ExpFunctor f => (f a -> a) -> All (Free f) -> a
cata phi = cata' . unall where
    cata' (Roll x) = phi $ xmap cata' Return x
    cata' (Return x) = x 

-- * Existential quantification
data Any f = forall a. Any { unany :: f a }
ana :: ExpFunctor f => (a -> f a) -> a -> Any (Cofree f)
ana psi = Any . ana' where
    ana' a = Cofree a $ xmap ana' extract (psi a)
    extract :: Cofree f a -> a
    extract (Cofree a _) = a

-- * Term functor
data T a 
    = App a a 
    | Abs Abs a (a -> a) 
    | Sort Sort
data Sort = Box | Star deriving (Eq)
data Abs = Pi | Lam deriving (Eq)
    
type Term = All (Free T)

instance ExpFunctor T where
    xmap f g (App a b) = App (f a) (f b)
    xmap f g (Abs a t h) = Abs a (f t) (f. h . g)
    xmap f g (Sort s) = Sort s

app :: Free T a -> Free T a -> Free T a
app a b = Roll (App a b)

lam :: Free T a -> (Free T a -> Free T a) -> Free T a
lam t f = Roll (Abs Lam t f)

pi :: Free T a -> (Free T a -> Free T a) -> Free T a
pi t f = Roll (Abs Pi t f)

box :: Free T a
box = Roll (Sort Box)

star :: Free T a
star = Roll (Sort Star)

instance Show Term where
    show x = cata phi x vars where
        phi :: T ([String] -> String) -> [String] -> String
        phi (Sort s) vars = show s
        phi (App a b) vars = "(" ++ a vars ++ " " ++ b vars ++ ")" 
        phi (Abs a t f) (v:vars) = "(" ++ show a ++ " " ++ v ++ " : " ++ t vars ++ ". " ++ f (const v) vars ++ ")"
        vars :: [String]
        vars = [ [i] | i <- ['a'..'z']] ++ [i : show j | j <- [1..], i <- ['a'..'z'] ]

instance Show Abs where 
    show Lam = "lam"
    show Pi = "pi"

instance Show Sort where
    show Star = "*"
    show Box = "#"
            

whnf :: Free T a -> Free T a
whnf x = spine x [] where
    spine :: Free T a -> [Free T a] -> Free T a
    spine (Roll (App f a)) as         = spine f (a:as)
    spine (Roll (Abs Lam t e)) (a:as) = spine (e a) as
    spine a as                        = foldl app a as
    
nf :: Free T a -> Free T a
nf x = spine x [] where
    spine :: Free T a -> [Free T a] -> Free T a
    spine (Roll (App f a)) as         = spine f (a:as)
    spine (Roll (Abs Lam t e)) (a:as) = spine (e (nf a)) as
    spine (Roll (Abs Lam t e)) []     = lam (nf t) (nf . e)
    spine (Roll (Abs Pi t e)) as      = foldl app (pi (nf t) (nf . e)) as
    spine a as                        = foldl app a as

a --> b = pi a (const b)
a # b = app a b

xpairt = lam star $ \a -> lam star $ \b -> lam star $ \c -> (a --> b --> c) --> c
xpair  = lam star $ \a -> lam star $ \b -> lam a $ \x -> lam b $ \y -> lam star $ \c -> lam (a --> b --> c) $ \f -> f # x # y
xsplit = lam star $ \a -> lam star $ \b -> lam star $ \r -> lam (a --> b --> r) $ \f -> lam (xpairt # a # b) $ \p -> p # r # f
xfst   = lam star $ \a -> lam star $ \b -> lam (xpairt # a # b) $ \p -> xsplit # a # b # a # (lam a $ \x -> lam b $ \y -> x) # p
xsnd   = lam star $ \a -> lam star $ \b -> lam (xpairt # a # b) $ \p -> xsplit # a # b # b # (lam a $ \x -> lam b $ \y -> y) # p
xid    = lam star $ \a -> lam a id


fresh :: Free T Int -> Int
fresh term = succ $ fresh' term 0 where
    fresh' :: Free T Int -> Int -> Int
    fresh' (Return x) = max x 
    fresh' (Roll (App a b)) = fresh' a . fresh' b
    fresh' (Roll (Abs a t f)) = fresh' t . fresh . f . Return
    fresh' (Roll (Sort s)) = id

type TC a = Either String a

bind :: Free T Int -> Map Int (Free T a) -> TC (Free T a)
bind term env = bind' term env (fresh term) where
    bind' :: Free T Int -> Map Int (Free T a) -> Int -> TC (Free T a)
    bind' (Roll (Sort s)) _ i = return (Roll (Sort s))
    bind' (Return x) env i = maybe (throwError $ "Unbound variable " ++ show x) return (Map.lookup x env)
    bind' (Roll (App a b)) env i = liftM2 app (bind' a env i) (bind' b env i)
    bind' (Roll (Abs a t f)) env i = do
	let 
	    f' :: Free T a -> Free T a
            f' x = either error id $ bind' (f $ Return i) (Map.insert i x env) $ succ i
	t' <- bind' t env i
        return . Roll $ Abs a t' f'


-- this should be able to lift the quantifier out, but i'm a little slow
rebind :: Free T Int -> TC (Term)
rebind term = return $ All (either error id (bind term Map.empty))

aeq :: Term -> Term -> Bool
aeq (All e1) (All e2) = aeq' e1 e2 0

aeq' :: Free T Int -> Free T Int -> Int -> Bool
aeq' (Roll (App f a))   (Roll (App f' a'))    i = aeq' f f' i && aeq' a a' i
aeq' (Roll (Abs a t e)) (Roll (Abs a' t' e')) i = a == a' && aeq' t t' i && aeq' (e (Return i)) (e' (Return i)) (succ i)
aeq' (Roll (Sort s))    (Roll (Sort s'))      _ = s == s'
aeq' (Return v)            (Return v')              _ = v == v'
aeq' _                  _                     _ = False

beq :: Term -> Term -> Bool
beq (All e1) (All e2) = beq' e1 e2 0

beq' :: Free T Int -> Free T Int -> Int -> Bool
beq' e1 e2 a = aeq' (nf e1) (nf e2) a

typecheck :: Term -> TC Term
typecheck (All x) = typecheck' x where
	typecheck' :: Free T Int -> TC Term
	typecheck' (Return t) 	      = throwError "Unbound variable"
	typecheck' (Roll (Sort Star)) = return (All box)
    	typecheck' (Roll (Sort Box))  = throwError "Found #"

{-


tc :: Term -> TC Term
tc qx@(A x) = tc' x x qx where
    tc' :: Free T Int -> TC (Free T Int)
    tc' (Return t)          	= return t'
    tc' (Roll (Sort Star))      = return $ A box
    tc' (Roll (App f a)) 	= do
        tf <- tc' f
        case (unall $ whnf tf) of
            Roll (Abs Pi at k) -> do
                ta <- tc' a
                when (not (beq'' (unall ta) at)) $ do
                    throwError "Bad function argument type:"
                return (k a)
            _ -> throwError $ "Non-function in application: " ++ show tf
    tc' (Roll (Abs Lam t e)) = do
        tc t
        te <- tc (e t)
        let result = A (pi t $ typeof . e)
        tc result
        return result
    tc' (Roll (Abs Pi a k)) = do
        s <- tc a
        let s' = liftA whnf s
        t <- tc (k s')
        let t' = liftA whnf t
	maybe (\x -> return (A (Roll (Sort x)))) 
	      (throwError "Bad abstraction") 
              $ do -- Maybe
		Roll (Sort s'') <- return (unall s')
		Roll (Sort t'') <- return (unall s')
       		rule s'' t''
typeof :: Term -> Term
typeof = either error id . tc -- todo: strip checks?
-}

rule :: Sort -> Sort -> Maybe Sort
rule Star Star = return Star
rule Box Star = return Star
rule Star Box = return Box
rule Box Box = return Box
