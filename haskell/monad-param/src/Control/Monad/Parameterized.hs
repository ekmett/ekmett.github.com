{-# OPTIONS_GHC -fno-implicit-prelude -fallow-overlapping-instances -fallow-undecidable-instances -fglasgow-exts #-} 
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Parameterized
-- Copyright   :  (C) 2006 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (requires the kitchen sink)
--
-- Implements a notion of parameterized monad by varying the monad itself, this lets us 
-- avoid having to carry a parameter around for monads that do not need it, and we can rederive
-- the normal notion of a parameterized monad from this variation for those that do.
-- The signature of `>>=` costs us type inference for the types of `return` and `mzero`,
-- so we restore that by defining `return` as the unit of the `Identity` monad and `mzero` as 
-- the unit of the trivial bottom monad, and appealing to the monad laws to allow these to combine
-- with all other monads satisfying the monad laws through `>>=`
--
-- Caveat: this currently does not permit types to vary under the @do@-sugar because of assumptions in GHC
-- about the shape of `>>=`.
-- 
-- This imports and defines the correct instances for a good portion of the @MTL@, primarily because
-- it is so awkward to import them all otherwise due to the fact that most of them re-export the 'Control.Monad.Monad' syntax.
-- Does not export "Control.Monad.ST" or "Control.Monad.Writer" since it is unclear if you want strict or lazy versions in scope
----------------------------------------------------------------------------

module Control.Monad.Parameterized (
	-- * Rebound `Monad` 
	  Return	(returnM) 
	, Fail		(fail)
	, Bind		((>>=),(>>))
	, (=<<)

	-- * Rebound `MonadPlus` 
	, MPlus		(mplus)
	, MonadZero	(mzeroM)

	-- * A bottom monad
	, MZero

	-- * Convenient class aliases
	, Monad
	, MonadPlus

	-- * Traditional interfaces
	, Go		(go)
	, return
	, mzero	

	-- * Export common monads in this sugar
	, module Control.Concurrent.STM
	, module Control.Monad.Cont
	, module Control.Monad.Cont.Class
	, module Control.Monad.Error
	, module Control.Monad.Error.Class
	, module Control.Monad.Fix
	, module Control.Monad.Identity
	, module Control.Monad.List
	, module Control.Monad.Reader
	, module Control.Monad.State
	, module Control.Monad.Writer.Class
	-- , module Data.STRef
	-- , module Data.IORef

	, mapM          -- :: (Monad m) => (a -> m b) -> [a] -> m [b]
	, mapM_         -- :: (Monad m) => (a -> m b) -> [a] -> m ()
	, forM          -- :: (Monad m) => [a] -> (a -> m b) -> m [b]
	, forM_         -- :: (Monad m) => [a] -> (a -> m b) -> m ()
	, sequence      -- :: (Monad m) => [m a] -> m [a]
	, sequence_     -- :: (Monad m) => [m a] -> m ()
	, join          -- :: (Monad m) => m (m a) -> m a
	, msum          -- :: (MonadPlus m) => [m a] -> m a
	, filterM       -- :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
	, mapAndUnzipM  -- :: (Monad m) => (a -> m (b,c)) -> [a] -> m ([b], [c])
	, zipWithM      -- :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
	, zipWithM_     -- :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m ()
	, foldM         -- :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a 
	, foldM_        -- :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m ()
	, replicateM    -- :: (Monad m) => Int -> m a -> m [a]
	, replicateM_   -- :: (Monad m) => Int -> m a -> m ()
	, guard         -- :: (MonadPlus m) => Bool -> m ()
	, when          -- :: (Monad m) => Bool -> m () -> m ()
	, unless        -- :: (Monad m) => Bool -> m () -> m ()
	, liftM         -- :: (Monad m) => (a -> b) -> (m a -> m b)
	, liftM2        -- :: (Monad m) => (a -> b -> c) -> (m a -> m b -> m c)
	, liftM3        -- :: ...
	, liftM4        -- :: ...
	, liftM5        -- :: ...
	, ap            -- :: (Monad m) => m (a -> b) -> m a -> m b
	) where

import Data.Monoid
import Prelude hiding (Monad,(>>=),(>>),return,fail,(=<<))
import Data.Maybe (catMaybes)
-- import Data.STRef
-- import Data.IORef

import Control.Concurrent.STM
import Control.Monad
	(mapM, mapM_, sequence, sequence_, forM, forM_, join, msum, filterM, mapAndUnzipM, zipWithM, zipWithM_,
	foldM, foldM_, replicateM, replicateM_, guard, when, unless, liftM, liftM2, liftM3, liftM4, liftM5, ap)
import Control.Monad.Cont
	(Cont(..),mapCont, withCont, ContT(..),mapContT,withContT)
import Control.Monad.Cont.Class 
	(MonadCont, callCC)
import Control.Monad.Error 
	(ErrorT, runErrorT, mapErrorT)
import Control.Monad.Error.Class 
	(Error, noMsg, strMsg, MonadError, throwError, catchError)
import Control.Monad.Fix 
	(MonadFix, mfix, fix)
import Control.Monad.Identity (Identity,runIdentity)
import Control.Monad.State 
	(State(..),StateT(..),MonadState
	,evalState,execState,mapState,withState
	,evalStateT,execStateT,mapStateT,withStateT)
import Control.Monad.ST.Strict as StrictST
	(ST, runST, fixST, RealWorld, stToIO) -- unsafe* should be imported directly
import Control.Monad.ST.Lazy as LazyST
	(ST, runST, fixST, RealWorld, stToIO, strictToLazyST,lazyToStrictST) -- unsafe* should be imported directly
import Control.Monad.Reader 
	(Reader(..),ReaderT(..)
	,mapReader,withReader,mapReaderT,withReaderT)
import qualified Control.Monad.Writer.Lazy as LazyW
	(Writer,runWriter,execWriter,mapWriter
	,WriterT,runWriterT,execWriterT,mapWriterT)
import qualified Control.Monad.Writer.Strict as StrictW
	(Writer,runWriter,execWriter,mapWriter
	,WriterT,runWriterT,execWriterT,mapWriterT)
import Control.Monad.Writer.Class
import Control.Monad.List (ListT(..),runListT)
import qualified Control.Monad as Old

infixl 1  >>, >>=
infixr 1  =<<

-- instance Show a => Show (Identity a) where show a = "Identity " ++ show (runIdentity a)
-- instance Show (MZero a) where show a = "error \"MZero\""

-- | An inferable version of `Prelude.return`
return :: a -> Identity a
return = Old.return

-- | Restrict the cases where we allow pattern matching to `fail`. You have to explicitly supply this for your `Monad`
class Fail m where
	fail :: String -> m a

-- | Implement parameterized monads like Oleg's restricted monads, but vary the monad itself rather than restrict its parameters
class (Functor m, Functor m', Functor m'') => Bind m m' m'' | m m' -> m'' where
	(>>=) :: m a -> (a -> m' b) -> (m'' b) 
	(>>)  :: m a -> m' b -> m'' b
	m >> k = m >>= const k

instance Functor a => Bind Identity a a 	where m >>= f = f (runIdentity m)
instance Functor a => Bind a Identity a 	where m >>= f = fmap (runIdentity . f) m
instance Bind Identity Identity Identity 	where m >>= f = f (runIdentity m)

(=<<) :: Bind m m' m'' => (a -> m' b) -> m a -> m'' b
k =<< m = m >>= k

-- | When a parameterized monad can be used without varying its parameter, we can get the ease of use of the original @Monad@ class.
class (Fail m, Return m, Bind m m m) => Monad m
instance (Fail m, Return m, Bind m m m) => Monad m

-- | Same trick using with `Identity` to build a canonical `return`, here we exploit the `MonadPlus` laws to make a canonical `mzero`. Has no members except bottom.
data MZero a
-- | An inferable version of `Control.Monad.mzero`

mzero :: MZero a
mzero = undefined 

-- | its trivial to map a function over nothing
instance Functor MZero where fmap f = undefined

-- | Break out mplus
class MPlus m m' m'' | m m' -> m'' where
	mplus :: m a -> m' a -> m'' a

-- | We we losing type inference for MonadZero anyways, plumb around the special cases
instance MPlus MZero a a where mplus _ a = a
instance MPlus a MZero a where mplus a _ = a
instance MPlus MZero MZero MZero where mplus _ _  = undefined
instance Functor a => Bind MZero a MZero where _ >>= _ = undefined
instance Functor a => Bind a MZero MZero where _ >>= _ = undefined
instance Bind MZero MZero MZero where _ >>= _ = undefined

-- resolve conflict
instance Bind Identity MZero MZero where _ >>= _ = undefined
instance Bind MZero Identity MZero where _ >>= _ = undefined

-- | The traditional `return`, note this probably has lost its type inference where you want to use it. 
class Return m where 
	returnM :: a -> m a 

-- | Traditional `Control.Monad.mzero`, note this probably has lost its type inference. 
-- You probably want `mzero`.
class MonadZero m where 
	mzeroM :: m a

-- | Now of course we can have `MZero`s and `Identity`s float to the top of a @do@ expression, so we need a way to convert them to any `Monad` or `MonadPlus` instance respectively

class Go n m where 
	-- | Usage: @go (do something)@
	go :: n a -> m a 
-- ensure confluence of both reductions
instance Return a => Go Identity a where 	go = returnM . runIdentity 
instance MonadZero a =>  Go MZero a where 	go _ = mzeroM
instance Go a a where 		go = id
instance Return Identity where returnM = return
instance Return MZero where returnM _ = undefined

-- | Class alias to get back an approximation of the original, easy-to-specify `MonadPlus` class where available
class (MPlus m m m, MonadZero m) => MonadPlus m
instance (MPlus m m m, MonadZero m) => MonadPlus m

instance Return Maybe where returnM = Old.return
instance Fail Maybe where fail = Old.fail
instance Bind Maybe Maybe Maybe where (>>=) = (Old.>>=)
instance MonadZero Maybe where mzeroM = Old.mzero
instance MPlus Maybe Maybe Maybe where mplus = Old.mplus

instance Return [] where returnM = Old.return
instance Fail [] where fail = Old.fail
instance Bind [] [] [] where (>>=) = (Old.>>=)
instance MonadZero [] where mzeroM = Old.mzero
instance MPlus [] [] [] where mplus = Old.mplus

instance Return STM where returnM = Old.return
instance Fail STM where fail = Old.fail
instance Bind STM STM STM where (>>=) = (Old.>>=)

instance Return IO where returnM = Old.return
instance Fail IO where fail = Old.fail
instance Bind IO IO IO where (>>=) = (Old.>>=)

instance Return (State s) where returnM = Old.return
instance Fail (State s) where fail = Old.fail
instance Bind (State s) (State s) (State s) where (>>=) = (Old.>>=)

instance Return (Reader e) where returnM = Old.return
instance Fail (Reader e) where fail = Old.fail
instance Bind (Reader e) (Reader e) (Reader e) where (>>=) = (Old.>>=)

instance Return (Cont r) where returnM = Old.return
instance Fail (Cont r) where fail = Old.fail
instance Bind (Cont r) (Cont r) (Cont r) where (>>=) = (Old.>>=)

instance Return (StrictST.ST s) where returnM = Old.return
instance Fail (StrictST.ST s) where fail = Old.fail
instance Bind (StrictST.ST s) (StrictST.ST s) (StrictST.ST s) where (>>=) = (Old.>>=)

instance Return (LazyST.ST s) where returnM = Old.return
instance Fail (LazyST.ST s) where fail = Old.fail
instance Bind (LazyST.ST s) (LazyST.ST s) (LazyST.ST s) where (>>=) = (Old.>>=)

instance Monoid w => Return (LazyW.Writer w) where returnM = Old.return
instance Monoid w => Fail (LazyW.Writer w) where fail = Old.fail
instance Monoid w => Bind (LazyW.Writer w) (LazyW.Writer w) (LazyW.Writer w) where (>>=) = (Old.>>=)

instance Monoid w => Return (StrictW.Writer w) where returnM = Old.return
instance Monoid w => Fail (StrictW.Writer w) where fail = Old.fail
instance Monoid w => Bind (StrictW.Writer w) (StrictW.Writer w) (StrictW.Writer w) where (>>=) = (Old.>>=)

instance Old.Monad m => Return (ListT m) where returnM = Old.return
instance Old.Monad m => Fail (ListT m) where fail = Old.fail
instance Old.Monad m => Bind (ListT m) (ListT m) (ListT m) where (>>=) = (Old.>>=)
instance Old.Monad m => MonadZero (ListT m) where mzeroM = Old.mzero
instance Old.Monad m => MPlus (ListT m) (ListT m) (ListT m) where mplus = Old.mplus

instance Old.Monad m => Return (StateT s m) where returnM = Old.return
instance Old.Monad m => Fail (StateT s m) where fail = Old.fail
instance Old.Monad m => Bind (StateT s m) (StateT s m) (StateT s m) where (>>=) = (Old.>>=)

instance Old.Monad m => Return (ReaderT e m) where returnM = Old.return
instance Old.Monad m => Fail (ReaderT e m) where fail = Old.fail
instance Old.Monad m => Bind (ReaderT e m) (ReaderT e m) (ReaderT e m) where (>>=) = (Old.>>=)

instance (Old.Monad m, Monoid w) => Return (LazyW.WriterT w m) where returnM = Old.return
instance (Old.Monad m, Monoid w) => Fail (LazyW.WriterT w m) where fail = Old.fail
instance (Old.Monad m, Monoid w) => Bind (LazyW.WriterT w m) (LazyW.WriterT w m) (LazyW.WriterT w m) where (>>=) = (Old.>>=)

instance (Old.Monad m, Monoid w) => Return (StrictW.WriterT w m) where returnM = Old.return
instance (Old.Monad m, Monoid w) => Fail (StrictW.WriterT w m) where fail = Old.fail
instance (Old.Monad m, Monoid w) => Bind (StrictW.WriterT w m) (StrictW.WriterT w m) (StrictW.WriterT w m) where (>>=) = (Old.>>=)

instance (Old.Monad m, Error e) => Return (ErrorT e m) where returnM = Old.return
instance (Old.Monad m, Error e) => Fail (ErrorT e m) where fail = Old.fail
instance (Old.Monad m, Error e) => Bind (ErrorT e m) (ErrorT e m) (ErrorT e m) where (>>=) = (Old.>>=)
instance (Old.Monad m, Error e) => MonadZero (ErrorT e m) where mzeroM = Old.mzero
instance (Old.Monad m, Error e) => MPlus (ErrorT e m) (ErrorT e m) (ErrorT e m) where mplus = Old.mplus

instance Old.Monad m => Return (ContT r m) where returnM = Old.return
instance Old.Monad m => Fail (ContT r m) where fail = Old.fail
instance Old.Monad m => Bind (ContT r m) (ContT r m) (ContT r m) where (>>=) = (Old.>>=)

-- lift some desirable mixins
instance Bind Maybe [] [] where
	Just a >>= f = f a
	Nothing >>= _ = []

instance Bind [] Maybe [] where
	xs >>= f = catMaybes $ map f xs

instance Bind STM IO IO where m >>= k = atomically m >>= k
instance Bind IO STM IO where m >>= k = m >>= (atomically . k)

instance Bind [] IO (ListT IO) where
	xs >>= f = ListT (mapM f xs)

-- | Demonstrate mixed STM and IO
testSTMIO = newTVar 2 >>= readTVar >>= print

-- | Demonstrate mixed Maybe and List
testMaybeList = Just 2 >>= \x -> [x*1,x*2]

-- | Demonstrate mixing Identity, Maybe and List
testIdentityMaybe = 
	return 2 >>= (Just . (*4)) >>= \y -> 
	[y,y+1] >>= \z -> 
	if z `mod` 2 == 0 then Just z else Nothing
