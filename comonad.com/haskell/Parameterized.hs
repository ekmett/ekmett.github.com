{-# OPTIONS_GHC -fno-implicit-prelude -fallow-overlapping-instances -fallow-undecidable-instances -fglasgow-exts -fextended-default-rules #-}
module Control.Monad.Parameterized
	-- * Rebound monad sugar
	( returnM	-- inferable return
	, mzeroM	-- inferable mzero
	, Return	(return) 
	, Go		(go)
	, Identity      (runIdentity)
	, Fail		(fail)
	, Bind		((>>=),(>>))
	, MPlus		(mplus)
	, MonadZero	(mzero)

	-- * Something like the legacy interfaces
	, Monad
	, MonadPlus

	) where
-- | Trade in a lot of nice type inference for a more general monad

import Prelude hiding (
	Monad,(>>=),(>>),return,fail,(=<<) -- ,mapM,mapM_,forM,forM_,sequence,sequence_,(=<<)
	)

import Data.Maybe (catMaybes)

import Control.Concurrent.STM
import Control.Monad.Identity (Identity,runIdentity)
import Control.Monad.State (State,runState,StateT,runStateT)
import Control.Monad.Reader (Reader,runReader,ReaderT,runReaderT)
import Control.Monad.List (ListT(..),runListT)
import qualified Control.Monad as Old

instance Show a => Show (Identity a) where
	show a = "Identity " ++ show (runIdentity a)

instance Show (MZero a) where
	show a = "(undefined :: MZero a)"

returnM :: a -> Identity a
returnM = Old.return

-- | Restrict the cases where we allow pattern matching to fail. You have to explicitly supply this to allow patterns to fail.
class Fail m where
	fail :: String -> m a

-- | A parameterized monad is like a restricted monad, but we vary the monad itself
class (Functor m, Functor m', Functor m'') => Bind m m' m'' | m m' -> m'' where
	(>>=) :: m a -> (a -> m' b) -> (m'' b) 
	(>>)  :: m a -> m' b -> m'' b
	m >> k = m >>= const k

-- | lookie, monad laws!
instance Functor a => Bind Identity a a 	where m >>= f = f (runIdentity m)
instance Functor a => Bind a Identity a 	where m >>= f = fmap (runIdentity . f) m
instance Bind Identity Identity Identity 	where m >>= f = f (runIdentity m)

(=<<) :: Bind m m' m'' => (a -> m' b) -> m a -> m'' b
k =<< m = m >>= k


-- | When a parameterized monad can be used without varying its parameter, we can get the ease of the original Monad syntax.
class (Fail m, Return m, Bind m m m) => Monad m
instance (Fail m, Return m, Bind m m m) => Monad m

-- | same trick as above, exploit the MonadZero laws
data MZero a
mzeroM :: MZero a
mzeroM = undefined 

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

class Return m where return :: a -> m a 
class MonadZero m where mzero :: m a

-- | Now of course we can have MZero's and Identity's float to the top
class Go n m where go :: n a -> m a 
-- ensure confluence of both reductions
instance Return a => Go Identity a where 	go = return . runIdentity 
instance MonadZero a =>  Go MZero a where 	go _ = mzero
instance Go a a where 		go = id
instance Return Identity where return = returnM
instance Return MZero where return _ = undefined

-- | class alias to get back an approximation of the original, easy-to-use-class where available
class (MPlus m m m, MonadZero m) => MonadPlus m
instance (MPlus m m m, MonadZero m) => MonadPlus m

instance Return Maybe where return = Old.return
instance Bind Maybe Maybe Maybe where (>>=) = (Old.>>=)
instance MonadZero Maybe where mzero = Old.mzero
instance MPlus Maybe Maybe Maybe where mplus = Old.mplus

instance Return [] where return = Old.return
instance Bind [] [] [] where (>>=) = (Old.>>=)
instance MonadZero [] where mzero = Old.mzero
instance MPlus [] [] [] where mplus = Old.mplus

instance Return STM where return = Old.return
instance Bind STM STM STM where (>>=) = (Old.>>=)

instance Return IO where return = Old.return
instance Bind IO IO IO where (>>=) = (Old.>>=)

instance Return (State s) where return = Old.return
instance Bind (State s) (State s) (State s) where (>>=) = (Old.>>=)

instance Return (Reader e) where return = Old.return
instance Bind (Reader e) (Reader e) (Reader e) where (>>=) = (Old.>>=)

-- lift some desirable mixins
instance Bind Maybe [] [] where
	Just a >>= f = f a
	Nothing >>= _  = []

instance Bind [] Maybe [] where
	xs >>= f = catMaybes $ map f xs

instance Bind STM IO IO where m >>= k = atomically m >>= k
instance Bind IO STM IO where m >>= k = m >>= (atomically . k)

instance Bind [] IO (ListT IO) where
	xs >>= f = ListT (mapM f xs)

-- perform it once or n times? don't decide 
--instance Bind IO [] (ListT IO) where m >>= f = 

testSTMIO = newTVar 2 >>= readTVar >>= print
testMaybeList = Just 2 >>= \x -> [x*1,x*2]
testIdentityMaybe = 
	returnM 2 >>= (Just . (*4)) >>= \y -> 
	[y,y+1] >>= \z -> 
	if z `mod` 2 == 0 then Just z else Nothing

