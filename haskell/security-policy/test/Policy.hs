module Policy (Classified, Secret, runSecret, runClassified) where

import Control.Monad (liftM)
import Control.Monad.Security
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent.MVar (MVar,newMVar,withMVar)
import Foreign.C.String (peekCAString,withCString,CString)
import System.Console.Readline (readline)
import Data.Typeable (Typeable(..))
import Data.Generics (Data(..))
import Data.Char (isAlpha, isDigit)
import Debug.Trace (trace)

-- | `Secret` > `Classified`. A simple two level lattice.
data Government
instance SecurityPolicy Government

unsecure = error "unsecured access attempt"

newtype Classified a = Classified { tellClassified :: a }
instance Typeable (Classified a) where
	typeOf = unsecure
instance Data (Classified a) where 
	gunfold = unsecure
	toConstr = unsecure
	dataTypeOf = unsecure
instance SecurityLevel Government Classified
instance Monad Classified where
        return = Classified
        a >>= f = f (tellClassified a)

newtype Secret a = Secret { tellSecret :: a }
instance Typeable (Secret a) where 
	typeOf = unsecure
instance Data (Secret a) where 
	gunfold = unsecure
	toConstr = unsecure
	dataTypeOf = unsecure
instance SecurityLevel Government Secret
instance Monad Secret where
        return = Secret
        a >>= f = f (tellSecret a)

-- | All n^2 combinations
instance SecurityLattice Government Classified Classified Classified where 
	witness = (>>=)
instance SecurityLattice Government Secret Secret Secret where 
	witness = (>>=)
instance SecurityLattice Government Classified Secret Secret where
        witness a f = f (tellClassified a)
instance SecurityLattice Government Secret Classified Secret where
        witness a f = reclassify (f (tellSecret a))

foreign import ccall "unistd.h getpass" getpass :: CString -> IO (CString)

-- | plz?
canHasStdio :: MVar ()
canHasStdio = unsafePerformIO $ newMVar ()

-- | we only let you extract secret things that we can show in the logs later
runSecret :: (SecurityLattice Government m Secret Secret, Show a) => m a -> IO (Maybe a)
runSecret s = withMVar canHasStdio $ const $ do 
	user <- liftM (maybe "" (filter (\x -> isAlpha x || isDigit x))) $ readline "Secret Username: "
	pass <- peekCAString =<< withCString "Secret Password: " getpass 
	if user == "edwardk" && pass == "c0m0n4d"
		then do logOk user
			return $ Just theSecret
	        else do logFail user
			return Nothing
 	where 
		logOk u = putStrLn ("user " ++ u ++ " accessed fact " ++ show theSecret)
		logFail u = putStrLn ("access denied for user " ++ u)
		theSecret = tellSecret $ reclassify s


-- | allow anyone to extract classified data if they just say please.
runClassified :: Classified a -> a 
runClassified = tellClassified
