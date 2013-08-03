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
instance Policy Government

newtype Classified a = Classified { tellClassified :: a }

instance Typeable (Classified a) where typeOf = unsecure
instance Data (Classified a) where gunfold = unsecure; toConstr = unsecure; dataTypeOf = unsecure

instance Monad Classified where
        return = Classified
        a >>= f = f (tellClassified a)

instance PolicyLevel Government Classified

-- insert unsafePerformIO log step here
unsecure = error "unsecured access attempt"

-- | Secret 
newtype Secret a = Secret { tellSecret :: a }

instance Typeable (Secret a) where typeOf = unsecure
instance Data (Secret a) where gunfold = unsecure; toConstr = unsecure; dataTypeOf = unsecure

instance Monad Secret where
        return = Secret
        a >>= f = f (tellSecret a)

instance PolicyLevel Government Secret

instance PolicyLattice Classified Classified Classified where witness = (>>=)
instance PolicyLattice Secret Secret Secret where witness = (>>=)

-- | n^2 - n instances. 
instance PolicyLattice Classified Secret Secret where
        witness a f = f (tellClassified a)

instance PolicyLattice Secret Classified Secret where
        witness a f = reclassify (f (tellSecret a))


foreign import ccall "unistd.h getpass" getpass :: CString -> IO (CString)

canHasStdio :: MVar ()
canHasStdio = unsafePerformIO $ newMVar ()

-- we only let you extract secret things that we can show in the logs later
runSecret :: (Access m Secret, Show a) => m a -> IO (Maybe a)
runSecret s = withMVar canHasStdio $ const $ do 
	user <- liftM (maybe "" (filter (\x -> isAlpha x || isDigit x))) $ readline "Secret Username: "
	pass <- peekCAString =<< withCString "Secret Password: " getpass 
	if user == "edwardk" && pass == "monad"
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
