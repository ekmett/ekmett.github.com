{-# OPTIONS -fallow-undecidable-instances #-}
module Main where
-- Raw pointer manipulation round-trip through C could force it
-- and an unsound combination of newtype deriving and data families can also force it
-- it can also be extracted by using Bjorn Bringert's external standalone deriving Foo for 
-- Bar clauses. (thanks Bjorn)
-- However, in the absence of these a dialect of haskell that is hsplugin safe could allow
-- arbitrary user code to access secured data.

import Control.Monad.Security
import Policy
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Typeable

agents :: Secret [(String,Bool)]
agents = return 
	[ ("Jimmy Hoffa", True)
	, ("John F. Kennedy", True)
	, ("Eve",True)
	, ("Mallory",True)
	, ("Valerie Plame", False)
	, ("Fidel Castro", False)
	]

suspects :: Classified [String] 
suspects = return ["Alice","Bob","Eve"]

phonebook :: Secret (Map String String)
phonebook = return $ Map.fromList 
	[ ("Alice","313-555-5555")
	, ("Bob","734-555-1111")
	, ("Eve","810-555-2222") 
	]

lookupSuspect :: String -> Secret (Maybe String)
lookupSuspect name = do
	-- should cost nothing to reclassify, as this just trades in newtypes
	xs <- reclassify suspects
	if elem name xs
		then do pb <- phonebook
			return $ Map.lookup name pb
	   	else return Nothing

main = do
	answer <- runSecret (lookupSuspect "Eve")
	print answer
	
-- can't get ahold of Government to define a Subversive typeclass, and we can't get a witness to extract the data
-- newtype Subversive a = Subversive { runSubversive :: a }
-- instance SecurityLevel a Classified => SecurityLevel a Subversive
-- instance SecurityLattice a Secret Subversive Subversive where witness = undefined

{-
test/Main.hs:42:0:
    Couldn't match expected type `a' (a rigid variable)
           against inferred type `Policy.Government'
      `a' is bound by the instance declaration at test/Main.hs:42:0
    When using functional dependencies to combine
      SecurityLevel Policy.Government Classified,
        arising from the instance declaration at Defined in Policy
      SecurityLevel a Classified,
        arising from is bound by the instance declaration at test/Main.hs:42:0
        at test/Main.hs:42:0
    When checking the super-classes of an instance declaration
    In the instance declaration for `SecurityLevel a Subversive'
-}

--	let answer = runSubversive (reclassify (lookupSuspect "Eve") :: Subversive (Maybe String))
--	let answer = cast agents :: Maybe [(String,Bool)]
