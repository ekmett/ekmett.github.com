module Data.THash.Simple where

import Data.THash
import Control.Concurrent
import GHC.Conc
import Control.Monad
import Random
import System.Time
import Text.Printf
import System

main :: IO()
main =  do
        ourHashTable <- atomically (new hashInt)
	atomically $ do
        	insert ourHashTable 25 "Nehir"
                insert ourHashTable 24 "Cristian"
                insert ourHashTable 31 "Paul"
                insert ourHashTable 18 "Mariana"
                delete ourHashTable 25
        ourValues <- atomically (values ourHashTable)
        allPairs <- atomically (each ourHashTable)
        resultLookup <- atomically (Data.THash.lookup ourHashTable 24)
        print ourValues
        print allPairs
        print resultLookup
        print (hashInt 24)
