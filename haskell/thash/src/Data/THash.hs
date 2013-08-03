{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.THash
-- Copyright   :  (C) 2006 Edward Kmett
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- A simple "STM" based transactional linear hash table based on Witold 1980.
-- This wraps a "Data.THash.THT" in an simple container. It may be
-- more appropriate to use the underlying "THT" structure directly if you are
-- nesting these. The performance hit hasn't yet been measured.
----------------------------------------------------------------------------

module Data.THash (
    THash,  
    new,     -- (k -> Int) ->                               STM (THash k v)
    newH,    -- Hashable k => 				    STM (THash k v)
    fromList,-- Eq k => (k -> Int) -> [(k,v)] ->            STM (THash k v)
    insert,  -- Eq k => THash k v -> k -> v ->              STM (Bool)
    update,  -- Eq k => THash k v -> k -> v ->              STM ()
    modify,  -- Eq k => THash k v -> k -> (Maybe v -> v) -> STM ()
    delete,  -- Eq k => THash k v -> k ->                   STM (Bool)
    lookup,  -- Eq k => THash k v -> k ->                   STM (Maybe v)
    mapH,    -- ((k,v) -> r) -> THash k v ->                STM [r]
    each,    -- THash k v ->                                STM [(k,v)]
    keys,    -- THash k v ->                                STM [k]
    values,  -- THash k v ->                                STM [v]
    hashInt, -- Int -> Int
    get,
    -- hashString -- Int -> Int
) where 

import qualified Data.THash.THT as THT hiding(THT)
import Data.Hashable
import Data.THash.THT (THT)
import Prelude hiding (lookup)
import Control.Monad (liftM)
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Data.Bits

-- | A hash with keys of type k to values of type v
newtype THash k v = THash (TVar (THT k v))

{-# INLINE make #-}
make :: THT k v -> STM (THash k v)
make t = do x <- newTVar t; return $ THash x

{-# INLINE get #-}
get :: THash k v -> STM (THT k v)
get (THash h) = readTVar h

{-# INLINE set #-}
set :: THash k v -> THT k v -> STM ()
set (THash h) = writeTVar h 

{-# INLINE setif #-}
setif :: THash k v -> (THT k v,Bool) -> STM (Bool)
setif (THash h) (t,b) 
    | b == True 
    = writeTVar h t >> return True
    | otherwise
    = return False

{-# INLINE new #-}
-- | Build an empty hash table
new :: (k -> Int) -> STM (THash k v)
new hash = make =<< THT.new hash

{-# INLINE newH #-}
-- | Build an empty hash table using the default hash function for the key type.
newH :: Hashable k => STM (THash k v)
newH = new hash

{-# INLINE fromList #-}
-- | Build a hash table from a list of @(key,value)@ pairs
fromList :: Eq k => (k -> Int) -> [(k,v)] ->             STM (THash k v)
fromList hash list = make =<< THT.fromList hash list

{-# INLINE insert #-}
-- | Insert a value into the hash table. If a value with the key is present
-- then nothing is changed and 'False' is returned.
insert :: Eq k => THash k v -> k -> v ->               STM (Bool)
insert hash key value = do x <- get hash; y <- THT.insert x key value; setif hash y

{-# INLINE update #-}
-- | Insert a value into the hash table, replacing any value with the same key that is present.
update :: Eq k => THash k v -> k -> v -> STM ()
update hash key value = do x <- get hash; y <- THT.update x key value; set hash y

{-# INLINE modify #-}
-- | Update a value in the hash table using the supplied function. 
modify :: Eq k => THash k v -> k -> (Maybe v -> v) -> STM ()
modify hash key f = do x <- get hash; y <- THT.modify x key f ; set hash y

{-# INLINE delete #-}
-- | Remove a value from the hash table. Returns 'True' to indicate success.
delete :: Eq k => THash k v -> k -> STM (Bool)
delete hash key = do x <- get hash; y <- THT.delete x key; setif hash y

{-# INLINE lookup #-}
-- | Lookup a value in the hash table. 
lookup :: Eq k => THash k v -> k -> STM (Maybe v)
lookup hash key = do x <- get hash; THT.lookup x key

{-# INLINE mapH #-}
-- | Map a function over all @(key,value)@ functions in the hash table.
mapH :: ((k,v) -> r) -> THash k v -> STM [r]
mapH f hash = do x <- get hash; THT.mapH f x 

{-# INLINE each #-}
-- | @each = mapH id@ and returns all @(key,value)@ pairs in the hash.
each :: THash k v -> STM [(k,v)]
each = mapH id

{-# INLINE keys #-}
-- | @each = mapH fst@ and returns all keys in the hash.
keys   :: THash k v -> STM [k]
keys = mapH fst

{-# INLINE values #-}
-- | @each = mapH snd@ and returns all values present in the hash.
values :: THash k v -> STM [v]
values = mapH snd

{-# INLINE hashInt #-}
-- | Thomas Wang's 32 bit mix function; more effective than a prime modulus for 
-- declustering a linear hash, but not good against an adversary, since its easily 
-- reversed.
hashInt :: Int -> Int
hashInt = ap xor (`shiftR` 16) 
	. ap (+) (complement . (`shiftL` 11)) 
	. ap xor (`shiftR` 6) 
	. ap (+) (`shiftL` 3) 
	. ap xor (`shiftR` 10)
	. ap (+) (complement . (`shiftL` 15))
	where ap x y z = x z $ y z 
