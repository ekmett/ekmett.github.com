---------------------------------------------------------------------------
-- |
-- Module      :  Data.THash.THT
-- Copyright   :  (C) 2006 Edward Kmett
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- "Data.THash" Internals. Unless you really want to do the plumbing yourself
-- you probably want to use that instead.
-- 
-- There is a nearby point in the design space that generates a traditional
-- sorted linear hash table which will output keys and values in the same 
-- order as long as both hashes have the same set of keys, regardless of 
-- insertion order. To get there we would need to maintain the linked lists 
-- in sorted order.
----------------------------------------------------------------------------

module Data.THash.THT (
    THT,     -- Eq key => THT key value
    new,     -- (k -> Int) ->                              STM (THT k v)
    fromList,-- Eq k => (k -> Int) -> [(k,v)] ->           STM (THT k v)
    insert,  -- Eq k => THT k v -> k -> v ->               STM (THT k v, Bool)
    update,  -- Eq k => THT k v -> k -> v ->               STM (THT k v)
    modify,  -- Eq k => THT k v -> k -> (Maybe v -> v) ->  STM (THT k v)
    delete,  -- Eq k => THT k v -> k ->                    STM (THT k v, Bool)
    lookup,  -- Eq k => THT k v -> k ->                    STM (Maybe v)
    mapH,    --((k,v) -> r) -> THT k v ->                  STM [r]
    each,    -- THT k v ->                                 STM [(k,v)]
    keys,    -- THT k v ->                                 STM [k]
    values   -- THT k v ->                                 STM [v]
) where 

import Prelude
    ( Show(..), Ord(..), Eq, Bool(..), Maybe(..)
    , Num, Int
    , (*), (+), (-), ($), (==), (++), (.), (/=)
    , mapM_, sequence_, sequence, return, mod, fst, snd, id
    , otherwise
    )
import Data.Bits
import Data.Array
import Control.Monad (liftM, replicateM,when)
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import qualified Data.List as List (partition, lookup, length, concatMap, map)
import Foreign (unsafePerformIO)

data THT k v = MkTHT
    { slots       :: Array Int (TVar [(k, v)])
    , mask        :: !Int
    , count	  :: !Int
    , hash        :: k -> Int
    }

{-# INLINE stride #-}
stride :: THT k v -> Int
stride this = (mask this + 1) `shiftR` 1

{-# INLINE capacity #-}
capacity :: THT k v -> Int
capacity this = let (l,u) = bounds $ slots this in u - l + 1

{-# INLINE new #-}
new :: (k -> Int) -> STM (THT k v)
new hash = do 
    slots <- replicateM 4 $ newTVar []
    return MkTHT
        { slots     = listArray (0,3) slots
        , mask 	    = 0
	, count     = 0
        , hash      = hash 
        }

{-# INLINE fromList #-}
fromList :: Eq k => (k -> Int) -> [(k,v)] -> STM(THT k v)
fromList hash list = do
    slots <- replicateM capacity $ newTVar []
    let this = MkTHT
            { slots     = listArray (0,mask) slots
	    , count     = count
            , mask      = mask
            , hash      = hash
            }
        put (key,value) = do
            let loc = chain this key
            list <- readTVar loc
            writeTVar loc $ (key,value):list
    mapM_ put list
    return this
  where
    count = List.length list 
    pow2 m | count < m = m
           | otherwise = pow2 (m+m)
    capacity = 2*pow2 1 
    mask = capacity - 1

-- TODO fix from here down
{-# INLINE addBucket #-}
addBucket :: THT k v -> STM (THT k v)
addBucket this = do
    slots' <- maybeGrow $ slots this
    addBucket' this slots'
  where 
    maybeGrow | count this == capacity this = grow 
	      | otherwise  = return

{-# INLINE addBucket' #-}
addBucket' :: THT k v -> Array Int (TVar [(k, v)]) -> STM (THT k v)
addBucket' this slots' = do 
    when (count this > 0) $ do
	ll <- readTVar left
	let (ol,nl) = List.partition (\(k,v) -> old == locate this' k) ll
	writeTVar left ol
	writeTVar right nl
    return this'
  where
    count' = count this + 1
    mask'  = mask this .|. count'
    this'  = this { count = count', mask = mask', slots = slots' }
    new    = count this		-- intentionally using previous count
    old    = new - stride this  -- intentionally using previous count and stride
    left   = slots' ! old 
    right  = slots' ! new 
            
{-# INLINE removeBucket #-}
removeBucket :: THT k v -> STM (THT k v)
removeBucket this = do
    list1 <- readTVar right
    list2 <- readTVar left
    writeTVar left (list1 ++ list2)
    writeTVar right []
    return this'
  where 
    count'  = count this - 1
    mask'   = if count' < stride this 
		then mask this - stride this 
		else mask this
    stride' = (mask' + 1) `shiftR` 2
    this' = this { count = count', mask = mask' } 
    left  = slots this' ! (count this' - stride this')
    right = slots this' ! count this'
    
{-# INLINE lookup #-}
lookup :: Eq k => THT k v -> k -> STM (Maybe v)
lookup this key = if (count this == 0) then return Nothing else do
    list <- readTVar $ chain this key
    return $ List.lookup key list

{-# INLINE insert #-}
insert :: Eq k => THT k v -> k -> v -> STM (THT k v, Bool)
insert this key value = do
    list <- readTVar $ chain this key
    case List.lookup key list of 
        Just _ -> return (this,False)
        (Nothing) -> do
            this' <- addBucket this
            let tvar = chain this' key
            list' <- readTVar tvar
            writeTVar tvar $ (key,value):list'
            return (this',True)

{-# INLINE update #-}
update :: Eq k => THT k v -> k -> v -> STM (THT k v)
update this key value = modify this key $ \_ -> value

{-# INLINE modify #-}
modify :: Eq k => THT k v -> k -> (Maybe v -> v) -> STM (THT k v)
modify this key f = do
    list <- readTVar old
    case List.lookup key list of 
        Just value -> do 
            writeTVar old $ List.map fixup list 
            return this
        Nothing -> liftM fst . insert this key . f $ Nothing
    where
    old = chain this key
    fixup (k,v) | k == key  = (k, f $ Just v)
                | otherwise = (k, v)

{-# INLINE delete #-}
delete :: Eq k => THT k v -> k -> STM (THT k v, Bool)
delete this key = do 
    let tvar = chain this key
    list <- readTVar $ tvar
    case strip key list of 
        (list', True) -> do 
            writeTVar tvar list'
            this' <- removeBucket this
            return (this', True)
        (_, False) -> return (this, False)

{-# INLINE strip #-}
strip :: Eq k => k -> [(k,v)] -> ([(k,v)],Bool)
strip key list = strip' key list []

strip' :: Eq k => k -> [(k,v)] -> [(k,v)] -> ([(k,v)],Bool)
strip' key ((key',val):tail) head
    | key == key'   = (head ++ tail, True) -- delete
    | otherwise     = strip' key tail $ (key',val):head
strip' _ [] head = (head, False) 

{-# INLINE grow #-}
-- replace a numerically indexed array of TVars of lists with one twice its size.
grow :: (Ix i, Num i) => Array i (TVar [t]) -> STM (Array i (TVar [t]))
grow a = do
    top <- replicateM n $ newTVar []
    return $ listArray (l,h') $ elems a ++ top
  where
    (l,h) = bounds a
    l' = h + 1
    h' = l' + h - l
    n = rangeSize(l',h')
        
{-# INLINE bin #-}
-- figure out what bin a given hashed value is mapped to.
bin :: THT k v -> Int -> Int
bin this val = 
    if residue >= count this 
     then residue - stride this 
     else residue
  where
    residue = val .&. mask this 

{-# INLINE locate #-}
-- translate a key to its current bin
locate :: THT k v -> k -> Int
locate this key = bin this $ hash this key

{-# INLINE chain #-}
-- translate a key to a tvar in which we might find it
chain :: THT k v -> k -> TVar [(k,v)]
chain this key = slots this ! locate this key

{-# INLINE mapH #-}
mapH :: ((k,v) -> r) -> THT k v -> STM [r]
mapH f this = do 
    lists <- sequence [ readTVar $ slots this ! i | i <- [0..count this] ]
    return $ List.concatMap (List.map f) lists
    
{-# INLINE each #-}
each :: THT k v -> STM [(k,v)]
each = mapH id 

{-# INLINE keys #-}
keys :: THT k v -> STM [k]
keys = mapH fst

{-# INLINE values #-}
values :: THT k v -> STM [v]
values = mapH snd

