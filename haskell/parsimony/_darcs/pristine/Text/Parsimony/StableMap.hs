{-# LANGUAGE EmptyDataDecls #-}
module Text.Parsimony.StableMap
    ( StableMap
    , empty
    , insert
    , update
    , Text.Parsimony.StableMap.lookup
    ) where

-- map a StableName of an object to a functor of information about that value

import Prelude

import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap) 

import System.Mem.StableName
import Unsafe.Coerce

import Text.Parsimony.Util (Magic)

newtype StableMap f = StableMap { getStableMap :: IntMap [(StableName Magic, f Magic)] } 

empty :: StableMap f
empty = StableMap IntMap.empty

insert :: StableName a -> f a -> StableMap f -> StableMap f
insert k v = StableMap . IntMap.insertWith (++) (hashStableName k) [unsafeCoerce (k, v)] . getStableMap

lookup :: StableName a -> StableMap f -> Maybe (f a)
lookup k (StableMap m) = do
    x <- IntMap.lookup (hashStableName k) m
    unsafeCoerce $ Prelude.lookup (unsafeCoerce k) x

update :: StableName a -> f a -> StableMap f -> StableMap f
update k v = StableMap . IntMap.adjust update' (hashStableName k) . getStableMap where
    k' :: StableName Magic
    k' = unsafeCoerce k

    v' :: f Magic
    v' = unsafeCoerce v

    update' :: [(StableName Magic, f Magic)] -> [(StableName Magic, f Magic)] 
    update' ((k'',v''):ks) 
        | k' == k''  = (k'',v'):ks
        | otherwise = (k'',v''):update' ks
    update' [] = []
