{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Data.Monoid.Union
    ( module Data.Monoid.Reducer
    -- * Unions of Containers
    , HasUnion
    , empty
    , union
    , Union(Union,getUnion)
    -- * Unions of Containers of Monoids
    , HasUnionWith
    , emptyWith
    , unionWith
    , UnionWith(UnionWith,getUnionWith)
    ) where

import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)

import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.List as List

import Control.Functor.Pointed

import Data.Monoid.Reducer

-- | A Container suitable for the 'Union' 'Monoid'
class HasUnion f where
    empty :: f
    {-# SPECIALIZE union :: IntMap a -> IntMap a -> IntMap a #-}
    {-# SPECIALIZE union :: Ord k => Map k a -> Map k a -> Map k a #-}
    {-# SPECIALIZE union :: Eq a => [a] -> [a] -> [a] #-}
    {-# SPECIALIZE union :: Ord a => Set a -> Set a -> Set a #-}
    {-# SPECIALIZE union :: IntSet -> IntSet -> IntSet #-}
    union :: f -> f -> f

instance HasUnion (IntMap a) where
    empty = IntMap.empty
    union = IntMap.union

instance Ord k => HasUnion (Map k a) where
    empty = Map.empty
    union = Map.union

instance Eq a => HasUnion [a] where
    empty = []
    union = List.union

instance Ord a => HasUnion (Set a) where
    empty = Set.empty
    union = Set.union

instance HasUnion IntSet where
    empty = IntSet.empty
    union = IntSet.union

-- | The 'Monoid' @('union','empty')@
newtype Union f = Union { getUnion :: f } 
    deriving (Eq,Ord,Show,Read)

instance (HasUnion f) => Monoid (Union f) where
    mempty = Union empty
    Union a `mappend` Union b = Union (a `union` b)

instance (HasUnion f) => Reducer f (Union f) where
    unit = Union

instance Functor Union where
    fmap f (Union a) = Union (f a)

instance Pointed Union where 
    point = Union

instance Copointed Union where
    extract = getUnion

-- | Polymorphic containers that we can supply an operation to handle unions with
class Functor f => HasUnionWith f where
    {-# SPECIALIZE unionWith :: (a -> a -> a) -> IntMap a -> IntMap a -> IntMap a #-}
    {-# SPECIALIZE unionWith :: Ord k => (a -> a -> a) -> Map k a -> Map k a -> Map k a #-}
    unionWith :: (a -> a -> a) -> f a -> f a -> f a
    emptyWith :: f a 

instance HasUnionWith IntMap where 
    emptyWith = IntMap.empty
    unionWith = IntMap.unionWith

instance Ord k => HasUnionWith (Map k) where 
    emptyWith = Map.empty
    unionWith = Map.unionWith

-- | The 'Monoid' @('unionWith mappend','empty')@ for containers full of monoids.
newtype UnionWith f m = UnionWith { getUnionWith :: f m } 
    deriving (Eq,Ord,Show,Read,Functor,Pointed,Monad)

instance (HasUnionWith f, Monoid m) => Monoid (UnionWith f m) where
    mempty = UnionWith emptyWith
    UnionWith a `mappend` UnionWith b = UnionWith (unionWith mappend a b)

instance (HasUnionWith f, Monoid m) => Reducer (f m) (UnionWith f m) where
    unit = UnionWith

-- we want an absorbing 0, for that we need a seminearring and a notion of equality
