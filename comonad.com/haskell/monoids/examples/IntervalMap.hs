module Data.IntervalMap where

import Control.Functor.Pointed
import Data.Monoid.Ord
import Data.FingerTree

newtype Interval k v = Interval (k,k) v

instance Functor (Interval k) where
    fmap f (Interval i v) = Interval i (f v)

instance Bounded k => Pointed (Interval k) where
    point = Interval (minBound,maxBound) v

instance Copointed (Interval k) where
    copoint (Interval _ v) = v

instance (Ord k, Bounded k) => Measured (Min k, Max k) (Interval k v)
    measure (Interval (l,h) v) = (Min l,Max h)

data IntervalMap k v = Data.FingerTree v (Interval k v)

insert :: (k,k) -> v -> IntervalMap k v -> IntervalMap k v
insert (lo,hi) value tree = where
    (l,m) = split ((lo<).getMin.first) tree
    (n,r) = split ((hi<).getMax.first) tree
