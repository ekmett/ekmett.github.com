{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Ring.Module
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (MPTCs)
--
-- Left- and right- modules over rings, semirings, and Seminearrings.
-- To avoid a proliferation of classes. These only require that there
-- be an addition and multiplication operation for the 'Ring'
--
-----------------------------------------------------------------------------

module Data.Ring.Module 
    ( module Data.Ring
    -- * R-Modules
    , Module
    , LeftModule, (*.)
    , RightModule, (.*)
    , Bimodule
    -- * R-Normed Modules
    , Normed, mabs
    -- * Vector Spaces
    , VectorSpace
    -- * R-Algebras
    , Algebra
    ) where

import Data.Ring
import Data.Monoid.Union

-- import qualified Data.Monoid.Combinators as Monoid


class (Ringoid r, Monoid m) => Module r m where

-- | @ (x * y) *. m = x * (y *. m) @
class (Module r m) => LeftModule r m where
    (*.) :: r -> m -> m
    
-- | @ (m .* x) * y = m .* (x * y) @
class (Module r m) => RightModule r m where
    (.*) :: m -> r -> m

-- | @ (x *. m) .* y = x *. (m .* y) @
class (LeftModule r m, RightModule r m) => Bimodule r m 

class (Field f, Module f g) => VectorSpace f g

-- | An r-normed module m satisfies:
--
-- (1) @mabs m >= 0@
--
-- 2 @mabs m == zero{-_r-} => m == zero{-_m-}@
--
-- 3 @mabs (m + n) <= mabs m + mabs n@
--
-- 4 @r * mabs m = mabs (r *. m) -- if m is an r-LeftModule@
--
-- 5 @mabs m * r = mabs (m .* r) -- if m is an r-RightModule@
class Module r m => Normed r m where
    mabs :: m -> r

-- | Algebra over a (near) (semi) ring.
-- @r *. (x * y) = (r *. x) * y = x * (r *. y)@
-- @(x * y) .* r = y * (x .* r) = (y .* r) * x@
class (r `Bimodule` m, Multiplicative m) => Algebra r m 

instance (Module r m, Module r n) => Module r (m,n)
instance (Module r m, Module r n, Module r o) => Module r (m,n,o)
instance (Module r m, Module r n, Module r o, Module r p) => Module r (m,n,o,p)
instance (Module r m, Module r n, Module r o, Module r p, Module r q) => Module r (m,n,o,p,q)

instance (LeftModule r m, LeftModule r n) => LeftModule r (m,n) where
    r *. (m,n) = (r *. m, r *. n)
instance (LeftModule r m, LeftModule r n, LeftModule r o) => LeftModule r (m,n,o) where
    r *. (m,n,o) = (r *. m, r *. n, r *. o)
instance (LeftModule r m, LeftModule r n, LeftModule r o, LeftModule r p) => LeftModule r (m,n,o,p) where
    r *. (m,n,o,p) = (r *. m, r *. n, r *. o, r *. p)
instance (LeftModule r m, LeftModule r n, LeftModule r o, LeftModule r p, LeftModule r q) => LeftModule r (m,n,o,p,q) where
    r *. (m,n,o,p,q) = (r *. m, r *. n, r *. o, r *. p, r *. q)

instance (RightModule r m, RightModule r n) => RightModule r (m,n) where
    (m,n) .* r = (m .* r, n .* r)
instance (RightModule r m, RightModule r n, RightModule r o) => RightModule r (m,n,o) where
    (m,n,o) .* r = (m .* r, n .* r, o .* r)
instance (RightModule r m, RightModule r n, RightModule r o, RightModule r p ) => RightModule r (m,n,o,p) where
    (m,n,o,p) .* r = (m .* r, n .* r, o .* r, p .* r)
instance (RightModule r m, RightModule r n, RightModule r o, RightModule r p, RightModule r q ) => RightModule r (m,n,o,p,q) where
    (m,n,o,p,q) .* r = (m .* r, n .* r, o .* r, p .* r, q .* r)

instance (Bimodule r m, Bimodule r n) => Bimodule r (m,n)
instance (Bimodule r m, Bimodule r n, Bimodule r o) => Bimodule r (m,n,o)
instance (Bimodule r m, Bimodule r n, Bimodule r o, Bimodule r p) => Bimodule r (m,n,o,p)
instance (Bimodule r m, Bimodule r n, Bimodule r o, Bimodule r p, Bimodule r q) => Bimodule r (m,n,o,p,q)

-- we want an absorbing 0, for that we need a seminearring and a notion of equality
instance (HasUnionWith f, Ord r, Eq r, RightSemiNearRing r) => LeftModule r (UnionWith f r) where
    r *. m | r == zero = zero
           | otherwise = fmap (r `times`) m
instance (HasUnionWith f, Ord r, Eq r, RightSemiNearRing r) => RightModule r (UnionWith f r) where
    m .* r | r == zero = zero
           | otherwise = fmap (`times` r) m
instance (HasUnionWith f, Ord r, Eq r, RightSemiNearRing r) => Module r (UnionWith f r) where
