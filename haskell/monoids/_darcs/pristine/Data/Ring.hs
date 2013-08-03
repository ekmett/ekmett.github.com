{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Ring
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  portable (instances use MPTCs)
--
--
-- Defines left- and right- seminearrings. Every 'MonadPlus' wrapped around
-- a 'Monoid' qualifies due to the distributivity of (>>=) over 'mplus'.
--
-- See <http://conway.rutgers.edu/~ccshan/wiki/blog/posts/WordNumbers1/>
--
-----------------------------------------------------------------------------

module Data.Ring
    ( module Data.Group
    , Ringoid
    , LeftSemiNearRing
    , RightSemiNearRing
    , SemiRing
    , Ring
    , DivisionRing
    , Field
    ) where

import Data.Group
import Data.Monoid.Self

#ifdef X_OverloadedStrings
import Data.Monoid.FromString
#endif

#ifdef M_MTL
import Control.Monad.Reader
import qualified Control.Monad.RWS.Lazy as LRWS
import qualified Control.Monad.RWS.Strict as SRWS
import qualified Control.Monad.State.Lazy as LState
import qualified Control.Monad.State.Strict as SState
import qualified Control.Monad.Writer.Lazy as LWriter
import qualified Control.Monad.Writer.Strict as SWriter
#endif

#ifdef M_FINGERTREE
import Data.FingerTree
#endif

#ifdef M_CONTAINERS
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
#endif

#ifdef M_PARSEC
import Text.Parsec.Prim
#endif

#ifdef X_OverloadedStrings
import Data.Monoid.FromString
#endif

-- | @0@ annihilates `times`
class (Multiplicative m, Monoid m) => Ringoid m
instance Ringoid Integer
instance Ringoid Int
instance Ringoid m => Ringoid (Self m)
instance Ringoid m => Ringoid (Dual m)
instance Monoid m => Ringoid [m]
instance Monoid m => Ringoid (Maybe m)

-- | @a * (b + c) = (a * b) + (a * c)@
class Ringoid m => LeftSemiNearRing m 
instance LeftSemiNearRing m => LeftSemiNearRing (Self m)
instance RightSemiNearRing m => LeftSemiNearRing (Dual m)

-- | @(a + b) * c = (a * c) + (b * c)@
class Ringoid m => RightSemiNearRing m 
instance RightSemiNearRing m => RightSemiNearRing (Self m)
instance LeftSemiNearRing m => RightSemiNearRing (Dual m)
instance Monoid m => RightSemiNearRing [m]
instance Monoid m => RightSemiNearRing (Maybe m)

-- | A 'SemiRing' is an instance of both 'Multiplicative' and 'Monoid' where 
--   'times' distributes over 'plus'.
class (RightSemiNearRing a, LeftSemiNearRing a) => SemiRing a
instance SemiRing r => SemiRing (Self r)
instance SemiRing r => SemiRing (Dual r)

class (Group a, SemiRing a) => Ring a
instance Ring r => Ring (Self r)
instance Ring r => Ring (Dual r)

class (Ring a, MultiplicativeGroup a) => DivisionRing a
instance DivisionRing r => DivisionRing (Self r)
instance DivisionRing r => DivisionRing (Dual r)

class (Ring a, MultiplicativeGroup a) => Field a
instance Field f => Field (Dual f)
instance Field f => Field (Self f)

#ifdef M_REFLECTION
instance Ringoid m => Ringoid (ReducedBy m s)
instance LeftSemiNearRing m => LeftSemiNearRing (ReducedBy m s)
instance RightSemiNearRing m => RightSemiNearRing (ReducedBy m s)
instance SemiRing r => SemiRing (ReducedBy r s)
instance Ring r => Ring (ReducedBy r s)
instance DivisionRing r => DivisionRing (ReducedBy r s)
instance Field f => Field (ReducedBy f s)
#endif

#ifdef M_PARSEC
instance (Stream s m t, Monoid a) => Ringoid (ParsecT s u m a)
instance (Stream s m t, Monoid a) => RightSemiNearRing (ParsecT s u m a)
#endif

#ifdef M_MTL
instance (MonadPlus m, Monoid n) => Ringoid (SState.StateT s m n)
instance (MonadPlus m, Monoid n) => Ringoid (LState.StateT s m n)
instance (MonadPlus m, Monoid n) => Ringoid (ReaderT e m n)
instance (MonadPlus m, Monoid w, Monoid n) => Ringoid (SRWS.RWST r w s m n)
instance (MonadPlus m, Monoid w, Monoid n) => Ringoid (LRWS.RWST r w s m n)
instance (MonadPlus m, Monoid w, Monoid n) => Ringoid (SWriter.WriterT w m n)
instance (MonadPlus m, Monoid w, Monoid n) => Ringoid (LWriter.WriterT w m n)
instance (MonadPlus m, Monoid n) => RightSemiNearRing (SState.StateT s m n)
instance (MonadPlus m, Monoid n) => RightSemiNearRing (LState.StateT s m n)
instance (MonadPlus m, Monoid n) => RightSemiNearRing (ReaderT e m n)
instance (MonadPlus m, Monoid w, Monoid n) => RightSemiNearRing (SRWS.RWST r w s m n)
instance (MonadPlus m, Monoid w, Monoid n) => RightSemiNearRing (LRWS.RWST r w s m n)
instance (MonadPlus m, Monoid w, Monoid n) => RightSemiNearRing (SWriter.WriterT w m n)
instance (MonadPlus m, Monoid w, Monoid n) => RightSemiNearRing (LWriter.WriterT w m n)
#endif

#ifdef M_FINGERTREE
instance (Measured v m, Monoid m) => Ringoid (FingerTree v m)
instance (Measured v m, Monoid m) => RightSemiNearRing (FingerTree v m)
#endif

#ifdef M_CONTAINERS
instance Monoid m => Ringoid (Seq m)
instance Monoid m => RightSemiNearRing (Seq m)
#endif

#ifdef X_OverloadedStrings
instance Ringoid m => Ringoid (FromString m)
instance RightSemiNearRing m => RightSemiNearRing (FromString m)
instance LeftSemiNearRing m => LeftSemiNearRing (FromString m)
instance SemiRing r => SemiRing (FromString r)
instance Ring r => Ring (FromString r)
instance DivisionRing r => DivisionRing (FromString r)
instance Field f => Field (FromString f)
#endif
