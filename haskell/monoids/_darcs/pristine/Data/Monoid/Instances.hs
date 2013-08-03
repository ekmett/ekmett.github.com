{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, OverloadedStrings, CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Instances
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- A collection of orphan instance declarations for Monoids that should
-- eventually be pushed back down to the source packages.
--
-- Every package that uses these instances includes this package internally.
--
-- Includes:
--
-- * 'Monoid' instances for the 'Monad' transformers from the mtl package
--
-- * A 'Monoid' instance for the 'ParsecT' 'Monad' from parsec-3.
--
-- * 'IsString' instances for tuples of 'IsString' for overloaded string support.
--
-- * A 'Monoid' instance for the 'FingerTree' in the fingertree package 
--
-- * 'Monoid' instances for 'Int', 'Integer', and 'Ratio' using @(+,0)@
--
-- * 'Num' and 'Bits' instances for 'Bool' as a 'Boolean' `&&`/`||` 'SemiRing'
--
-- This module is automatically included everywhere this functionality is required
-- within this package. You should only have to import this module yourself if you 
-- want these instances for your own purposes.
-----------------------------------------------------------------------------

module Data.Monoid.Instances () where

#ifdef M_MTL
import Control.Monad.Reader
import qualified Control.Monad.RWS.Lazy as LRWS
import qualified Control.Monad.RWS.Strict as SRWS
import qualified Control.Monad.State.Lazy as LState
import qualified Control.Monad.State.Strict as SState
import Control.Monad.Writer
import qualified Control.Monad.Writer.Strict as SWriter
#endif

#ifdef X_OverloadedStrings
import Data.String
#endif

import Data.Bits
import Data.Ratio

#ifdef M_FINGERTREE
import Data.FingerTree
#endif

#ifdef M_PARSEC
import Text.Parsec.Prim
#endif

#ifdef M_MTL
instance (MonadPlus m, Monoid w) => Monoid (SWriter.WriterT w m n) where
    mempty = mzero
    mappend = mplus

instance (MonadPlus m, Monoid w) => Monoid (WriterT w m n) where
    mempty = mzero
    mappend = mplus

instance (MonadPlus m, Monoid w) => Monoid (SRWS.RWST r w s m n) where 
    mempty = mzero
    mappend = mplus

instance (MonadPlus m, Monoid w) => Monoid (LRWS.RWST r w s m n) where 
    mempty = mzero
    mappend = mplus

instance MonadPlus m => Monoid (ReaderT e m n) where
    mempty = mzero
    mappend = mplus

instance MonadPlus m => Monoid (SState.StateT s m n) where
    mempty = mzero
    mappend = mplus

instance MonadPlus m => Monoid (LState.StateT s m n) where
    mempty = mzero
    mappend = mplus
#endif

#ifdef M_FINGERTREE
instance Measured v a => Monoid (FingerTree v a) where
    mempty = empty
    mappend = (><)
#endif

#ifdef M_PARSEC
instance Stream s m t => Monoid (ParsecT s u m a) where
    mempty = mzero
    a `mappend` b = try a <|> b
#endif

#ifdef X_OverloadedStrings
instance (IsString a, IsString b) => IsString (a,b) where
    fromString a = (fromString a, fromString a)

instance (IsString a, IsString b, IsString c) => IsString (a,b,c) where
    fromString a = (fromString a, fromString a, fromString a)

instance (IsString a, IsString b, IsString c, IsString d) => IsString (a,b,c,d) where
    fromString a = (fromString a, fromString a, fromString a, fromString a)

instance (IsString a, IsString b, IsString c, IsString d, IsString e) => IsString (a,b,c,d,e) where
    fromString a = (fromString a, fromString a, fromString a, fromString a, fromString a)
#endif

instance Monoid Int where
    mempty = 0
    mappend = (+)

instance Monoid Integer where
    mempty = 0
    mappend = (+)

instance Integral m => Monoid (Ratio m) where
    mempty = 0
    mappend = (+)

instance Monoid Bool where
    mempty = 0
    mappend = (||)

-- boolean semiring
instance Num Bool where
    (+) = (||)
    (*) = (&&)
    x - y = x && not y
    negate = not
    abs = id
    signum = id
    fromInteger 0 = False
    fromInteger _ = True

instance Bits Bool where
    (.&.)           = (&&)
    (.|.)           = (||)
    xor True True   = False
    xor False False = False
    xor _ _         = True
    complement      = not
    shiftL a b      = a && (b == 0)
    shiftR a b      = a && (b == 0)
    shift  a b      = a && (b == 0)
    rotate a _      = a
    bit             = (==0)
    setBit a b      = a || (b == 0)
    testBit a b     = a && (b == 0)
    bitSize _       = 1
    isSigned _      = False
