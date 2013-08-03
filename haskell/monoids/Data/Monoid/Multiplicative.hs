{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Multiplicative
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  portable (but instances use MPTCs)
--
-- When dealing with a 'Ring' or other structure, you often need a pair of 
-- 'Monoid' instances that are closely related. Making a @newtype@ for one
-- is unsatisfying and yields an unnatural programming style. 
--
-- A 'Multiplicative' is a 'Monoid' that is intended for use in a scenario
-- that can be extended to have another 'Monoid' slot in for addition. This
-- enables one to use common notation.
--
-- Any 'Multiplicative' can be turned into a 'Monoid' using the 'Log' wrapper.
--
-- Any 'Monoid' can be turned into a 'Multiplicative' using the 'Exp' wrapper.
--
-- Instances are supplied for common Monads of Monoids, in a fashion 
-- which can be extended if the 'Monad' is a 'MonadPlus' to yield a 'RightSemiNearRing'
--
-- Instances are also supplied for common Applicatives of Monoids, in a
-- fashion which can be extended if the 'Applicative' is 'Alternative' to
-- yield a 'RightSemiNearRing'
-----------------------------------------------------------------------------

module Data.Monoid.Multiplicative 
    ( module Data.Monoid.Additive
    -- * Multiplicative Monoids
    , Multiplicative
    , one, times
    -- * Multiplicative to Monoid
    , Log(Log, getLog)
    -- * Monoid to Multiplicative
    , Exp(Exp, getExp)
    ) where

import Control.Applicative
import Data.Monoid.Additive
import Data.Generator
import Data.Monoid.Instances ()
import Data.Monoid.Self
import Data.Ratio

#ifdef M_STM
import Control.Concurrent.STM
#endif

#ifdef M_MTL
import Control.Monad.Cont
import Control.Monad.Identity
import Control.Monad.Reader
import qualified Control.Monad.RWS.Lazy as LRWS
import qualified Control.Monad.RWS.Strict as SRWS
import qualified Control.Monad.State.Lazy as LState
import qualified Control.Monad.State.Strict as SState
import qualified Control.Monad.Writer.Lazy as LWriter
import qualified Control.Monad.Writer.Strict as SWriter
import qualified Control.Monad.ST.Lazy as LST
import qualified Control.Monad.ST.Strict as SST
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

class Multiplicative m where
    one :: m
    times :: m -> m -> m

instance Multiplicative m => Multiplicative (Dual m) where
    one = Dual one
    Dual x `times` Dual y = Dual (y `times` x)

instance Multiplicative m => Multiplicative (m `ReducedBy` s) where
    one = Reduction one
    Reduction x `times` Reduction y = Reduction (x `times` y)

-- | Convert a 'Multiplicative' into a 'Monoid'. Mnemonic: @Log a + Log b = Log (a * b)@
data Log m = Log { getLog :: m }

instance Multiplicative m => Monoid (Log m) where
    mempty = Log one
    Log a `mappend` Log b = Log (a `times` b)

-- | Convert a 'Monoid' into a 'Multiplicative'. Mnemonic: @Exp a * Exp b = Exp (a + b)@
data Exp m = Exp { getExp :: m }

instance Monoid m => Multiplicative (Exp m) where
    one = Exp mempty
    Exp a `times` Exp b = Exp (a `mappend` b)

instance Multiplicative m => Multiplicative (Self m) where
    one = Self one  
    Self a `times` Self b = Self (a `times` b)

-- Monad instances
instance Monoid m => Multiplicative [m] where
    one = return mempty
    times = liftM2 mappend
instance Monoid m => Multiplicative (Maybe m) where
    one = return mempty
    times = liftM2 mappend
instance Monoid n => Multiplicative (IO n) where
    one = return mempty
    times = liftM2 mappend
instance Monoid n => Multiplicative (SST.ST s n) where
    one = return mempty
    times = liftM2 mappend
instance Monoid n => Multiplicative (LST.ST s n) where
    one = return mempty
    times = liftM2 mappend

-- Applicative instances
instance Monoid n => Multiplicative (ZipList n) where
    one = pure mempty
    times = liftA2 mappend

instance Monoid m => Multiplicative (Const m a) where
    one = pure undefined
    times = liftA2 undefined

-- Numeric instances
instance Multiplicative Int where
    one = 1
    times = (*)

instance Multiplicative Integer where
    one = 1
    times = (*)

instance Integral m => Multiplicative (Ratio m) where
    one = 1
    times = (*)

#ifdef M_CONTAINERS
instance Monoid m => Multiplicative (Seq m) where
    one = return mempty
    times = liftM2 mappend
#endif

#ifdef M_FINGERTREE
-- and things that can't quite be a Monad in Haskell
instance (Measured v m, Monoid m) => Multiplicative (FingerTree v m) where
    one = singleton mempty
    xss `times` yss = getSelf $ mapReduce (flip fmap' yss . mappend) xss
#endif

#ifdef M_MTL
instance Monoid m => Multiplicative (Identity m) where
    one = return mempty
    times = liftM2 mappend
instance (Monoid m) => Multiplicative (Cont r m) where
    one = return mempty
    times = liftM2 mappend
instance (Monoid w, Monoid m) => Multiplicative (SRWS.RWS r w s m) where
    one = return mempty
    times = liftM2 mappend
instance (Monoid w, Monoid m) => Multiplicative (LRWS.RWS r w s m) where
    one = return mempty
    times = liftM2 mappend
instance Monoid m => Multiplicative (SState.State s m) where
    one = return mempty
    times = liftM2 mappend
instance Monoid m => Multiplicative (LState.State s m) where
    one = return mempty
    times = liftM2 mappend
instance Monoid m => Multiplicative (Reader e m) where
    one = return mempty
    times = liftM2 mappend
instance (Monoid w, Monoid m) => Multiplicative (SWriter.Writer w m) where
    one = return mempty
    times = liftM2 mappend
instance (Monoid w, Monoid m) => Multiplicative (LWriter.Writer w m) where
    one = return mempty
    times = liftM2 mappend
instance (Monad m, Monoid n) => Multiplicative (ContT r m n) where
    one = return mempty 
    times = liftM2 mappend
instance (Monad m, Monoid w, Monoid n) => Multiplicative (SRWS.RWST r w s m n) where 
    one = return mempty 
    times = liftM2 mappend
instance (Monad m, Monoid w, Monoid n) => Multiplicative (LRWS.RWST r w s m n) where 
    one = return mempty 
    times = liftM2 mappend
instance (Monad m, Monoid n) => Multiplicative (SState.StateT s m n) where
    one = return mempty
    times = liftM2 mappend
instance (Monad m, Monoid n) => Multiplicative (LState.StateT s m n) where
    one = return mempty
    times = liftM2 mappend
instance (Monad m, Monoid n) => Multiplicative (ReaderT e m n) where
    one = return mempty
    times = liftM2 mappend
instance (Monad m, Monoid w, Monoid n) => Multiplicative (SWriter.WriterT w m n) where
    one = return mempty 
    times = liftM2 mappend
instance (Monad m, Monoid w, Monoid n) => Multiplicative (LWriter.WriterT w m n) where
    one = return mempty 
    times = liftM2 mappend
#endif

#ifdef M_STM
instance Monoid n => Multiplicative (STM n) where
    one = return mempty
    times = liftM2 mappend
#endif

#ifdef M_PARSEC
instance (Stream s m t, Monoid n) => Multiplicative (ParsecT s u m n) where
    one = return mempty
    times = liftM2 mappend
#endif

#ifdef X_OverloadedStrings 
instance Multiplicative m => Multiplicative (FromString m) where
    one = FromString one
    FromString a `times` FromString b = FromString (a `times` b)
#endif
