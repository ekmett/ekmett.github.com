-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Type.Sign
-- Copyright   :  (C) 2006 Edward Kmett
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (MPTC, FD, and missing constructors)
--
-- Used to express trichotomy at the type level.
-----------------------------------------------------------------------------
module Data.Type.Sign (TSign, Negative, Positive, SignZero) where

import Data.Type.Boolean
import Data.Type.Ord

data Closure
class Closed a | -> a
instance Closed Closure

data Negative
data SignZero
data Positive

class TCSign c a | a -> c
instance TCSign Closure Negative
instance TCSign Closure Positive
instance TCSign Closure SignZero

class TCSign Closure s => TSign s
instance TSign Negative
instance TSign SignZero
instance TSign Positive

instance TEq Negative Negative T
instance TEq Negative SignZero F
instance TEq Negative Positive F
instance TEq SignZero Negative F
instance TEq SignZero SignZero T
instance TEq SignZero Positive F
instance TEq Positive Negative F
instance TEq Positive SignZero F
instance TEq Positive Positive T
