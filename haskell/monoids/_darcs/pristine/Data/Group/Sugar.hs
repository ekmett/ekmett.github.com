-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Group.Sugar
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Syntactic sugar for working with groups that conflicts with names from the "Prelude".
--
-- > import Prelude hiding ((-), (+), (*), (/), (^), (^^), negate, subtract, recip)
-- > import Data.Group.Sugar
--
-----------------------------------------------------------------------------

module Data.Group.Sugar 
    ( module Data.Monoid.Sugar
    , module Data.Group
    , (-)
    , negate
    , subtract
    , (/)
    , (.\.)
    , (^^)
    , recip
    ) where

import Data.Monoid.Sugar
import Data.Group.Combinators as Group
import Data.Group
import Prelude hiding ((-), (+), (*), (/), (^^), negate, subtract, recip)

infixl 8 /
infixr 8 .\.
infixl 7 -

(-) :: Group g => g -> g -> g
(-) = minus

negate :: Group g => g -> g
negate = gnegate

subtract :: Group g => g -> g -> g
subtract = gsubtract

(/) :: MultiplicativeGroup g => g -> g -> g
(/) = over

(.\.) :: MultiplicativeGroup g => g -> g -> g
(.\.) = under

recip :: MultiplicativeGroup g => g -> g
recip = grecip

(^^) :: MultiplicativeGroup g => g -> Integer -> g
g ^^ n = getLog (Group.replicate (Log g) n)
