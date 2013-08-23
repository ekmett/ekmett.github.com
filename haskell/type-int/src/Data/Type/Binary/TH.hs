{-# OPTIONS -fth #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Type.Binary.TH
-- Copyright   :  (C) 2006 Edward Kmett
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (Template Haskell)
--
-- Provides a simple way to construct type level binaries.
-- $(binaryE 24) returns an undefined value with the same type as the 
-- Type.Binary with value 24.
-----------------------------------------------------------------------------
module Data.Type.Binary.TH ( binaryE, binaryT ) where

import Data.Type.Binary.Internals
import Language.Haskell.TH

f = conT $ mkName "F"
t = conT $ mkName "T"
o = conT $ mkName "O"
i = conT $ mkName "I"

-- | $(binaryT n) returns the appropriate TBinary instance
binaryT :: Integral a => a -> TypeQ
binaryT n = case n of
    0  -> f
    -1 -> t
    n  -> appT (if (n `mod` 2) == 0 then o else i) $ binaryT $ n `div` 2

-- | $(binaryE n) returns an undefined value of the appropriate TBinary instance
binaryE :: Integral a => a -> ExpQ
binaryE n = sigE (varE $ mkName "undefined") $ binaryT n
