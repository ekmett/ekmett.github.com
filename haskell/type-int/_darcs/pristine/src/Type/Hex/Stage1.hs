{-# GHC_OPTIONS -fglasgow-exts #-}
{-# GHC_OPTIONS -fth #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Type.Hex.Stage1
-- Copyright   :  (C) 2006 Edward Kmett
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (MPTC, FD, TH, undecidable instances, missing constructors)
--
-- Stage1: Lay the ground work for all sorts of template haskell hackery 
-- in the later stages. Only a handful of class specifications in this file
-- are for later public consumption, and none of those are fleshed out here.
--
-- This multiple-stage implementation is necessitated by the way Template 
-- Haskell is implemented in GHC.
----------------------------------------------------------------------------
module Type.Hex.Stage1 where

import Type.Boolean
import Type.Sign
import Control.Monad
import Language.Haskell.TH

t = conT $ mkName "T"
f = conT $ mkName "F"

hex = "0123456789ABCDEF"

xn = map (\x -> mkName $ "D"++return x) hex 
hn = map (\x -> mkName $ "H"++return x) hex 

x = map conT xn
h = map conT hn
xh = zip x h

x0 = tail x
h0 = tail h
xh0 = tail xh

xF = init x
hF = init h
xhF = zip xF hF

x0F = tail xF

a = mkName "a"; va = varT a
b = mkName "b"; vb = varT b
c = mkName "c"; vc = varT c
d = mkName "d"; vd = varT d

mkXT :: Name -> DecQ
mkXT n = return $ DataD [] n [a] [] []

mkHT :: Name -> DecQ
mkHT n = return $ DataD [] n [] [] []

-- imports
tnot = conT $ mkName "TNot"
positive = conT $ mkName "Positive"
negative = conT $ mkName "Negative"
signzero = conT $ mkName "SignZero"

-- to be fleshed out when available
class LSN a d a' | a -> d a', d a' -> a
lsn = conT $ mkName "LSN"
class Trichotomy n s | n -> s
trichotomy = conT $ mkName "Trichotomy"
class TEven a b | a -> b
teven = conT $ mkName "TEven"
class TSucc n m | n -> m, m -> n
tsucc = conT $ mkName "TSucc"
class TAddC' a b c d | a b c -> d
taddc' = conT $ mkName "TAddC'"
class TNF' a b c | a -> b c
tnf' = conT $ mkName "TNF'"
class THex a where fromTHex :: Integral b => a -> b
thex = conT $ mkName "THex"
class SHR1 a b c | a b -> c
shr1 = conT $ mkName "SHR1"


wrapMI list f = (flip mapM) list $ \v -> do
	i <- f v
	return $ InstanceD [] i []

concatMapM f = liftM concat . mapM f
