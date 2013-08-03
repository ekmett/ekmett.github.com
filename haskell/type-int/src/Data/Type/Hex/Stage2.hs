{-# GHC_OPTIONS -fglasgow-exts #-}
{-# GHC_OPTIONS -fth #-}
{-# GHC_OPTIONS -fallow-undecidable-instances #-}
{-# GHC_OPTIONS -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Type.Hex.Stage2
-- Copyright   :  (C) 2006 Edward Kmett
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (MPTC, FD, TH, undecidable instances, missing constructors)
--
-- Stage2: Create the D0 and H0 data elements that will be used later.
-- Define utility classes, some classes defined here can not be fleshed 
-- out until Stage3.
--
-- This multiple-stage implementation is necessitated by the way Template 
-- Haskell is implemented in GHC.
----------------------------------------------------------------------------
module Data.Type.Hex.Stage2 where

import Data.Type.Boolean
import Control.Monad
import Data.Type.Hex.Stage1
import Language.Haskell.TH

#ifndef __HADDOCK__
$(mapM mkXT xn)
$(mapM mkHT hn)
#endif

-- | extract the least signficant nybble from a hex number
instance LSN F H0 F
instance LSN T HF T
#ifndef __HADDOCK__
$( wrapMI xhF $ \(x,h) -> lsn `appT` (appT x t) `appT` h `appT` t)
$( wrapMI xh0 $ \(x,h) -> lsn `appT` (appT x f) `appT` h `appT` f)
$( wrapMI (liftM2 (,) xh x) $ \((x,h),x') -> let axa = appT x' va in lsn `appT` (appT x axa) `appT` h `appT` axa)
#endif
tLSN :: LSN a d a' => a -> (d,a'); tLSN = undefined
tNSL :: LSN a d a' => a' -> d -> a; tNSL = undefined

class LSN (D0 a) H0 a => Ext0 a
instance LSN (D0 a) H0 a => Ext0 a
class LSN (DF a) HF a => ExtF a
instance LSN (DF a) HF a => ExtF a

instance THex F                              where fromTHex _ = fromInteger 0
instance THex T                              where fromTHex _ = fromInteger (-1)
instance (THex a, Ext0 a) => THex (D0 a)     where fromTHex _ = let x = fromTHex (undefined::a) in 16*x
instance THex a => THex (D1 a) where fromTHex _ = let x = fromTHex (undefined::a) in 16*x+1
instance THex a => THex (D2 a) where fromTHex _ = let x = fromTHex (undefined::a) in 16*x+2
instance THex a => THex (D3 a) where fromTHex _ = let x = fromTHex (undefined::a) in 16*x+3
instance THex a => THex (D4 a) where fromTHex _ = let x = fromTHex (undefined::a) in 16*x+4
instance THex a => THex (D5 a) where fromTHex _ = let x = fromTHex (undefined::a) in 16*x+5
instance THex a => THex (D6 a) where fromTHex _ = let x = fromTHex (undefined::a) in 16*x+6
instance THex a => THex (D7 a) where fromTHex _ = let x = fromTHex (undefined::a) in 16*x+7
instance THex a => THex (D8 a) where fromTHex _ = let x = fromTHex (undefined::a) in 16*x+8
instance THex a => THex (D9 a) where fromTHex _ = let x = fromTHex (undefined::a) in 16*x+9
instance THex a => THex (DA a) where fromTHex _ = let x = fromTHex (undefined::a) in 16*x+10
instance THex a => THex (DB a) where fromTHex _ = let x = fromTHex (undefined::a) in 16*x+11
instance THex a => THex (DC a) where fromTHex _ = let x = fromTHex (undefined::a) in 16*x+12
instance THex a => THex (DD a) where fromTHex _ = let x = fromTHex (undefined::a) in 16*x+13
instance THex a => THex (DE a) where fromTHex _ = let x = fromTHex (undefined::a) in 16*x+14
instance (THex a, ExtF a) => THex (DF a)     where fromTHex _ = let x = fromTHex(undefined::a) in 16*x+15


instance TEven F T
instance TEven T F 
#ifndef __HADDOCK__
$( wrapMI (zip x [0..15]) $ \(x,n) -> teven `appT` (appT x va) `appT` (if n `mod` 2 == 0 then t else f) )

$( mapM (\(x,y) -> do
	c <- tnot `appT` va `appT` vb
	i <- tnot `appT` (appT x va) `appT` (appT y vb)
	return $ InstanceD [c] i []) $ zip x (reverse x))
#endif

class TOdd a b' 
instance (TEven a b, TNot b b') => TOdd a b'

instance TNF' F F F
instance TNF' T T F
instance TNF' (D0 F) F F
instance TNF' (DF T) T F
instance (TNF' (DF a) c b, TIf b (DF c) T d) => TNF' (DF (DF a)) d b
instance (TNF' (D0 a) c b, TIf b (D0 c) F d) => TNF' (D0 (D0 a)) d b
#ifndef __HADDOCK__
$( wrapMI x0 $ \x -> tnf' `appT` (appT x f) `appT` (appT x f) `appT` t)
$( wrapMI xF $ \x -> tnf' `appT` (appT x t) `appT` (appT x t) `appT` t)
$( let xn = zip x [0..15]
       xn2 = liftM2 (,) xn xn
       list' = (flip filter) xn2 $ \((_,n),(_,m)) -> if n == 0 then m /= 0 else (if n == 15 then m /= 15 else True)
       list = map (\((x,_),(y,_)) -> (x,y)) list'
   in (flip mapM) list $ \(x,y) -> do
	pre <- tnf' `appT` (appT x va) `appT` vc `appT` vb
	post <- tnf' `appT` (appT y (appT x va)) `appT` (appT y vc) `appT` vb
	return $ InstanceD [pre] post [])
#endif
