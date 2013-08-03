{-# GHC_OPTIONS -fglasgow-exts #-}
{-# GHC_OPTIONS -fth #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Type.Hex.Stage2
-- Copyright   :  (C) 2006 Edward Kmett
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (MPTC, FD, TH, undecidable instances, missing constructors)
--
-- Stage3: Define everything else. The juicier bits are then exposed via
-- Type.Hex
-----------------------------------------------------------------------------

module Type.Hex.Stage3 where

import Type.Boolean
import Control.Monad
import Type.Hex.Stage1
import Type.Hex.Stage2
import Type.Sign
import Type.Ord
import Data.Bits
import Language.Haskell.TH
import qualified Type.Binary as B

instance TSucc T F
instance TSucc F (D1 F)
instance TSucc (DE T) T
instance (TSucc n m, ExtF n, Ext0 m) => TSucc (DF n) (D0 m)
#ifndef __HADDOCK__
$( wrapMI (zip (init x0) (tail x0)) $ \(x1,x2) -> tsucc `appT` (appT x1 f) `appT` (appT x2 f))
$( wrapMI (zip (init xF) (tail xF)) $ \(x1,x2) -> tsucc `appT` (appT x1 t) `appT` (appT x2 t))
$( wrapMI (liftM2 (,) (zip xF x0) x) $ \((xn,xm),x) -> let b = appT x va in 
	tsucc `appT` (appT xn b) `appT` (appT xm b))
#endif
tSucc :: TSucc n m => n -> m; tSucc = undefined
tPred :: TSucc n m => m -> n; tPred = undefined

class TNeg a b | a -> b, b -> a
instance (TNot a b, TSucc b c) => TNeg a c
tNeg :: TNeg a b => a -> b; tNeg = undefined

instance Trichotomy T Negative
instance Trichotomy F SignZero
#ifndef __HADDOCK__
$( wrapMI x0 $ \x -> trichotomy `appT` (appT x f) `appT` positive )
$( wrapMI xF $ \x -> trichotomy `appT` (appT x t) `appT` negative )
$( let eda = [(appT extf va,appT tf va),(appT ext0 va, appT tf va)]
       ext0 = conT $ mkName "Ext0"
       extf = conT $ mkName "ExtF"
       tf = conT $ mkName "DF"
       t0 = conT $ mkName "D0" in
   (flip mapM) (zip x eda) $ \(x,(ea,da)) -> do
	tc <- trichotomy `appT` va `appT` vb
	i <- trichotomy `appT` (appT x da) `appT` vb
	eq <- ea
	return $ InstanceD [tc,eq] i [])
#endif

class TIsPositive n b | n -> b
instance (Trichotomy n s, TEq s Positive b) => TIsPositive n b
tIsPositive :: TIsPositive n b => n -> b; tIsPositive = undefined

class TIsNegative n b | n -> b
instance (Trichotomy n s, TEq s Negative b) => TIsNegative n b
tIsNegative :: TIsNegative n b => n -> b; tIsNegative = undefined

class TIsZero n b | n -> b
instance (Trichotomy n s, TEq s SignZero b) => TIsZero n b
tIsZero :: TIsZero n b => n -> b; tIsZero = undefined

instance TAddC' F F F F
instance TAddC' T F T F
instance TAddC' F T F T
instance TAddC' T T T T
instance TAddC' T F F T
instance TAddC' F T T F
instance TAddC' F F T (D1 F)
instance TAddC' T T F (DE T)
instance TSucc a b => TAddC' F (DF a) T (D0 b)
instance TSucc b a => TAddC' T (D0 a) F (DF b)
instance TSucc a b => TAddC' (DF a) F T (D0 b)
instance TSucc b a => TAddC' (D0 a) T F (DF b)
#ifndef __HADDOCK__
$( wrapMI (liftM2 (,) [t,f] x) $ \(tf,dx) -> let dxa = appT dx va in
	taddc' `appT` tf `appT` dxa `appT` tf `appT` dxa)
$( wrapMI (liftM2 (,) [t,f] x) $ \(tf,dx) -> let dxa = appT dx va in
	taddc' `appT` dxa `appT` tf `appT` tf `appT` dxa)
$( wrapMI (zip xF x0) $ \(dn,dm) -> taddc' `appT` f `appT` (appT dn va) `appT` t `appT` (appT dm va))
$( wrapMI (zip xF x0) $ \(dn,dm) -> taddc' `appT` t `appT` (appT dm va) `appT` f `appT` (appT dn va))
$( wrapMI (zip xF x0) $ \(dn,dm) -> taddc' `appT` (appT dn va) `appT` f `appT` t `appT` (appT dm va))
$( wrapMI (zip xF x0) $ \(dn,dm) -> taddc' `appT` (appT dm va) `appT` t `appT` f `appT` (appT dn va))
$( (flip mapM) (liftM3 (,,) (zip x [0..15]) (zip x [0..15]) [(f,0),(t,1)]) $ \((x0,n0),(x1,n1),(b,c)) -> do
	let total = n0+n1+c
	    pcarry = if total > 15 then t else f
	    x2 = x !! (total `mod` 16)
	pre <- taddc' `appT` va `appT` vb `appT` pcarry `appT` vc 
	post <- taddc' `appT` (appT x0 va) `appT` (appT x1 vb) `appT` b `appT` (appT x2 vc)
	return $ InstanceD [pre] post [])
#endif
tAddC' :: TAddC' a b c d => a -> b -> c -> d; tAddC' = undefined
tAddF' :: TAddC' a b F d => a -> b -> d; tAddF' = undefined

class TNF a b | a -> b
instance TNF' a b c => TNF a b
tNF   :: TNF a b => a -> b;     tNF = undefined

class TAdd' a b c | a b -> c
instance (TAddC' a b F d, TNF d d') => TAdd' a b d'
tAdd' :: (TAdd' a b c ) => a -> b -> c; tAdd' = undefined

class TSub' a b c | a b -> c
instance (TNeg b b', TAdd' a b' c) => TSub' a b c
tSub' :: TSub' a b c => a -> b -> c; tSub' = undefined

class TAdd a b c | a b -> c, a c -> b, b c -> a
instance (TAdd' a b c, TNeg b b', TAdd' c b' a, TNeg a a', TAdd' c a' b) => TAdd a b c
tAdd :: (TAdd a b c) => a -> b -> c;tAdd = undefined
tSub :: (TAdd a b c) => c -> a -> b;tSub = undefined

-- | $(hexT n) returns the appropriate THex instance
hexT :: Integral a => a -> TypeQ
hexT n = case n of
    0  -> f
    -1 -> t
    n  -> appT (x !! mod (fromIntegral n) 16) $ hexT $ n `div` 16

-- | $(hexE n) returns an undefined value of the appropriate THex instance
hexE :: Integral a => a -> ExpQ
hexE n = sigE (varE $ mkName "undefined") $ hexT n

instance THex (D0 a) => Show (D0 a) where show n = "$(hexE "++ (show $ fromTHex n)++")"
instance THex (D1 a) => Show (D1 a) where show n = "$(hexE "++ (show $ fromTHex n)++")"
instance THex (D2 a) => Show (D2 a) where show n = "$(hexE "++ (show $ fromTHex n)++")"
instance THex (D3 a) => Show (D3 a) where show n = "$(hexE "++ (show $ fromTHex n)++")"
instance THex (D4 a) => Show (D4 a) where show n = "$(hexE "++ (show $ fromTHex n)++")"
instance THex (D5 a) => Show (D5 a) where show n = "$(hexE "++ (show $ fromTHex n)++")"
instance THex (D6 a) => Show (D6 a) where show n = "$(hexE "++ (show $ fromTHex n)++")"
instance THex (D7 a) => Show (D7 a) where show n = "$(hexE "++ (show $ fromTHex n)++")"
instance THex (D8 a) => Show (D8 a) where show n = "$(hexE "++ (show $ fromTHex n)++")"
instance THex (D9 a) => Show (D9 a) where show n = "$(hexE "++ (show $ fromTHex n)++")"
instance THex (DA a) => Show (DA a) where show n = "$(hexE "++ (show $ fromTHex n)++")"
instance THex (DB a) => Show (DB a) where show n = "$(hexE "++ (show $ fromTHex n)++")"
instance THex (DC a) => Show (DC a) where show n = "$(hexE "++ (show $ fromTHex n)++")"
instance THex (DD a) => Show (DD a) where show n = "$(hexE "++ (show $ fromTHex n)++")"
instance THex (DE a) => Show (DE a) where show n = "$(hexE "++ (show $ fromTHex n)++")"
instance THex (DF a) => Show (DF a) where show n = "$(hexE "++ (show $ fromTHex n)++")"

instance SHR1 H0 F F
instance SHR1 H1 F (D1 F)
instance SHR1 H0 T (DE T)
instance SHR1 H1 T (DE T)
#ifndef __HADDOCK__
$( wrapMI (liftM3 (,,) (zip x [0..15]) (zip h [0..1]) (zip [t,f] [15,0])) $ \((d,dn),(c,cn),(tf,tfn)) ->
        let dlsn = x !! ((dn*2+cn) `mod` 16)
            dmsn = x !! (((dn `div` 8) + tfn*2) `mod` 16)
            nmsn = dn `div` 8
            dcase = if ((tfn .&. 1) `xor` nmsn) /= 0 then appT dmsn tf else tf
        in shr1 `appT` c `appT` (appT d tf) `appT` (appT dlsn dcase))
$( 
        (flip mapM) (liftM3 (,,) (zip x [0..15]) (zip h [0..1]) (zip x [0..15])) $ \((dm,dmi),(c,cn),(dn,dni)) -> do
        let msb_m = dmi `div` 8
            dn' = x !! ((msb_m + (dni*2)) `mod` 16)
            dm' = x !! ((cn + (dmi*2)) `mod` 16)
            pre_c = h !! msb_m
            dna = appT dn va
            dn'b = appT dn' vb
        pre <- shr1 `appT` pre_c `appT` dna `appT` dn'b
        post <- shr1 `appT` c `appT` (appT dm dna) `appT` (appT dm' dn'b)
        return $ InstanceD [pre] post [])
#endif

-- | A simple peasant multiplier. TODO: exploit 2s complement and reverse the worst cases
class TMul a b c | a b -> c
instance TMul a F F 
instance TNeg a b => TMul a T b
instance TMul (D0 a1) b c => TMul a1 (D0 b) c
instance ( TMul (D0 a1) b c
	 , TAdd' a1 c d) => TMul a1 (D1 b) d
instance ( TMul (D0 a1) b c
	 , SHR1 H0 a1 a2
	 , TAdd' a2 c d) => TMul a1 (D2 b) d
instance ( TMul (D0 a1) b c
	 , SHR1 H0 a1 a2
	 , TAdd' a1 a2 a3
	 , TAdd' a3 c d) => TMul a1 (D3 b) d
instance ( TMul (D0 a1) b c
	 , SHR1 H0 a1 a2
	 , SHR1 H0 a2 a4
	 , TAdd' a4 c d) => TMul a1 (D4 b) d
instance ( TMul (D0 a1) b c
	 , SHR1 H0 a1 a2
	 , SHR1 H0 a2 a4
	 , TAdd' a1 a4 a5
	 , TAdd' a5 c d) => TMul a1 (D5 b) d
instance ( TMul (D0 a1) b c
	 , SHR1 H0 a1 a2
	 , SHR1 H0 a2 a4
	 , TAdd' a2 a4 a6
	 , TAdd' a6 c d) => TMul a1 (D6 b) d
instance ( TMul (D0 a1) b c
	 , SHR1 H0 a1 a2
	 , SHR1 H0 a2 a4
	 , TAdd' a2 a4 a6
	 , TAdd' a1 a6 a7
	 , TAdd' a7 c d) => TMul a1 (D7 b) d
instance ( TMul (D0 a1) b c
	 , SHR1 H0 a1 a2
	 , SHR1 H0 a2 a4
	 , SHR1 H0 a4 a8
	 , TAdd' a8 c d) => TMul a1 (D8 b) d
instance ( TMul (D0 a1) b c
	 , SHR1 H0 a1 a2
	 , SHR1 H0 a2 a4
	 , SHR1 H0 a4 a8
	 , TAdd' a1 a8 a9
	 , TAdd' a9 c d) => TMul a1 (D9 b) d
instance ( TMul (D0 a1) b c
	 , SHR1 H0 a1 a2
	 , SHR1 H0 a2 a4
	 , SHR1 H0 a4 a8
	 , TAdd' a2 a8 aA
	 , TAdd' aA c d) => TMul a1 (DA b) d
instance ( TMul (D0 a1) b c
	 , SHR1 H0 a1 a2
	 , SHR1 H0 a2 a4
	 , SHR1 H0 a4 a8
	 , TAdd' a2 a8 a0 
	 , TAdd' a1 a0 aB
	 , TAdd' aB c d) => TMul a1 (DB b) d
instance ( TMul (D0 a1) b c
	 , SHR1 H0 a1 a2
	 , SHR1 H0 a2 a4
	 , SHR1 H0 a4 a8
	 , TAdd' a4 a8 aC
	 , TAdd' aC c d) => TMul a1 (DC b) d
instance ( TMul (D0 a1) b c
	 , SHR1 H0 a1 a2
	 , SHR1 H0 a2 a4
	 , SHR1 H0 a4 a8
	 , TAdd' a4 a8 aC
	 , TAdd' a1 aC aD
	 , TAdd' aD c d) => TMul a1 (DD b) d
instance ( TMul (D0 a1) b c
	 , SHR1 H0 a1 a2
	 , SHR1 H0 a2 a4
	 , SHR1 H0 a4 a8
	 , TAdd' a4 a8 aC
	 , TAdd' a2 aC aE
	 , TAdd' aE c d) => TMul a1 (DE b) d
instance ( TMul (D0 a1) b c
	 , SHR1 H0 a1 a2
	 , SHR1 H0 a2 a4
	 , SHR1 H0 a4 a8
	 , TAdd' a4 a8 aC
	 , TAdd' a2 aC aE
	 , TAdd' a1 aE aF
	 , TAdd' aF c d) => TMul a1 (DF b) d

tMul :: TMul a b c => a -> b -> c
tMul = undefined

class THex2Binary' a b | a -> b, b -> a
instance THex2Binary' F F 
instance THex2Binary' T T
instance THex2Binary' a b => THex2Binary' (D0 a) (B.O(B.O(B.O(B.O b))))
instance THex2Binary' a b => THex2Binary' (D1 a) (B.I(B.O(B.O(B.O b))))
instance THex2Binary' a b => THex2Binary' (D2 a) (B.O(B.I(B.O(B.O b))))
instance THex2Binary' a b => THex2Binary' (D3 a) (B.I(B.I(B.O(B.O b))))
instance THex2Binary' a b => THex2Binary' (D4 a) (B.O(B.O(B.I(B.O b))))
instance THex2Binary' a b => THex2Binary' (D5 a) (B.I(B.O(B.I(B.O b))))
instance THex2Binary' a b => THex2Binary' (D6 a) (B.O(B.I(B.I(B.O b))))
instance THex2Binary' a b => THex2Binary' (D7 a) (B.I(B.I(B.I(B.O b))))
instance THex2Binary' a b => THex2Binary' (D8 a) (B.O(B.O(B.O(B.I b))))
instance THex2Binary' a b => THex2Binary' (D9 a) (B.I(B.O(B.O(B.I b))))
instance THex2Binary' a b => THex2Binary' (DA a) (B.O(B.I(B.O(B.I b))))
instance THex2Binary' a b => THex2Binary' (DB a) (B.I(B.I(B.O(B.I b))))
instance THex2Binary' a b => THex2Binary' (DC a) (B.O(B.O(B.I(B.I b))))
instance THex2Binary' a b => THex2Binary' (DD a) (B.I(B.O(B.I(B.I b))))
instance THex2Binary' a b => THex2Binary' (DE a) (B.O(B.I(B.I(B.I b))))
instance THex2Binary' a b => THex2Binary' (DF a) (B.I(B.I(B.I(B.I b))))

class THex2Binary a b | a -> b
instance (THex2Binary' a b, B.TNF b b') => THex2Binary a b'
tHex2Binary :: THex2Binary a b => a -> b; tHex2Binary = undefined

class TBinary2Hex a b | a -> b
instance (THex2Binary' a b, TNF a a') => TBinary2Hex b a'
tBinary2Hex :: TBinary2Hex a b => a -> b; tBinary2Hex = undefined

class THexBinary a b | a -> b, b -> a
instance (THex2Binary a b, TBinary2Hex b a) => THexBinary a b

-- | peasant exponentiator with explicit binary exponent
class TPow' a b c | a b -> c
instance TPow' a F (D1 F)
instance (TPow' a k c, TMul c c d) => TPow' a (B.O k) d
instance (TPow' a k c, TMul c c d, TMul a d e) => TPow' a (B.I k) e

-- | peasant exponentiator
class TPow a b c | a b -> c
instance (THex2Binary b b', TPow' a b' c) => TPow a b c
tPow :: TPow a b c => a -> b -> c
tPow = undefined
