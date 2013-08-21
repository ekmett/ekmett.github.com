{-# LANGUAGE KindSignatures, RankNTypes, ScopedTypeVariables, EmptyDataDecls, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

----------------------------------------------------------------------------
-- |
-- Module     : Data.Reflection
-- Copyright  : 2009 Edward Kmett, 2004 Oleg Kiselyov and Chung-chieh Shan
-- License    : BSD3
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (scoped types, MPTCs, rank-n, FFI, kinds)
--
-- Implementation of the Functional Pearl: Implicit Configurations paper by
-- Oleg Kiselyov and Chung-chieh Shan.
--
-- <http://www.cs.rutgers.edu/~ccshan/prepose/prepose.pdf>
--
-- Packaged and updated to work with the current implementation of scoped type 
-- variables by Edward Kmett.
--
-------------------------------------------------------------------------------

module Data.Reflection 
    ( 
    -- * Reflect Integrals
      ReflectedNum
    , reflectNum
    , reifyIntegral
    -- * Reflect Lists of Integrals
    , ReflectedNums
    , reifyIntegrals
    -- * Reflect Storables
    , ReflectedStorable
    , reflectStorable
    , reifyStorable
    -- * Reflect Anything
    , Reflects
    , reflect
    , reify
    ) where

import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import System.IO.Unsafe

data Zero
data Twice s
data Succ s
data Pred s

class ReflectedNum s where
    reflectNum :: Num a => s -> a

instance ReflectedNum Zero where
    reflectNum _ = 0

instance ReflectedNum s => ReflectedNum (Twice s) where
    reflectNum _ = reflectNum (undefined :: s) * 2

instance ReflectedNum s => ReflectedNum (Succ s) where
    reflectNum _ = reflectNum (undefined :: s) + 1

instance ReflectedNum s => ReflectedNum (Pred s) where
    reflectNum _ = reflectNum (undefined :: s) - 1

reifyIntegral :: Integral a => a -> (forall s. ReflectedNum s => s -> w) -> w
reifyIntegral i k = case quotRem i 2 of
    (0, 0) -> k (undefined :: Zero)
    (j, 0) -> reifyIntegral j (\(_ :: s) -> k (undefined :: Twice s))
    (j, 1) -> reifyIntegral j (\(_ :: s) -> k (undefined :: Succ (Twice s)))
    (j,-1) -> reifyIntegral j (\(_ :: s) -> k (undefined :: Pred (Twice s)))
    _      -> undefined

data Nil
data Cons s ss

class ReflectedNums ss where
    reflectNums :: Num a => ss -> [a]

instance ReflectedNums Nil where
    reflectNums _ = []

instance (ReflectedNum s, ReflectedNums ss) => ReflectedNums (Cons s ss) where
    reflectNums _ = reflectNum (undefined :: s) : reflectNums (undefined :: ss)

reifyIntegrals :: Integral a => [a] -> (forall ss. ReflectedNums ss => ss -> w) -> w
reifyIntegrals [] k = k (undefined :: Nil)
reifyIntegrals (i:ii) k = 
    reifyIntegral i (\(_ :: s) -> 
    reifyIntegrals ii (\(_ :: ss) -> 
    k (undefined :: Cons s ss)))

data Store s a 

class ReflectedStorable s where
    reflectStorable :: Storable a => s a -> a

instance ReflectedNums s => ReflectedStorable (Store s) where
    {-# NOINLINE reflectStorable #-}
    reflectStorable _ = unsafePerformIO . alloca $ \p -> do 
            pokeArray (castPtr p) bytes
            peek p 
        where 
            bytes = reflectNums (undefined :: s) :: [CChar]

reifyStorable :: Storable a => a -> (forall s. ReflectedStorable s => s a -> w) -> w
reifyStorable a k = reifyIntegrals (bytes :: [CChar]) (\(_ :: s) -> k (undefined :: Store s a))
  where
    bytes = unsafePerformIO $ with a (peekArray (sizeOf a) . castPtr) 
{-# NOINLINE reifyStorable #-}

class Reflects s a | s -> a where 
    reflect :: s -> a

data Stable (s :: * -> *) a

{-
instance ReflectedStorable s => Reflects (Stable s a) a where
    reflect = unsafePerformIO . fmap const . deRefStablePtr $ reflectStorable (undefined :: s p) 

reify :: a -> (forall s. Reflects s a => s -> w) -> w
reify (a :: a) k = unsafePerformIO $ do
        p <- newStablePtr a
        reifyStorable p (\(_ :: s (StablePtr a)) -> return (k (undefined :: Stable s a)))
-}

instance ReflectedStorable s => Reflects (Stable s a) a where
    reflect = unsafePerformIO $ do
            a <- deRefStablePtr p
            freeStablePtr p
            return (const a)
        where  
            p = reflectStorable (undefined :: s p)
    {-# NOINLINE reflect #-}

reify :: a -> (forall s. (s `Reflects` a) => s -> w) -> w
reify (a :: a) k = unsafePerformIO $ do
        p <- newStablePtr a
        reifyStorable p (\(_ :: s (StablePtr a)) -> 
                let k' s = (reflect :: Stable s a -> a) `seq` return (k s) 
                in k' (undefined :: Stable s a))
{-# NOINLINE reify #-}
