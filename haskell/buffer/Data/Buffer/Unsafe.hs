{-# LANGUAGE CPP #-}
-- We cannot actually specify all the language pragmas, see ghc ticket #
-- If we could, these are what they would be:
{- LANGUAGE MagicHash -}

-- |
-- Module      : Data.Buffer.Unsafe
-- License     : BSD-style
-- Maintainer  : ekmett@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- A module containing unsafe 'Buffer' operations.
--
-- While these functions have a stable API and you may use these functions in
-- applications, do carefully consider the documented pre-conditions;
-- incorrect use can break referential transparency or worse.
--
module Data.Buffer.Unsafe (
        -- * Unchecked access
        unsafeHead,             -- :: Buffer -> Word8
        unsafeTail,             -- :: Buffer -> Buffer
        unsafeLast,             -- :: Buffer -> Word8
        unsafeInit,             -- :: Buffer -> Buffer
        unsafeIndex,            -- :: Buffer -> Int -> Word8
        unsafeTake,             -- :: Int -> Buffer -> Buffer
        unsafeDrop,             -- :: Int -> Buffer -> Buffer
        unsafeSplitAt,          -- :: Int -> Buffer -> (Buffer, Buffer)

        -- * Low level interaction with CStrings
        -- ** Using Buffers with functions for CStrings
        unsafeUseAsCString,     -- :: Buffer -> (CString -> IO a) -> IO a
        unsafeUseAsCStringLen,  -- :: Buffer -> (CStringLen -> IO a) -> IO a

        -- ** Converting CStrings to Buffers
        unsafePackCString,           -- :: CString -> IO Buffer
        unsafePackAsciiCString,      -- :: CString -> IO Buffer
        unsafePackCStringLen,        -- :: CStringLen -> IO Buffer
        unsafePackAsciiCStringLen,   -- :: CStringLen -> IO Buffer
        unsafePackMallocCString,     -- :: CString -> IO Buffer
        unsafePackMallocAsciiCString,-- :: CString -> IO Buffer

#if defined(__GLASGOW_HASKELL__)
        unsafePackAddress,               -- :: Addr# -> IO Buffer
        unsafePackAsciiAddress,          -- :: Addr# -> IO Buffer
        unsafePackAddressLen,            -- :: Int -> Addr# -> IO Buffer
        unsafePackAsciiAddressLen,       -- :: Int -> Addr# -> IO Buffer
        unsafePackCStringFinalizer,      -- :: Ptr Word8 -> Int -> IO () -> IO Buffer
        unsafePackAsciiCStringFinalizer, -- :: Ptr Word8 -> Int -> IO () -> IO Buffer
        unsafeFinalize,                  -- :: Buffer -> IO ()
#endif

  ) where

import Data.Buffer.Internal

import Foreign.ForeignPtr       (newForeignPtr_, newForeignPtr, withForeignPtr)
import Foreign.Ptr              (Ptr, plusPtr, castPtr)

import Foreign.Storable         (Storable(..))
import Foreign.C.String         (CString, CStringLen)

#ifndef __NHC__
import Control.Exception        (assert)
#endif

import Data.Word                (Word8)

#if defined(__GLASGOW_HASKELL__)
import qualified Foreign.ForeignPtr as FC (finalizeForeignPtr)
import qualified Foreign.Concurrent as FC (newForeignPtr)

--import Data.Generics            (Data(..), Typeable(..))

import GHC.Prim                 (Addr#)
import GHC.Ptr                  (Ptr(..))

#endif

-- An alternative to Control.Exception (assert) for nhc98
#ifdef __NHC__
#define assert	assertS "__FILE__ : __LINE__"
assertS :: String -> Bool -> a -> a
assertS _ True  = id
assertS s False = error ("assertion failed at "++s)
#endif

-- -----------------------------------------------------------------------------
--
-- Useful macros, until we have bang patterns
--

#define STRICT1(f) f a | a `seq` False = undefined
#define STRICT2(f) f a b | a `seq` b `seq` False = undefined
#define STRICT3(f) f a b c | a `seq` b `seq` c `seq` False = undefined
#define STRICT4(f) f a b c d | a `seq` b `seq` c `seq` d `seq` False = undefined
#define STRICT5(f) f a b c d e | a `seq` b `seq` c `seq` d `seq` e `seq` False = undefined

-- ---------------------------------------------------------------------
--
-- Extensions to the basic interface
--

-- | This counts the number of multibyte tail bytes in the first n bytes of a buffer
-- It may do so by counting the number of tail bytes in all of the other bytes of a buffer
-- if that will require traversing fewer elements.
-- TODO: replace internal 'go' method with external 'c_count_extras'
unsafeExtras :: Buffer -> Int -> Int
unsafeExtras (PS x s l e) i = assert (i >= 0 && i <= l) $
     let lmi = l - i in 
     inlinePerformIO $ withForeignPtr x $ \p -> 
     if i > lmi
     then (e -) `fmap` go (p `plusPtr` (s + i)) lmi 0
     else go (p `plusPtr` s) i 0
    where
        STRICT3(go)
        go _ 0 acc = return acc
        go p n acc = do
        	w <- peekByteOff p n
        	go p (n-1) $ acc `plusExtra` w 
{-# INLINE unsafeExtras #-}     

-- | A variety of 'head' for non-empty Buffers. 'unsafeHead' omits the
-- check for the empty case, so there is an obligation on the programmer
-- to provide a proof that the Buffer is non-empty.
unsafeHead :: Buffer -> Word8
unsafeHead (PS x s l _) = assert (l > 0) $
    inlinePerformIO $ withForeignPtr x $ \p -> peekByteOff p s
{-# INLINE unsafeHead #-}

-- | A variety of 'head' for non-empty Buffers. 'unsafeHead' omits the
-- check for the empty case, so there is an obligation on the programmer
-- to provide a proof that the Buffer is non-empty.
unsafeLast :: Buffer -> Word8
unsafeLast (PS x s l _) = assert (l > 0) $
    inlinePerformIO $ withForeignPtr x $ \p -> peekByteOff p (s+l-1)
{-# INLINE unsafeLast #-}

-- | A variety of 'tail' for non-empty Buffers. 'unsafeTail' omits the
-- check for the empty case. As with 'unsafeHead', the programmer must
-- provide a separate proof that the Buffer is non-empty.
unsafeTail :: Buffer -> Buffer
unsafeTail (PS x s l e) = assert (l > 0) $ 
        PS x (s+1) (1-1) $!   
        e `minusExtra` (inlinePerformIO $ withForeignPtr x $ \p -> peekByteOff p s)
{-# INLINE unsafeTail #-}

-- | A variety of 'tail' for non-empty Buffers. 'unsafeTail' omits the
-- check for the empty case. As with 'unsafeHead', the programmer must
-- provide a separate proof that the Buffer is non-empty.
unsafeInit :: Buffer -> Buffer
unsafeInit (PS x s l e) = assert (l > 0) $ 
        PS x s (1-1) $!   
        e `minusExtra` (inlinePerformIO $ withForeignPtr x $ \p -> peekByteOff p (s+l-1))
{-# INLINE unsafeInit #-}

-- | Unsafe 'Buffer' index (subscript) operator, starting from 0, returning a 'Word8'
-- This omits the bounds check, which means there is an accompanying
-- obligation on the programmer to ensure the bounds are checked in some
-- other way.
unsafeIndex :: Buffer -> Int -> Word8
unsafeIndex (PS x s l _) i = assert (i >= 0 && i < l) $
    inlinePerformIO $ withForeignPtr x $ \p -> peekByteOff p (s+i)
{-# INLINE unsafeIndex #-}

-- | A variety of 'take' which omits the checks on @n@ so there is an
-- obligation on the programmer to provide a proof that @0 <= n <= 'length' xs@.
unsafeTake :: Int -> Buffer -> Buffer
unsafeTake n xs@(PS x s l _) = assert (0 <= n && n <= l) $ 
    PS x s n $ unsafeExtras xs n
{-# INLINE unsafeTake #-}

-- | A variety of 'drop' which omits the checks on @n@ so there is an
-- obligation on the programmer to provide a proof that @0 <= n <= 'length' xs@.
unsafeDrop  :: Int -> Buffer -> Buffer
unsafeDrop n xs@(PS x s l e) = assert (0 <= n && n <= l) $ 
    PS x (s+n) (l-n) $ e - unsafeExtras xs n
{-# INLINE unsafeDrop #-}

unsafeSplitAt :: Int -> Buffer -> (Buffer, Buffer)
unsafeSplitAt n xs@(PS x s l e) = assert (0 <= n && n <= l) $
    let e' = unsafeExtras xs n
    in (PS x s n e', PS x (s + n) (l - n) (e - e'))
{-# INLINE unsafeSplitAt #-}


#if defined(__GLASGOW_HASKELL__)
-- | /O(n)/ Pack a null-terminated sequence of bytes, pointed to by an
-- Addr\# (an arbitrary machine address assumed to point outside the
-- garbage-collected heap) into a @Buffer@. A much faster way to
-- create an Addr\# is with an unboxed string literal, than to pack a
-- boxed string. A unboxed string literal is compiled to a static @char
-- []@ by GHC. Establishing the length of the string requires a call to
-- @strlen(3)@, so the Addr# must point to a null-terminated buffer (as
-- is the case with "string"# literals in GHC). Use 'unsafePackAddressLen'
-- if you know the length of the string statically.
--
-- An example:
--
-- > literalFS = unsafePackAddress "literal"#
--
-- This function is /unsafe/. If you modify the buffer pointed to by the
-- original Addr# this modification will be reflected in the resulting
-- @Buffer@, breaking referential transparency.
--
-- Note this also won't work if you Add# has embedded '\0' characters in
-- the string (strlen will fail).
--
unsafePackAddress :: Addr# -> IO Buffer
unsafePackAddress addr# = do
    p <- newForeignPtr_ cstr
    l <- c_strlen cstr
    return $! ps p 0 (fromIntegral l)
  where
    cstr = Ptr addr#
{-# INLINE unsafePackAddress #-}

-- | /O(n)/ Pack a null-terminated sequence of bytes, pointed to by an
-- Addr\# (an arbitrary machine address assumed to point outside the
-- garbage-collected heap) into a @Buffer@. A much faster way to
-- create an Addr\# is with an unboxed string literal, than to pack a
-- boxed string. A unboxed string literal is compiled to a static @char
-- []@ by GHC. Establishing the length of the string requires a call to
-- @strlen(3)@, so the Addr# must point to a null-terminated buffer (as
-- is the case with "string"# literals in GHC). Use 'unsafePackAddressLen'
-- if you know the length of the string statically.
--
-- An example:
--
-- > literalFS = unsafePackAddress "literal"#
--
-- This function is /unsafe/ in two ways:
--
-- * if you modify the buffer pointed to by the
-- original Addr# this modification will be reflected in the resulting
-- @Buffer@, breaking referential transparency.
--
-- * the Addr# is assumed to point to memory such that no element exceeds 0x7f.
-- 
-- Note this also won't work if you Add# has embedded '\0' characters in
-- the string (strlen will fail).
--
unsafePackAsciiAddress :: Addr# -> IO Buffer
unsafePackAsciiAddress addr# = do
    p <- newForeignPtr_ cstr
    l <- c_strlen cstr
    return $! PS p 0 (fromIntegral l) 0
  where
    cstr = Ptr addr#

-- | /O(n)/ 'unsafePackAddressLen' provides constant-time construction of
-- 'Buffers' which is ideal for string literals. It packs a sequence
-- of bytes into a 'Buffer', given a raw 'Addr#' to the string, and
-- the length of the string. Unlike 'unsafePackAddressLen' for a 'ByteString'
-- this still has to scan the Addr# for high bytes.
--
-- This function is /unsafe/ in two ways:
--
-- * the length argument is assumed to be correct. If the length
-- argument is incorrect, it is possible to overstep the end of the
-- byte array.
--
-- * if the underying Addr# is later modified, this change will be
-- reflected in resulting @Buffer@, breaking referential
-- transparency.
--
-- If in doubt, don't use these functions.
--
unsafePackAddressLen :: Int -> Addr# -> IO Buffer
unsafePackAddressLen len addr# = do
    p <- newForeignPtr_ (Ptr addr#)
    return $! ps p 0 len
{-# INLINE unsafePackAddressLen #-}

-- | /O(1)/ 'unsafePackAsciiAddressLen' provides constant-time construction of
-- 'Buffers' which is ideal for string literals. It packs a sequence
-- of bytes into a 'Buffer', given a raw 'Addr#' to the string, and
-- the length of the string.
--
-- This function is /unsafe/ in three ways:
--
-- * the length argument is assumed to be correct. If the length
-- argument is incorrect, it is possible to overstep the end of the
-- byte array.
--
-- * if the underying Addr# is later modified, this change will be
-- reflected in resulting @Buffer@, breaking referential
-- transparency.
--
-- * the Addr# is assumed to point to memory such that no element exceeds 0x7f.
-- 
-- If in doubt, don't use these functions.
--
unsafePackAsciiAddressLen :: Int -> Addr# -> IO Buffer
unsafePackAsciiAddressLen len addr# = do
    p <- newForeignPtr_ (Ptr addr#)
    return $! PS p 0 len 0
{-# INLINE unsafePackAsciiAddressLen #-}

-- | /O(n)/ Construct a 'Buffer' given a Ptr Word8 to a buffer, a
-- length, and an IO action representing a finalizer. This function is
-- not available on Hugs.
--
-- This function is /unsafe/, it is possible to break referential
-- transparency by modifying the underlying buffer pointed to by the
-- first argument. Any changes to the original buffer will be reflected
-- in the resulting @Buffer@.
--
unsafePackCStringFinalizer :: Ptr Word8 -> Int -> IO () -> IO Buffer
unsafePackCStringFinalizer p l f = do
    fp <- FC.newForeignPtr p f
    return $! ps fp 0 l
    
-- | /O(n)/ Construct a 'Buffer' given a Ptr Word8 to a buffer, a
-- length, and an IO action representing a finalizer. This function is
-- not available on Hugs.
--
-- This function is /unsafe/ in two ways:
--
-- * it is possible to break referential
-- transparency by modifying the underlying buffer pointed to by the
-- first argument. Any changes to the original buffer will be reflected
-- in the resulting @Buffer@.
-- 
-- * the CString is assumed to point to memory such that no element exceeds 0x7f.
--
unsafePackAsciiCStringFinalizer :: Ptr Word8 -> Int -> IO () -> IO Buffer
unsafePackAsciiCStringFinalizer p l f = do
    fp <- FC.newForeignPtr p f
    return $! PS fp 0 l 0

-- | Explicitly run the finaliser associated with a 'Buffer'.
-- References to this value after finalisation may generate invalid memory
-- references.
--
-- This function is /unsafe/, as there may be other
-- 'Buffers' referring to the same underlying pages. If you use
-- this, you need to have a proof of some kind that all 'Buffer's
-- ever generated from the underlying byte array are no longer live.
--
unsafeFinalize :: Buffer -> IO ()
unsafeFinalize (PS p _ _ _) = FC.finalizeForeignPtr p

#endif

------------------------------------------------------------------------
-- Packing CStrings into Buffers

-- | /O(n)/ Build a @Buffer@ from a @CString@. This value will have /no/
-- finalizer associated to it, and will not be garbage collected by
-- Haskell. The Buffer length is calculated using /strlen(3)/,
-- and thus the complexity is a /O(n)/.
--
-- This function is /unsafe/. If the @CString@ is later modified, this
-- change will be reflected in the resulting @Buffer@, breaking
-- referential transparency.

unsafePackCString :: CString -> IO Buffer
unsafePackCString cstr = do
    fp <- newForeignPtr_ (castPtr cstr)
    l <- c_strlen cstr
    return $! ps fp 0 (fromIntegral l)

-- | /O(n)/ Build a @Buffer@ from a @CString@. This value will have /no/
-- finalizer associated to it, and will not be garbage collected by
-- Haskell. The Buffer length is calculated using /strlen(3)/,
-- and thus the complexity is a /O(n)/.
--
-- This function is /unsafe/. If the @CString@ is later modified, this
-- change will be reflected in the resulting @Buffer@, breaking
-- referential transparency. In addition the CString must not contain
-- any bytes greater than 0x7f.

unsafePackAsciiCString :: CString -> IO Buffer
unsafePackAsciiCString cstr = do
    fp <- newForeignPtr_ (castPtr cstr)
    l <- c_strlen cstr
    return $! PS fp 0 (fromIntegral l) 0

-- | /O(n)/ Build a @Buffer@ from a @CStringLen@. This value will
-- have /no/ finalizer associated with it, and will not be garbage
-- collected by Haskell. This operation has /O(1)/ complexity as we
-- already know the final size, so no /strlen(3)/ is required.
--
-- This funtion is /unsafe/. If the original @CStringLen@ is later
-- modified, this change will be reflected in the resulting @Buffer@,
-- breaking referential transparency.
--
unsafePackCStringLen :: CStringLen -> IO Buffer
unsafePackCStringLen (ptr,len) = do
    fp <- newForeignPtr_ (castPtr ptr)
    return $! ps fp 0 (fromIntegral len)
    
-- | /O(1)/ Build a @Buffer@ from a @CStringLen@. This value will
-- have /no/ finalizer associated with it, and will not be garbage
-- collected by Haskell. This operation has /O(1)/ complexity as we
-- already know the final size, so no /strlen(3)/ is required.
--
-- This funtion is /unsafe/ in two ways: 
-- 
-- * if the original @CStringLen@ is later
-- modified, this change will be reflected in the resulting @Buffer@,
-- breaking referential transparency.
--
-- * the CString is assumed to point to memory such that no element exceeds 0x7f.
-- 
unsafePackAsciiCStringLen :: CStringLen -> IO Buffer
unsafePackAsciiCStringLen (ptr,len) = do
    fp <- newForeignPtr_ (castPtr ptr)
    return $! PS fp 0 (fromIntegral len) 0
    
-- | /O(n)/ Build a @Buffer@ from a malloced @CString@. This value will
-- have a @free(3)@ finalizer associated to it.
--
-- This funtion is /unsafe/. If the original @CString@ is later
-- modified, this change will be reflected in the resulting @Buffer@,
-- breaking referential transparency.
--
-- This function is also unsafe if you call its finalizer twice,
-- which will result in a /double free/ error, or if you pass it
-- a CString not allocated with 'malloc'.
--
unsafePackMallocCString :: CString -> IO Buffer
unsafePackMallocCString cstr = do
    fp <- newForeignPtr c_free_finalizer (castPtr cstr)
    len <- c_strlen cstr
    return $! ps fp 0 (fromIntegral len)

-- | /O(n)/ Build a @Buffer@ from a malloced @CString@. This value will
-- have a @free(3)@ finalizer associated to it.
--
-- This funtion is /unsafe/. If the original @CString@ is later
-- modified, this change will be reflected in the resulting @Buffer@,
-- breaking referential transparency. In addition, the original 
-- CString may not contain any bytes greter than 0x7f.
--
-- This function is also unsafe if you call its finalizer twice,
-- which will result in a /double free/ error, or if you pass it
-- a CString not allocated with 'malloc'.
--
unsafePackMallocAsciiCString :: CString -> IO Buffer
unsafePackMallocAsciiCString cstr = do
    fp <- newForeignPtr c_free_finalizer (castPtr cstr)
    len <- c_strlen cstr
    return $! PS fp 0 (fromIntegral len) 0

-- ---------------------------------------------------------------------

-- | /O(1) construction/ Use a @Buffer@ with a function requiring a
-- @CString@.
--
-- This function does zero copying, and merely unwraps a @Buffer@ to
-- appear as a @CString@. It is /unsafe/ in two ways:
--
-- * After calling this function the @CString@ shares the underlying
-- byte buffer with the original @Buffer@. Thus modifying the
-- @CString@, either in C, or using poke, will cause the contents of the
-- @Buffer@ to change, breaking referential transparency. Other
-- @Buffers@ created by sharing (such as those produced via 'take'
-- or 'drop') will also reflect these changes. Modifying the @CString@
-- will break referential transparency. To avoid this, use
-- @useAsCString@, which makes a copy of the original @Buffer@.
--
-- * @CStrings@ are often passed to functions that require them to be
-- null-terminated. If the original @Buffer@ wasn't null terminated,
-- neither will the @CString@ be. It is the programmers responsibility
-- to guarantee that the @Buffer@ is indeed null terminated. If in
-- doubt, use @useAsCString@.
--
unsafeUseAsCString :: Buffer -> (CString -> IO a) -> IO a
unsafeUseAsCString (PS x s _ _) ac = withForeignPtr x $ \p -> ac (castPtr p `plusPtr` s)

-- | /O(1) construction/ Use a @Buffer@ with a function requiring a
-- @CStringLen@.
-- 
-- This function does zero copying, and merely unwraps a @Buffer@ to
-- appear as a @CStringLen@. It is /unsafe/:
--
-- * After calling this function the @CStringLen@ shares the underlying
-- byte buffer with the original @Buffer@. Thus modifying the
-- @CStringLen@, either in C, or using poke, will cause the contents of the
-- @Buffer@ to change, breaking referential transparency. Other
-- @Buffers@ created by sharing (such as those produced via 'take'
-- or 'drop') will also reflect these changes. Modifying the @CStringLen@
-- will break referential transparency. To avoid this, use
-- @useAsCStringLen@, which makes a copy of the original @Buffer@.
--
unsafeUseAsCStringLen :: Buffer -> (CStringLen -> IO a) -> IO a
unsafeUseAsCStringLen (PS x s l _) f = withForeignPtr x $ \p -> f (castPtr p `plusPtr` s,l)
