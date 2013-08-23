{-# LANGUAGE CPP, ForeignFunctionInterface #-}
-- We cannot actually specify all the language pragmas, see ghc ticket #
-- If we could, these are what they would be:
{- LANGUAGE UnliftedFFITypes, MagicHash,
            UnboxedTuples, DeriveDataTypeable -}
{-# OPTIONS_HADDOCK hide #-}

-- |
-- Module      : Data.Buffer.Internal
-- License     : BSD-style
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- A module containing semi-public 'Buffer' internals. This exposes the
-- 'Buffer' representation and low level construction functions. As such
-- all the functions in this module are unsafe. The API is also not stable.
--
-- Where possible application should instead use the functions from the normal
-- public interface modules, such as "Data.Buffer.Unsafe". Packages that
-- extend the Buffer system at a low level will need to use this module.
--
module Data.Buffer.Internal (

        -- * The @Buffer@ type and representation
        Buffer(..),         -- instances: Eq, Ord, Show, Read, Data, Typeable

        -- * Low level introduction and elimination
        create,                 -- :: Int -> (Ptr Word8 -> IO ()) -> IO Buffer
        create',                -- :: Int -> (Ptr Word8 -> IO Int) -> IO Buffer
        createAndTrim,          -- :: Int -> (Ptr Word8 -> IO Int) -> IO Buffer
        createAndTrim',         -- :: Int -> (Ptr Word8 -> IO (Int, Int, a)) -> IO (Buffer, a)
        unsafeCreate,           -- :: Int -> (Ptr Word8 -> IO ()) -> Buffer
        unsafeCreate',          -- :: Int -> (Ptr Word8 -> IO Int) -> Buffer
        mallocBuffer,           -- :: Int -> IO (ForeignPtr a)

        -- * Conversion to and from ForeignPtrs
        fromForeignPtr,         -- :: ForeignPtr Word8 -> Int -> Int -> Buffer
        toForeignPtr,           -- :: Buffer -> (ForeignPtr Word8, Int, Int)

        -- * Utilities
        inlinePerformIO,        -- :: IO a -> a
        nullForeignPtr,         -- :: ForeignPtr Word8

        -- * Standard C Functions
        c_strlen,               -- :: CString -> IO CInt
        c_free_finalizer,       -- :: FunPtr (Ptr Word8 -> IO ())

        memchr,                 -- :: Ptr Word8 -> Word8 -> CSize -> IO Ptr Word8
        memcmp,                 -- :: Ptr Word8 -> Ptr Word8 -> CSize -> IO CInt
        memcpy,                 -- :: Ptr Word8 -> Ptr Word8 -> CSize -> IO ()
        memset,                 -- :: Ptr Word8 -> Word8 -> CSize -> IO (Ptr Word8)

        -- * cbits functions
        c_reverse,              -- :: Ptr Word8 -> Ptr Word8 -> CInt -> IO ()
        c_intersperse,          -- :: Ptr Word8 -> Ptr Word8 -> CInt -> Word8 -> IO ()
        c_maximum,              -- :: Ptr Word8 -> CInt -> IO Word8
        c_minimum,              -- :: Ptr Word8 -> CInt -> IO Word8
        c_count,                -- :: Ptr Word8 -> CInt -> Word8 -> IO CInt
        c_count_extras,         -- :: Ptr Word8 -> CInt -> Word8 -> IO CInt
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 611
        -- * Internal GHC magic
        memcpy_ptr_baoff,       -- :: Ptr a -> RawBuffer -> CInt -> CSize -> IO (Ptr ())
#endif

        -- * Chars
        w2c, c2w, isSpaceWord8, isSpaceChar8,

        -- * Smart Constructor
        ps, -- :: !!(ForeignPtr Word8) -> Int -> Int -> Buffer
        ps0, -- :: Buffer -> Int -> Int -> Buffer

        -- * UTF8 multibyte tail counting
        extra,      -- :: Word8 -> Int
        extras,     -- :: ForeignPtr Word8 -> Int -> Int -> Int
        minusExtra, -- :: Int -> Word8 -> Int
        plusExtra   -- :: Int -> Word8 -> Int
  ) where

import Foreign.ForeignPtr       (ForeignPtr, withForeignPtr)
import Foreign.Ptr              (Ptr, FunPtr, plusPtr)
import Foreign.Storable         (Storable(..))
import Foreign.C.Types          (CInt, CSize, CULong)
import Foreign.C.String         (CString)

#ifndef __NHC__
import Control.Exception        (assert)
#endif

import Data.Char                (ord)
import Data.Word                (Word8)
import qualified Data.ByteString as B

#if defined(__GLASGOW_HASKELL__)
import Data.Typeable            (Typeable)
#if __GLASGOW_HASKELL__ >= 610
import Data.Data                (Data)
#else
import Data.Generics            (Data)
#endif
import GHC.Base                 (realWorld#,unsafeChr)
#if __GLASGOW_HASKELL__ >= 611
import GHC.IO                   (IO(IO))
#else
import GHC.IOBase               (IO(IO),RawBuffer)
#endif
#if __GLASGOW_HASKELL__ >= 611
import GHC.IO                   (unsafeDupablePerformIO)
#elif __GLASGOW_HASKELL__ >= 608
import GHC.IOBase               (unsafeDupablePerformIO)
#else
import GHC.IOBase               (unsafePerformIO)
#endif
#else
import Data.Char                (chr)
import System.IO.Unsafe         (unsafePerformIO)
#endif

#if __GLASGOW_HASKELL__ >= 605 && !defined(SLOW_FOREIGN_PTR)
import GHC.ForeignPtr           (mallocPlainForeignPtrBytes)
#else
import Foreign.ForeignPtr       (mallocForeignPtrBytes)
#endif

#if __GLASGOW_HASKELL__>=605
import GHC.ForeignPtr           (ForeignPtr(ForeignPtr))
import GHC.Base                 (nullAddr#)
#else
import Foreign.Ptr              (nullPtr)
#endif

#if __HUGS__
import Hugs.ForeignPtr          (newForeignPtr_)
#elif __GLASGOW_HASKELL__<=604
import Foreign.ForeignPtr       (newForeignPtr_)
#endif

-- CFILES stuff is Hugs only
{-# CFILES cbits/fpstring.c #-}

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

-- -----------------------------------------------------------------------------
-- 
-- a couple of classes that we use extensively in the rope implementation
--

#include "measure.h"

instance Measured Buffer where
    measureU (PS _ _ l e) = PACK(l, e)

instance Sized Buffer where
    sizeU (PS _ _ l _) = UNBOX(l)


-- -----------------------------------------------------------------------------

-- | A space-efficient representation of a Word8 vector, supporting many
-- efficient operations.  A 'Buffer' contains 8-bit characters only.
--
-- Instances of Eq, Ord, Read, Show, Data, Typeable
--
data Buffer = PS {-# UNPACK #-} !(ForeignPtr Word8) -- payload
                     {-# UNPACK #-} !Int            -- offset
                     {-# UNPACK #-} !Int            -- length
                     {-# UNPACK #-} !Int            -- extras
#if defined(__GLASGOW_HASKELL__)
    deriving (Data, Typeable)
#endif

instance Measured Buffer where
    measure (PS _ _ l e) = (# l, e #)

instance Sized Buffer where
    size (PS _ _ l _) = l

instance Valid Buffer where
    valid (PS x s l e) = l >= 0 && e >= 0 && e <= l && e == inlinePerformIO (withForeignPtr x $ \p -> go (p `plusPtr` s) 0 0)
    where
        STRICT3(go)
        go _ 0 acc = return acc
        go p n acc = do
            w <- peekByteOff p n
            go p (n-1) $ acc `plusExtra` w

-- | Smart constructor that builds a 'Buffer' using the same arguments required to build a 'ByteString' by
-- just counting the multibyte tail bytes for you.
-- TODO: replace internal 'go' with a call to 'c_count_extras'
ps :: ForeignPtr Word8 -> Int -> Int -> Buffer
ps x s l = PS x s l (extras x s l)
{-# INLINE ps #-}

extras :: ForeignPtr Word8 -> Int -> Int -> Int
extras p s l = fromIntegral $ inlinePerformIO $ withForeignPtr x $ \p -> c_count_extras (p `plusPtr` s) (fromIntegral l)

-- | /O(n)/ - Conversion from a strict 'ByteString'
fromByteString :: ByteString -> Buffer
fromByteString (B.PS x s l) = ps x s l

-- | /O(1)/ - Conversion to a strict 'ByteString'
toByteString :: Buffer -> ByteString
toByteString (PS x s l _) = B.PS x s l

-- | Exploit the fact that if out 'parent' Buffer doesn't have any multibyte tail bytes
-- then neither can we. This could work smarter by exploiting the size of the interval
-- to determine if it makes more sense to excise parts of the parents tail byte count,
-- but for the common case, this should suffice.
ps0 :: Buffer -> Int -> Int -> Buffer
ps0 (PS x _ _ 0) s l = PS x s l 0
ps0 (PS x _ _ _) s l = ps x s l
{-# INLINE ps0 #-}


extra :: Word8 -> Int
STRICT1(extra)
extra b = if b >= 0x80 && b < 0xC0 then 1 else 0
{-# INLINE extra #-}


minusExtra :: Int -> Word8 -> Int
STRICT2(minusExtra)
minusExtra 0 _ = 0 
minusExtra e b = e - extra b
{-# INLINE minusExtra #-}

plusExtra :: Int -> Word8 -> Int
STRICT2(plusExtra)
plusExtra e b = e + extra b
{-# INLINE plusExtra #-}

-- HACK. TODO: unpack via UTF8B
instance Show Buffer where
    showsPrec p pr r = showsPrec p (unpackWith w2c pr) r

-- HACK. TODO: repack via UTF8B
instance Read Buffer where
    readsPrec p str = [ (packWith c2w x, y) | (x, y) <- readsPrec p str ]

-- | /O(n)/ Converts a 'Buffer' to a '[a]', using a conversion function.
unpackWith :: (Word8 -> a) -> Buffer -> [a]
unpackWith _ (PS _  _ 0 _) = []
unpackWith k (PS x s l _) = inlinePerformIO $ withForeignPtr x $ \p ->
        go (p `plusPtr` s) (l - 1) []
    where
        STRICT3(go)
        go p 0 acc = peek p          >>= \e -> return (k e : acc)
        go p n acc = peekByteOff p n >>= \e -> go p (n-1) (k e : acc)
{-# INLINE unpackWith #-}
{-# SPECIALIZE unpackWith :: (Word8 -> Char) -> Buffer -> [Char] #-}

-- | /O(n)/ Convert a '[a]' into a 'Buffer' using some
-- conversion function
packWith :: (a -> Word8) -> [a] -> Buffer
packWith k str = unsafeCreate' (length str) $ \p -> go p str 0
    where
        STRICT3(go)
        go _ [] acc = return acc
        go p (x:xs) acc = do
            let kx = k x
            poke p kx
            go (p `plusPtr` 1) xs $! acc + extra kx
{-# INLINE packWith #-}
{-# SPECIALIZE packWith :: (Char -> Word8) -> [Char] -> Buffer #-}

------------------------------------------------------------------------

-- | The 0 pointer. Used to indicate the empty Buffer.
nullForeignPtr :: ForeignPtr Word8
#if __GLASGOW_HASKELL__>=605
nullForeignPtr = ForeignPtr nullAddr# undefined --TODO: should ForeignPtrContents be strict?
#else
nullForeignPtr = unsafePerformIO $ newForeignPtr_ nullPtr
{-# NOINLINE nullForeignPtr #-}
#endif

-- ---------------------------------------------------------------------
-- Low level constructors

-- | /O(n)/ Build a Buffer from a ForeignPtr.
--
-- If you do not need the offset parameter then you do should be using
-- 'Data.Buffer.Unsafe.unsafePackCStringLen' or
-- 'Data.Buffer.Unsafe.unsafePackCStringFinalizer' instead.
--
fromForeignPtr :: ForeignPtr Word8
               -> Int -- ^ Offset
               -> Int -- ^ Length
               -> Buffer
fromForeignPtr fp s l = ps fp s l
{-# INLINE fromForeignPtr #-}

-- | /O(1)/ Deconstruct a ForeignPtr from a Buffer
toForeignPtr :: Buffer -> (ForeignPtr Word8, Int, Int) -- ^ (ptr, offset, length)
toForeignPtr (PS x s l _) = (x, s, l)
{-# INLINE toForeignPtr #-}

-- | A way of creating Buffers outside the IO monad. The @Int@
-- argument gives the final size of the Buffer. Unlike
-- 'createAndTrim' the Buffer is not reallocated if the final size
-- is less than the estimated size.
unsafeCreate :: Int -> (Ptr Word8 -> IO ()) -> Buffer
unsafeCreate l f = unsafeDupablePerformIO (create l f)
{-# INLINE unsafeCreate #-}

-- | A way of creating Buffers outside the IO monad. The @Int@
-- argument gives the final size of the Buffer. Unlike
-- 'createAndTrim' the Buffer is not reallocated if the final size
-- is less than the estimated size. The action must also return an accurate count of the multibyte tail bytes
unsafeCreate' :: Int -> (Ptr Word8 -> IO Int) -> Buffer
unsafeCreate' l f = unsafeDupablePerformIO (create' l f)
{-# INLINE unsafeCreate' #-}


#if !defined(__GLASGOW_HASKELL__) || __GLASGOW_HASKELL__ < 608
-- for Hugs   
unsafeDupablePerformIO :: IO a -> a
unsafeDupablePerformIO = unsafePerformIO
#endif

-- | Create Buffer of size @l@ and use action @f@ to fill it's contents.
create :: Int -> (Ptr Word8 -> IO ()) -> IO Buffer
create l f = do
    fp <- mallocBuffer l
    withForeignPtr fp $ \p -> f p
    return $! ps fp 0 l
{-# INLINE create #-}


-- | Create Buffer of size @l@ and use action @f@ to fill it's contents and count the multibyte tail bytes
-- The safety of this method relies on the correctness of the returned multibyte tail byte count.
create' :: Int -> (Ptr Word8 -> IO Int) -> IO Buffer
create' l f = do
    fp <- mallocBuffer l
    h <- withForeignPtr fp $ \p -> f p
    return $! PS fp 0 l h 
{-# INLINE create' #-}

-- | Given the maximum size needed and a function to make the contents
-- of a Buffer, createAndTrim makes the 'Buffer'. The generating
-- function is required to return the actual final size (<= the maximum
-- size), and the resulting byte array is realloced to this size.
--
-- createAndTrim is the main mechanism for creating custom, efficient
-- Buffer functions, using Haskell or C functions to fill the space.
--
createAndTrim :: Int -> (Ptr Word8 -> IO Int) -> IO Buffer
createAndTrim l f = do
    fp <- mallocBuffer l
    withForeignPtr fp $ \p -> do
        l' <- f p
        if assert (l' <= l) $ l' >= l
            then return $! ps fp 0 l
            else create l' $ \p' -> memcpy p' p (fromIntegral l')
{-# INLINE createAndTrim #-}

createAndTrim' :: Int -> (Ptr Word8 -> IO (Int, Int, a)) -> IO (Buffer, a)
createAndTrim' l f = do
    fp <- mallocBuffer l
    withForeignPtr fp $ \p -> do
        (off, l', res) <- f p
        if assert (l' <= l) $ l' >= l
            then return $! (ps fp 0 l, res)
            else do q <- create l' $ \p' -> memcpy p' (p `plusPtr` off) (fromIntegral l')
                    return $! (q, res)

-- | Wrapper of mallocForeignPtrBytes with faster implementation
-- for GHC 6.5 builds newer than 06/06/06
mallocBuffer :: Int -> IO (ForeignPtr a)
mallocBuffer l = do
#if __GLASGOW_HASKELL__ >= 605 && !defined(SLOW_FOREIGN_PTR)
    mallocPlainForeignPtrBytes l
#else
    mallocForeignPtrBytes l
#endif
{-# INLINE mallocBuffer #-}

------------------------------------------------------------------------

-- | Conversion between 'Word8' and 'Char'. Should compile to a no-op.
w2c :: Word8 -> Char
#if !defined(__GLASGOW_HASKELL__)
w2c = chr . fromIntegral
#else
w2c = unsafeChr . fromIntegral
#endif
{-# INLINE w2c #-}

-- | Unsafe conversion between 'Char' and 'Word8'. This is a no-op and
-- silently truncates to 8 bits Chars > '\255'. It is provided as
-- convenience for Buffer construction.
c2w :: Char -> Word8
c2w = fromIntegral . ord
{-# INLINE c2w #-}

-- | Selects words corresponding to white-space characters in the Latin-1 range
-- ordered by frequency. 
isSpaceWord8 :: Word8 -> Bool
isSpaceWord8 w =
    w == 0x20 ||
    w == 0x0A || -- LF, \n
    w == 0x09 || -- HT, \t
    w == 0x0C || -- FF, \f
    w == 0x0D || -- CR, \r
    w == 0x0B || -- VT, \v
    w == 0xA0    -- spotted by QC..
{-# INLINE isSpaceWord8 #-}

-- | Selects white-space characters in the Latin-1 range
isSpaceChar8 :: Char -> Bool
isSpaceChar8 c =
    c == ' '     ||
    c == '\t'    ||
    c == '\n'    ||
    c == '\r'    ||
    c == '\f'    ||
    c == '\v'    ||
    c == '\xa0'
{-# INLINE isSpaceChar8 #-}

------------------------------------------------------------------------

-- | Just like unsafePerformIO, but we inline it. Big performance gains as
-- it exposes lots of things to further inlining. /Very unsafe/. In
-- particular, you should do no memory allocation inside an
-- 'inlinePerformIO' block. On Hugs this is just @unsafePerformIO@.
--
{-# INLINE inlinePerformIO #-}
inlinePerformIO :: IO a -> a
#if defined(__GLASGOW_HASKELL__)
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
#else
inlinePerformIO = unsafePerformIO
#endif

-- ---------------------------------------------------------------------
-- 
-- Standard C functions
--

foreign import ccall unsafe "string.h strlen" c_strlen
    :: CString -> IO CSize

foreign import ccall unsafe "static stdlib.h &free" c_free_finalizer
    :: FunPtr (Ptr Word8 -> IO ())

foreign import ccall unsafe "string.h memchr" c_memchr
    :: Ptr Word8 -> CInt -> CSize -> IO (Ptr Word8)

memchr :: Ptr Word8 -> Word8 -> CSize -> IO (Ptr Word8)
memchr p w s = c_memchr p (fromIntegral w) s

foreign import ccall unsafe "string.h memcmp" memcmp
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO CInt

foreign import ccall unsafe "string.h memcpy" c_memcpy
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO (Ptr Word8)

memcpy :: Ptr Word8 -> Ptr Word8 -> CSize -> IO ()
memcpy p q s = c_memcpy p q s >> return ()

{-
foreign import ccall unsafe "string.h memmove" c_memmove
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO (Ptr Word8)

memmove :: Ptr Word8 -> Ptr Word8 -> CSize -> IO ()
memmove p q s = do c_memmove p q s
                   return ()
-}

foreign import ccall unsafe "string.h memset" c_memset
    :: Ptr Word8 -> CInt -> CSize -> IO (Ptr Word8)

memset :: Ptr Word8 -> Word8 -> CSize -> IO (Ptr Word8)
memset p w s = c_memset p (fromIntegral w) s

-- ---------------------------------------------------------------------
--
-- Uses our C code
--

foreign import ccall unsafe "static fpstring.h bfps_reverse" c_reverse
    :: Ptr Word8 -> Ptr Word8 -> CULong -> IO ()

foreign import ccall unsafe "static fpstring.h bfps_intersperse" c_intersperse
    :: Ptr Word8 -> Ptr Word8 -> CULong -> Word8 -> IO ()

foreign import ccall unsafe "static fpstring.h bfps_maximum" c_maximum
    :: Ptr Word8 -> CULong -> IO Word8

foreign import ccall unsafe "static fpstring.h bfps_minimum" c_minimum
    :: Ptr Word8 -> CULong -> IO Word8

foreign import ccall unsafe "static fpstring.h bfps_count" c_count
    :: Ptr Word8 -> CULong -> Word8 -> IO CULong
    
foreign import ccall unsafe "static fpstring.h bfps_count_extras" c_count_extras
    :: Ptr Word8 -> CULong -> Word8 -> IO CULong    

-- ---------------------------------------------------------------------
-- Internal GHC Haskell magic

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 611
foreign import ccall unsafe "__hscore_memcpy_src_off"
   memcpy_ptr_baoff :: Ptr a -> RawBuffer -> CInt -> CSize -> IO (Ptr ())
#endif
