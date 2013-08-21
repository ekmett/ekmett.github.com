{-# LANGUAGE CPP #-}
-- We cannot actually specify all the language pragmas, see ghc ticket #
-- If we could, these are what they would be:
{- LANGUAGE MagicHash, UnboxedTuples,
            NamedFieldPuns, BangPatterns, RecordWildCards -}
{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module      : Data.Buffer
-- Copyright   : (c) The University of Glasgow 2001,
--               (c) David Roundy 2003-2005,
--               (c) Simon Marlow 2005
--               (c) Bjorn Bringert 2006
--               (c) Don Stewart 2005-2008
--               (c) Edward Kmett 2009-2010
--
--               Array fusion code:
--               (c) 2001,2002 Manuel M T Chakravarty & Gabriele Keller
--               (c) 2006      Manuel M T Chakravarty & Roman Leshchinskiy
--
-- License     : BSD-style
--
-- Maintainer  : ekmett@gmail.com
-- Stability   : experimental
-- Portability : portable
-- 
-- A time and space-efficient implementation of byte vectors using
-- packed Word8 arrays, suitable for high performance use, both in terms
-- of large data quantities, or high speed requirements. Byte vectors
-- are encoded as strict 'Word8' arrays of bytes, held in a 'ForeignPtr',
-- and can be passed between C and Haskell with little effort.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions.  eg.
--
-- > import qualified Data.Buffer as B
--
-- Original GHC implementation by Bryan O\'Sullivan.
-- Rewritten to use 'Data.Array.Unboxed.UArray' by Simon Marlow.
-- Rewritten to support slices and use 'ForeignPtr' by David Roundy.
-- Polished and extended by Don Stewart.
--

module Data.Buffer.Word8 (

        -- * The @Buffer@ type
        Buffer,             -- abstract, instances: Eq, Ord, Show, Read, Data, Typeable, Monoid

        -- * Introducing and eliminating 'Buffer's
        empty,                  -- :: Buffer
        singleton,              -- :: Word8   -> Buffer
        pack,                   -- :: [Word8] -> Buffer
        unpack,                 -- :: Buffer -> [Word8]

        -- * Basic interface
        cons,                   -- :: Word8 -> Buffer -> Buffer
        snoc,                   -- :: Buffer -> Word8 -> Buffer
        append,                 -- :: Buffer -> Buffer -> Buffer
        head,                   -- :: Buffer -> Word8
        uncons,                 -- :: Buffer -> Maybe (Word8, Buffer)
        unsnoc,                 -- :: Buffer -> Maybe (Buffer, Word8)
        last,                   -- :: Buffer -> Word8
        tail,                   -- :: Buffer -> Buffer
        init,                   -- :: Buffer -> Buffer
        null,                   -- :: Buffer -> Bool
        length,                 -- :: Buffer -> Int

        -- * Transforming Buffers
        map,                    -- :: (Word8 -> Word8) -> Buffer -> Buffer
        reverse,                -- :: Buffer -> Buffer
        intersperse,            -- :: Word8 -> Buffer -> Buffer
        intercalate,            -- :: Buffer -> [Buffer] -> Buffer
        transpose,              -- :: [Buffer] -> [Buffer]

        -- * Reducing 'Buffer's (folds)
        foldl,                  -- :: (a -> Word8 -> a) -> a -> Buffer -> a
        foldl',                 -- :: (a -> Word8 -> a) -> a -> Buffer -> a
        foldl1,                 -- :: (Word8 -> Word8 -> Word8) -> Buffer -> Word8
        foldl1',                -- :: (Word8 -> Word8 -> Word8) -> Buffer -> Word8

        foldr,                  -- :: (Word8 -> a -> a) -> a -> Buffer -> a
        foldr',                 -- :: (Word8 -> a -> a) -> a -> Buffer -> a
        foldr1,                 -- :: (Word8 -> Word8 -> Word8) -> Buffer -> Word8
        foldr1',                -- :: (Word8 -> Word8 -> Word8) -> Buffer -> Word8

        -- ** Special folds
        concat,                 -- :: [Buffer] -> Buffer
        concatMap,              -- :: (Word8 -> Buffer) -> Buffer -> Buffer
        any,                    -- :: (Word8 -> Bool) -> Buffer -> Bool
        all,                    -- :: (Word8 -> Bool) -> Buffer -> Bool
        maximum,                -- :: Buffer -> Word8
        minimum,                -- :: Buffer -> Word8

        -- * Building Buffers
        -- ** Scans
        scanl,                  -- :: (Word8 -> Word8 -> Word8) -> Word8 -> Buffer -> Buffer
        scanl1,                 -- :: (Word8 -> Word8 -> Word8) -> Buffer -> Buffer
        scanr,                  -- :: (Word8 -> Word8 -> Word8) -> Word8 -> Buffer -> Buffer
        scanr1,                 -- :: (Word8 -> Word8 -> Word8) -> Buffer -> Buffer

        -- ** Accumulating maps
        mapAccumL,              -- :: (acc -> Word8 -> (acc, Word8)) -> acc -> Buffer -> (acc, Buffer)
        mapAccumR,              -- :: (acc -> Word8 -> (acc, Word8)) -> acc -> Buffer -> (acc, Buffer)

        -- ** Generating and unfolding Buffers
        replicate,              -- :: Int -> Word8 -> Buffer
        unfoldr,                -- :: (a -> Maybe (Word8, a)) -> a -> Buffer
        unfoldrN,               -- :: Int -> (a -> Maybe (Word8, a)) -> a -> (Buffer, Maybe a)

        -- * Substrings

        -- ** Breaking strings
        take,                   -- :: Int -> Buffer -> Buffer
        drop,                   -- :: Int -> Buffer -> Buffer
        splitAt,                -- :: Int -> Buffer -> (Buffer, Buffer)
        takeWhile,              -- :: (Word8 -> Bool) -> Buffer -> Buffer
        dropWhile,              -- :: (Word8 -> Bool) -> Buffer -> Buffer
        span,                   -- :: (Word8 -> Bool) -> Buffer -> (Buffer, Buffer)
        spanEnd,                -- :: (Word8 -> Bool) -> Buffer -> (Buffer, Buffer)
        break,                  -- :: (Word8 -> Bool) -> Buffer -> (Buffer, Buffer)
        breakEnd,               -- :: (Word8 -> Bool) -> Buffer -> (Buffer, Buffer)
        group,                  -- :: Buffer -> [Buffer]
        groupBy,                -- :: (Word8 -> Word8 -> Bool) -> Buffer -> [Buffer]
        inits,                  -- :: Buffer -> [Buffer]
        tails,                  -- :: Buffer -> [Buffer]

        -- ** Breaking into many substrings
        split,                  -- :: Word8 -> Buffer -> [Buffer]
        splitWith,              -- :: (Word8 -> Bool) -> Buffer -> [Buffer]

        -- * Predicates
        isPrefixOf,             -- :: Buffer -> Buffer -> Bool
        isSuffixOf,             -- :: Buffer -> Buffer -> Bool
        isInfixOf,              -- :: Buffer -> Buffer -> Bool

        -- ** Search for arbitrary substrings
        breakSubstring,         -- :: Buffer -> Buffer -> (Buffer,Buffer)

        -- * Searching Buffers

        -- ** Searching by equality
        elem,                   -- :: Word8 -> Buffer -> Bool
        notElem,                -- :: Word8 -> Buffer -> Bool

        -- ** Searching with a predicate
        find,                   -- :: (Word8 -> Bool) -> Buffer -> Maybe Word8
        filter,                 -- :: (Word8 -> Bool) -> Buffer -> Buffer
        partition,              -- :: (Word8 -> Bool) -> Buffer -> (Buffer, Buffer)

        -- * Indexing Buffers
        index,                  -- :: Buffer -> Int -> Word8
        elemIndex,              -- :: Word8 -> Buffer -> Maybe Int
        elemIndices,            -- :: Word8 -> Buffer -> [Int]
        elemIndexEnd,           -- :: Word8 -> Buffer -> Maybe Int
        findIndex,              -- :: (Word8 -> Bool) -> Buffer -> Maybe Int
        findIndices,            -- :: (Word8 -> Bool) -> Buffer -> [Int]
        count,                  -- :: Word8 -> Buffer -> Int

        -- * Zipping and unzipping Buffers
        zip,                    -- :: Buffer -> Buffer -> [(Word8,Word8)]
        zipWith,                -- :: (Word8 -> Word8 -> c) -> Buffer -> Buffer -> [c]
        unzip,                  -- :: [(Word8,Word8)] -> (Buffer,Buffer)

        -- * Ordered Buffers
        sort,                   -- :: Buffer -> Buffer

        -- * Low level conversions
        -- ** Copying Buffers
        copy,                   -- :: Buffer -> Buffer
        copyCString,            -- :: CString -> Buffer
        copyCStringLen,         -- :: CStringLen -> Buffer

        -- ** Packing 'CString's and pointers
        packCString,            -- :: CString -> IO Buffer
        packCStringLen,         -- :: CStringLen -> IO Buffer
        packMallocCString       -- :: CString -> ByteString

        -- ** Using Buffers as 'CString's
        useAsCString,           -- :: Buffer -> (CString    -> IO a) -> IO a
        useAsCStringLen,        -- :: Buffer -> (CStringLen -> IO a) -> IO a

        -- * I\/O with 'Buffer's

        -- ** Standard input and output
        getLine,                -- :: IO Buffer
        getContents,            -- :: IO Buffer
        putStr,                 -- :: Buffer -> IO ()
        putStrLn,               -- :: Buffer -> IO ()
        interact,               -- :: (Buffer -> Buffer) -> IO ()

        -- ** Files
        readFile,               -- :: FilePath -> IO Buffer
        writeFile,              -- :: FilePath -> Buffer -> IO ()
        appendFile,             -- :: FilePath -> Buffer -> IO ()

        -- ** I\/O with Handles
        hGetLine,               -- :: Handle -> IO Buffer
        hGetContents,           -- :: Handle -> IO Buffer
        hGet,                   -- :: Handle -> Int -> IO Buffer
        hGetNonBlocking,        -- :: Handle -> Int -> IO Buffer
        hPut,                   -- :: Handle -> Buffer -> IO ()
        hPutStr,                -- :: Handle -> Buffer -> IO ()
        hPutStrLn,              -- :: Handle -> Buffer -> IO ()

        breakByte
  ) where

import qualified Prelude as P
import Prelude hiding           (reverse,head,tail,last,init,null
                                ,length,map,lines,foldl,foldr,unlines
                                ,concat,any,take,drop,splitAt,takeWhile
                                ,dropWhile,span,break,elem,filter,maximum
                                ,minimum,all,concatMap,foldl1,foldr1
                                ,scanl,scanl1,scanr,scanr1
                                ,readFile,writeFile,appendFile,replicate
                                ,getContents,getLine,putStr,putStrLn,interact
                                ,zip,zipWith,unzip,notElem)

import Data.Buffer.Internal
import Data.Buffer.Unsafe

import qualified Data.List as List

import Data.Word                (Word8)

-- Control.Exception.assert not available in yhc or nhc
#ifndef __NHC__
import Control.Exception        (finally, bracket, assert)
#else
import Control.Exception    (bracket, finally)
#endif
import Control.Monad            (when)

import Foreign.C.String         (CString, CStringLen)
import Foreign.C.Types          (CSize)
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc    (allocaBytes, mallocBytes, reallocBytes, finalizerFree)
import Foreign.Marshal.Array    (allocaArray)
import Foreign.Ptr
import Foreign.Storable         (Storable(..))

-- hGetBuf and hPutBuf not available in yhc or nhc
import System.IO                (stdin,stdout,hClose,hFileSize
                                ,hGetBuf,hPutBuf,openBinaryFile
                                ,IOMode(..))
import System.IO.Error          (mkIOError, illegalOperationErrorType)

import Data.Monoid              (Monoid, mempty, mappend, mconcat)

#if !defined(__GLASGOW_HASKELL__)
import System.IO.Unsafe
import qualified System.Environment
import qualified System.IO      (hGetLine)
#endif

#if defined(__GLASGOW_HASKELL__)

import System.IO                (hGetBufNonBlocking)

#if __GLASGOW_HASKELL__ >= 611
import Data.IORef
import GHC.IO.Handle.Internals
import GHC.IO.Handle.Types
import GHC.IO.Buffer
import GHC.IO.BufferedIO as Buffered
import GHC.IO hiding (finally)
import Data.Char                (ord)
import Foreign.Marshal.Utils    (copyBytes)
#else
import System.IO.Error          (isEOFError)
import qualified GHC.IOBase as IOBase
import GHC.IOBase hiding (Buffer)
import GHC.Handle
#endif

import GHC.Prim                 (int2Word#, Word#, (+#), geWord#, ltWord#, writeWord8OffAddr#)
import GHC.Base                 (build, Int(..))
import GHC.Word hiding (Word8)
import GHC.Ptr                  (Ptr(..))
import GHC.ST                   (ST(..))

#endif

-- An alternative to Control.Exception (assert) for nhc98
#ifdef __NHC__
#define assert  assertS "__FILE__ : __LINE__"
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

instance Eq  Buffer where
    (==)    = eq

instance Ord Buffer where
    compare = compareBytes

instance Monoid Buffer where
    mempty  = empty
    mappend = append
    mconcat = concat

-- | /O(n)/ Equality on the 'Buffer' type.
eq :: Buffer -> Buffer -> Bool
eq a@(PS p s l x) b@(PS p' s' l' x')
    | l /= l' || x /= x' = False    -- short cut on length and number of extra bytes
    | p == p' && s == s' = True     -- short cut for the same string
    | otherwise          = compareBytes a b == EQ
{-# INLINE eq #-}
-- ^ still needed

-- | /O(n)/ 'compareBytes' provides an 'Ordering' for 'Buffers' supporting slices. 
compareBytes :: Buffer -> Buffer -> Ordering
compareBytes (PS x1 s1 l1 _) (PS x2 s2 l2 _)
    | l1 == 0  && l2 == 0               = EQ  -- short cut for empty strings
    | otherwise                         = inlinePerformIO $
        withForeignPtr x1 $ \p1 ->
        withForeignPtr x2 $ \p2 -> do
            i <- memcmp (p1 `plusPtr` s1) (p2 `plusPtr` s2) (fromIntegral $ min l1 l2)
            return $! case i `compare` 0 of
                        EQ  -> l1 `compare` l2
                        x   -> x

{-

-- Pure Haskell version

compareBytes (PS fp1 off1 len1 _) (PS fp2 off2 len2 _)
--    | len1 == 0  && len2 == 0                     = EQ  -- short cut for empty strings
--    | fp1 == fp2 && off1 == off2 && len1 == len2  = EQ  -- short cut for the same string
    | otherwise                                   = inlinePerformIO $
    withForeignPtr fp1 $ \p1 ->
        withForeignPtr fp2 $ \p2 ->
            cmp (p1 `plusPtr` off1)
                (p2 `plusPtr` off2) 0 len1 len2

-- XXX todo.
cmp :: Ptr Word8 -> Ptr Word8 -> Int -> Int -> Int-> IO Ordering
cmp p1 p2 n len1 len2
      | n == len1 = if n == len2 then return EQ else return LT
      | n == len2 = return GT
      | otherwise = do
          a <- peekByteOff p1 n :: IO Word8
          b <- peekByteOff p2 n
          case a `compare` b of
                EQ -> cmp p1 p2 (n+1) len1 len2
                LT -> return LT
                GT -> return GT
-}

-- -----------------------------------------------------------------------------
-- Introducing and eliminating 'Buffer's

-- | /O(1)/ The empty 'Buffer'
empty :: Buffer
empty = PS nullForeignPtr 0 0 0

-- | /O(1)/ Convert a 'Word8' into a 'Buffer'
singleton :: Word8 -> Buffer
singleton c = unsafeCreate 1 $ \p -> poke p c
{-# INLINE [1] singleton #-}

-- Inline [1] for intercalate rule

--
-- XXX The use of unsafePerformIO in allocating functions (unsafeCreate) is critical!
--
-- Otherwise:
--
--  singleton 255 `compare` singleton 127
--
-- is compiled to:
--
--  case mallocBuffer 2 of 
--      ForeignPtr f internals -> 
--           case writeWord8OffAddr# f 0 255 of _ -> 
--           case writeWord8OffAddr# f 0 127 of _ ->
--           case eqAddr# f f of 
--                  False -> case compare (GHC.Prim.plusAddr# f 0) 
--                                        (GHC.Prim.plusAddr# f 0)
--
--

-- | /O(n)/ Convert a '[Word8]' into a 'Buffer'. 
--
-- For applications with large numbers of string literals, pack can be a
-- bottleneck. In such cases, consider using packAddress (GHC only).
pack :: [Word8] -> Buffer

#if !defined(__GLASGOW_HASKELL__)
pack str = unsafeCreate' (P.length str) $ \p -> go p str 0
    where
        STRICT3(go)
        go _ [] acc    = return acc
        go p (x:xs) acc = poke p x >> go (p `plusPtr` 1) xs (acc `plusExtra` x) -- less space than pokeElemOff

#else /* hack away */

pack str = unsafeCreate' (P.length str) $ \(Ptr p) -> stToIO (go p 0# str 0#)
    where
        go _ _ [] acc       = return (I# acc)
        go p i (W8# c:cs) acc = do
            writeByte p i c
            go p (i +# 1#) cs (acc +# (if geWord# c (int2Word# 0x80#) && ltWord# c (int2Word# 0xC0#) then 1# else 0#))

        writeByte p i c = ST $ \s# ->
            case writeWord8OffAddr# p i c s# of s2# -> (# s2#, () #)

#endif

-- | /O(n)/ Converts a 'Buffer' to a '[Word8]'.
unpack :: Buffer -> [Word8]

#if !defined(__GLASGOW_HASKELL__)

unpack (PS _  _ 0 _) = []
unpack (PS ps s l _) = inlinePerformIO $ withForeignPtr ps $ \p ->
        go (p `plusPtr` s) (l - 1) []
    where
        STRICT3(go)
        go p 0 acc = peek p          >>= \e -> return (e : acc)
        go p n acc = peekByteOff p n >>= \e -> go p (n-1) (e : acc)
{-# INLINE unpack #-}

#else

unpack xs = build (unpackFoldr xs)
{-# INLINE unpack #-}

--
-- Have unpack fuse with good list consumers
--
-- critical this isn't strict in the acc
-- as it will break in the presence of list fusion. this is a known
-- issue with seq and build/foldr rewrite rules, which rely on lazy
-- demanding to avoid bottoms in the list.
--
unpackFoldr :: Buffer -> (Word8 -> a -> a) -> a -> a
unpackFoldr (PS fp off len _) f ch = withPtr fp $ \p -> do
    let loop q n    _   | q `seq` n `seq` False = undefined -- n.b.
        loop _ (-1) acc = return acc
        loop q n    acc = do
           a <- peekByteOff q n
           loop q (n-1) (a `f` acc)
    loop (p `plusPtr` off) (len-1) ch
{-# INLINE [0] unpackFoldr #-}

unpackList :: Buffer -> [Word8]
unpackList (PS fp off len _) = withPtr fp $ \p -> do
    let STRICT3(loop)
        loop _ (-1) acc = return acc
        loop q n acc = do
           a <- peekByteOff q n
           loop q (n-1) (a : acc)
    loop (p `plusPtr` off) (len-1) []

{-# RULES
"Buffer unpack-list" [1]  forall p  .
    unpackFoldr p (:) [] = unpackList p
 #-}

#endif

-- ---------------------------------------------------------------------
-- Basic interface

-- | /O(1)/ Test whether a Buffer is empty.
null :: Buffer -> Bool
null (PS _ _ l _) = assert (l >= 0) $ l <= 0
{-# INLINE null #-}

-- ---------------------------------------------------------------------
-- | /O(1)/ 'length' returns the length of a Buffer as an 'Int'.
length :: Buffer -> Int
length (PS _ _ l _) = assert (l >= 0) $ l
{-# INLINE length #-}

------------------------------------------------------------------------

-- | /O(n)/ 'cons' is analogous to (:) for lists, but of different
-- complexity, as it requires a memcpy.
cons :: Word8 -> Buffer -> Buffer
cons c (PS x s l e) = unsafeCreate' (l+1) $ \p -> withForeignPtr x $ \f -> do
        poke p c
        memcpy (p `plusPtr` 1) (f `plusPtr` s) (fromIntegral l)
        return $! if c >= 0x80 && c < 0xC0 then e + 1 else e
{-# INLINE cons #-}

-- | /O(n)/ Append a byte to the end of a 'Buffer'
snoc :: Buffer -> Word8 -> Buffer
snoc (PS x s l e) c = unsafeCreate' (l+1) $ \p -> withForeignPtr x $ \f -> do
        memcpy p (f `plusPtr` s) (fromIntegral l)
        poke (p `plusPtr` l) c
        return $! if c >= 0x80 && c < 0xC0 then e + 1 else e
{-# INLINE snoc #-}

-- todo fuse

-- | /O(1)/ Extract the first element of a Buffer, which must be non-empty.
-- An exception will be thrown in the case of an empty Buffer.
head :: Buffer -> Word8
head (PS x s l _)
    | l <= 0    = errorEmptyList "head"
    | otherwise = inlinePerformIO $ withForeignPtr x $ \p -> peekByteOff p s
{-# INLINE head #-}

-- | /O(1)/ Extract the elements after the head of a Buffer, which must be non-empty.
-- An exception will be thrown in the case of an empty Buffer.
tail :: Buffer -> Buffer
tail (PS x s l e)
    | l <= 0    = errorEmptyList "tail"
    | otherwise = PS x (s+1) (l-1) $! minusExtra e $ inlinePerformIO $ withForeignPtr x $ \p -> peekByteOff p s
{-# INLINE tail #-}

-- | /O(1)/ Extract the head and tail of a Buffer, returning Nothing
-- if it is empty.
uncons :: Buffer -> Maybe (Word8, Buffer)
uncons (PS x s l e)
    | l <= 0    = Nothing   
    | otherwise = let h = inlinePerformIO $ withForeignPtr x $ \p -> peekByteOff p s
                  in Just (h, PS x (s+1) (l-1) $! e `minusExtra` h)
{-# INLINE uncons #-}

-- | /O(1)/ Extract the head and tail of a Buffer, returning Nothing
-- if it is empty.
unsnoc :: Buffer -> Maybe (Buffer, Word8)
unsnoc (PS x s l e)
    | l <= 0    = Nothing   
    | otherwise = let z = inlinePerformIO $ withForeignPtr x $ \p -> peekByteOff p (s+l-1)
                  in Just (PS x s (l-1) $! e `minusExtra` z, z)
{-# INLINE unsnoc #-}


-- | /O(1)/ Extract the last element of a Buffer, which must be finite and non-empty.
-- An exception will be thrown in the case of an empty Buffer.
last :: Buffer -> Word8
last (PS x s l _)
    | l <= 0   = errorEmptyList "last"
    | otherwise = inlinePerformIO $ withForeignPtr x $ \p -> peekByteOff p (s+l-1)
{-# INLINE last #-}

-- | /O(1)/ Return all the elements of a 'Buffer' except the last one.
-- An exception will be thrown in the case of an empty Buffer.
init :: Buffer -> Buffer
init (PS x s l e)
    | l <= 0 = errorEmptyList "init"
    | otherwise = PS x s (l-1) $! minusExtra e $ inlinePerformIO $ withForeignPtr x $ \p -> peekByteOff p (s+l-1)
{-# INLINE init #-}

-- | /O(n)/ Append two Buffers
append :: Buffer -> Buffer -> Buffer
append xs ys | null xs   = ys
             | null ys   = xs
             | otherwise = concat [xs,ys]
{-# INLINE append #-}

-- ---------------------------------------------------------------------
-- Transformations

-- EAK: is inlinePerformIO correct here? create' _does_ perform allocation

-- | /O(n)/ 'map' @f xs@ is the Buffer obtained by applying @f@ to each
-- element of @xs@. This function is subject to array fusion.
map :: (Word8 -> Word8) -> Buffer -> Buffer
map f (PS fp s len _) = inlinePerformIO $ withForeignPtr fp $ \a ->
    create' len $ map_ 0 (a `plusPtr` s) 0
  where
    map_ :: Int -> Ptr Word8 -> Int -> Ptr Word8 -> IO Int
    STRICT4(map_)
    map_ n p1 acc p2 
       | n >= len = return acc
       | otherwise = do
            x <- peekByteOff p1 n
            let x' = f x
            pokeByteOff p2 n x'
            map_ (n+1) p1 (acc `plusExtra` x') p2 
            
{-# INLINE map #-}

-- | /O(n)/ 'reverse' @xs@ efficiently returns the elements of @xs@ in reverse order.
reverse :: Buffer -> Buffer
reverse (PS x s l e) = unsafeCreate' l $ \p -> withForeignPtr x $ \f -> do
        c_reverse p (f `plusPtr` s) (fromIntegral l)
        return e

-- | /O(n)/ The 'intersperse' function takes a 'Word8' and a
-- 'Buffer' and \`intersperses\' that byte between the elements of
-- the 'Buffer'.  It is analogous to the intersperse function on
-- Lists.
intersperse :: Word8 -> Buffer -> Buffer
intersperse c xs@(PS x s l e)
    | length xs < 2  = xs
    | otherwise      = unsafeCreate' (2*l-1) $ \p -> withForeignPtr x $ \f -> do
        c_intersperse p (f `plusPtr` s) (fromIntegral l) c
        return $! e + extra c * (l-1)

-- | The 'transpose' function transposes the rows and columns of its
-- 'Buffer' argument.
transpose :: [Buffer] -> [Buffer]
transpose xs = P.map pack (List.transpose (P.map unpack xs))

-- ---------------------------------------------------------------------
-- Reducing 'Buffer's

-- | 'foldl', applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and a Buffer, reduces the
-- Buffer using the binary operator, from left to right.
--
-- This function is subject to array fusion.
--
foldl :: (a -> Word8 -> a) -> a -> Buffer -> a
foldl f v (PS x s l _) = inlinePerformIO $ withForeignPtr x $ \ptr ->
        lgo v (ptr `plusPtr` s) (ptr `plusPtr` (s+l))
    where
        STRICT3(lgo)
        lgo z p q | p == q    = return z
                  | otherwise = do c <- peek p
                                   lgo (f z c) (p `plusPtr` 1) q
{-# INLINE foldl #-}

-- | 'foldl\'' is like 'foldl', but strict in the accumulator.
-- However, for Buffers, all left folds are strict in the accumulator.
--
foldl' :: (a -> Word8 -> a) -> a -> Buffer -> a
foldl' = foldl
{-# INLINE foldl' #-}

-- | 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a Buffer,
-- reduces the Buffer using the binary operator, from right to left.
foldr :: (Word8 -> a -> a) -> a -> Buffer -> a
foldr k v (PS x s l _) = inlinePerformIO $ withForeignPtr x $ \ptr ->
        go v (ptr `plusPtr` (s+l-1)) (ptr `plusPtr` (s-1))
    where
        STRICT3(go)
        go z p q | p == q    = return z
                 | otherwise = do c  <- peek p
                                  go (c `k` z) (p `plusPtr` (-1)) q -- tail recursive
{-# INLINE foldr #-}

-- | 'foldr\'' is like 'foldr', but strict in the accumulator.
foldr' :: (Word8 -> a -> a) -> a -> Buffer -> a
foldr' k v (PS x s l _) = inlinePerformIO $ withForeignPtr x $ \ptr ->
        go v (ptr `plusPtr` (s+l-1)) (ptr `plusPtr` (s-1))
    where
        STRICT3(go)
        go z p q | p == q    = return z
                 | otherwise = do c  <- peek p
                                  go (c `k` z) (p `plusPtr` (-1)) q -- tail recursive
{-# INLINE foldr' #-}

-- | 'foldl1' is a variant of 'foldl' that has no starting value
-- argument, and thus must be applied to non-empty 'Buffers'.
-- This function is subject to array fusion. 
-- An exception will be thrown in the case of an empty Buffer.
foldl1 :: (Word8 -> Word8 -> Word8) -> Buffer -> Word8
foldl1 f xs
    | null xs   = errorEmptyList "foldl1"
    | otherwise = foldl f (unsafeHead xs) (unsafeTail xs)
{-# INLINE foldl1 #-}

-- | 'foldl1\'' is like 'foldl1', but strict in the accumulator.
-- An exception will be thrown in the case of an empty Buffer.
foldl1' :: (Word8 -> Word8 -> Word8) -> Buffer -> Word8
foldl1' f xs
    | null xs   = errorEmptyList "foldl1'"
    | otherwise = foldl' f (unsafeHead xs) (unsafeTail xs)
{-# INLINE foldl1' #-}

-- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- and thus must be applied to non-empty 'Buffer's
-- An exception will be thrown in the case of an empty Buffer.
foldr1 :: (Word8 -> Word8 -> Word8) -> Buffer -> Word8
foldr1 f xs
    | null xs        = errorEmptyList "foldr1"
    | otherwise      = foldr f (unsafeLast xs) (unsafeInit xs)
{-# INLINE foldr1 #-}

-- | 'foldr1\'' is a variant of 'foldr1', but is strict in the
-- accumulator.
foldr1' :: (Word8 -> Word8 -> Word8) -> Buffer -> Word8
foldr1' f xs
    | null xs        = errorEmptyList "foldr1"
    | otherwise      = foldr' f (unsafeLast xs) (unsafeInit xs)
{-# INLINE foldr1' #-}

-- ---------------------------------------------------------------------
-- Special folds

-- | /O(n)/ Concatenate a list of Buffers.
concat :: [Buffer] -> Buffer
concat []     = empty
concat [xs]   = xs
concat xs     = unsafeCreate' len $ \ptr -> go xs ptr 0
  where len = P.sum . P.map length $ xs
        STRICT3(go)
        go []               _   acc  = return acc
        go (PS p s l e: xs') ptr acc = do
                withForeignPtr p $ \fp -> memcpy ptr (fp `plusPtr` s) (fromIntegral l)
                go xs' (ptr `plusPtr` l) (acc + e)

-- | Map a function over a 'Buffer' and concatenate the results
concatMap :: (Word8 -> Buffer) -> Buffer -> Buffer
concatMap f = concat . foldr ((:) . f) []

-- foldr (append . f) empty

-- | /O(n)/ Applied to a predicate and a Buffer, 'any' determines if
-- any element of the 'Buffer' satisfies the predicate.
any :: (Word8 -> Bool) -> Buffer -> Bool
any _ (PS _ _ 0 _) = False
any f (PS x s l _) = inlinePerformIO $ withForeignPtr x $ \ptr ->
        go (ptr `plusPtr` s) (ptr `plusPtr` (s+l))
    where
        STRICT2(go)
        go p q | p == q    = return False
               | otherwise = do c <- peek p
                                if f c then return True
                                       else go (p `plusPtr` 1) q
{-# INLINE any #-}

-- todo fuse

-- | /O(n)/ Applied to a predicate and a 'Buffer', 'all' determines
-- if all elements of the 'Buffer' satisfy the predicate.
all :: (Word8 -> Bool) -> Buffer -> Bool
all _ (PS _ _ 0 _) = True
all f (PS x s l _) = inlinePerformIO $ withForeignPtr x $ \ptr ->
        go (ptr `plusPtr` s) (ptr `plusPtr` (s+l))
    where
        STRICT2(go)
        go p q | p == q     = return True  -- end of list
               | otherwise  = do c <- peek p
                                 if f c
                                    then go (p `plusPtr` 1) q
                                    else return False
{-# INLINE all #-}

------------------------------------------------------------------------

-- | /O(n)/ 'maximum' returns the maximum value from a 'Buffer'
-- This function will fuse.
-- An exception will be thrown in the case of an empty Buffer.
maximum :: Buffer -> Word8
maximum xs@(PS x s l _)
    | null xs   = errorEmptyList "maximum"
    | otherwise = inlinePerformIO $ withForeignPtr x $ \p ->
                      c_maximum (p `plusPtr` s) (fromIntegral l)
{-# INLINE maximum #-}

-- | /O(n)/ 'minimum' returns the minimum value from a 'Buffer'
-- This function will fuse.
-- An exception will be thrown in the case of an empty Buffer.
minimum :: Buffer -> Word8
minimum xs@(PS x s l _)
    | null xs   = errorEmptyList "minimum"
    | otherwise = inlinePerformIO $ withForeignPtr x $ \p ->
                      c_minimum (p `plusPtr` s) (fromIntegral l)
{-# INLINE minimum #-}

------------------------------------------------------------------------

-- | The 'mapAccumL' function behaves like a combination of 'map' and
-- 'foldl'; it applies a function to each element of a Buffer,
-- passing an accumulating parameter from left to right, and returning a
-- final value of this accumulator together with the new list.
mapAccumL :: (acc -> Word8 -> (acc, Word8)) -> acc -> Buffer -> (acc, Buffer)
mapAccumL f acc (PS fp o len _) = inlinePerformIO $ withForeignPtr fp $ \a -> do
    gp   <- mallocBuffer len
    (acc', e) <- withForeignPtr gp $ \p -> mapAccumL_ acc 0 (a `plusPtr` o) p 0
    return $! (acc', PS gp 0 len e)
  where
    STRICT5(mapAccumL_)
    mapAccumL_ s n p1 p2 e
       | n >= len = return (s, e)
       | otherwise = do
            x <- peekByteOff p1 n
            let (s', y) = f s x
            pokeByteOff p2 n y
            mapAccumL_ s' (n+1) p1 p2 (e `plusExtra` y)
{-# INLINE mapAccumL #-}

-- | The 'mapAccumR' function behaves like a combination of 'map' and
-- 'foldr'; it applies a function to each element of a Buffer,
-- passing an accumulating parameter from right to left, and returning a
-- final value of this accumulator together with the new Buffer.
mapAccumR :: (acc -> Word8 -> (acc, Word8)) -> acc -> Buffer -> (acc, Buffer)
mapAccumR f acc (PS fp o len _) = inlinePerformIO $ withForeignPtr fp $ \a -> do
    gp   <- mallocBuffer len
    (acc', e) <- withForeignPtr gp $ \p -> mapAccumR_ acc (len-1) (a `plusPtr` o) p 0
    return $! (acc', PS gp 0 len e)
  where
    STRICT5(mapAccumR_)
    mapAccumR_ s n p q e
       | n <  0    = return (s, e)
       | otherwise = do
            x  <- peekByteOff p n
            let (s', y) = f s x
            pokeByteOff q n y
            mapAccumR_ s' (n-1) p q (e `plusExtra` y)
{-# INLINE mapAccumR #-}

-- ---------------------------------------------------------------------
-- Building Buffers

-- | 'scanl' is similar to 'foldl', but returns a list of successive
-- reduced values from the left. This function will fuse.
--
-- > scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
--
-- Note that
--
-- > last (scanl f z xs) == foldl f z xs.
--
scanl :: (Word8 -> Word8 -> Word8) -> Word8 -> Buffer -> Buffer
scanl f v (PS fp s len _) = inlinePerformIO $ withForeignPtr fp $ \a ->
    create' (len+1) $ \q -> do
        poke q v
        scanl_ v 0 (a `plusPtr` s) (q `plusPtr` 1) (extra v)
  where
    STRICT5(scanl_)
    scanl_ z n p q e
        | n >= len  = return e
        | otherwise = do
            x <- peekByteOff p n
            let z' = f z x
            pokeByteOff q n z'
            scanl_ z' (n+1) p q (e `plusExtra` z')
{-# INLINE scanl #-}

    -- n.b. haskell's List scan returns a list one bigger than the
    -- input, so we need to snoc here to get some extra space, however,
    -- it breaks map/up fusion (i.e. scanl . map no longer fuses)

-- | 'scanl1' is a variant of 'scanl' that has no starting value argument.
-- This function will fuse.
--
-- > scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]
scanl1 :: (Word8 -> Word8 -> Word8) -> Buffer -> Buffer
scanl1 f xs
    | null xs   = empty
    | otherwise = scanl f (unsafeHead xs) (unsafeTail xs)
{-# INLINE scanl1 #-}

-- | scanr is the right-to-left dual of scanl.
scanr :: (Word8 -> Word8 -> Word8) -> Word8 -> Buffer -> Buffer
scanr f v (PS fp s len _) = inlinePerformIO $ withForeignPtr fp $ \a ->
    create' (len+1) $ \q -> do
        poke (q `plusPtr` len) v
        scanr_ v (len-1) (a `plusPtr` s) q (extra v)
  where
    STRICT5(scanr_)
    scanr_ z n p q e
        | n <  0    = return e
        | otherwise = do
            x <- peekByteOff p n
            let z' = f x z
            pokeByteOff q n z'
            scanr_ z' (n-1) p q (e `plusExtra` z')
{-# INLINE scanr #-}

-- | 'scanr1' is a variant of 'scanr' that has no starting value argument.
scanr1 :: (Word8 -> Word8 -> Word8) -> Buffer -> Buffer
scanr1 f xs
    | null xs   = empty
    | otherwise = scanr f (unsafeLast xs) (unsafeInit xs) 
{-# INLINE scanr1 #-}

-- ---------------------------------------------------------------------
-- Unfolds and replicates

-- | /O(n)/ 'replicate' @n x@ is a Buffer of length @n@ with @x@
-- the value of every element. The following holds:
--
-- > replicate w c = unfoldr w (\u -> Just (u,u)) c
--
-- This implemenation uses @memset(3)@
replicate :: Int -> Word8 -> Buffer
replicate w c
    | w <= 0    = empty
    | otherwise = unsafeCreate w $ \ptr ->
                      memset ptr c (fromIntegral w) >> return ()

-- | /O(n)/, where /n/ is the length of the result.  The 'unfoldr' 
-- function is analogous to the List \'unfoldr\'.  'unfoldr' builds a 
-- Buffer from a seed value.  The function takes the element and 
-- returns 'Nothing' if it is done producing the Buffer or returns 
-- 'Just' @(a,b)@, in which case, @a@ is the next byte in the string, 
-- and @b@ is the seed value for further production.
--
-- Examples:
--
-- >    unfoldr (\x -> if x <= 5 then Just (x, x + 1) else Nothing) 0
-- > == pack [0, 1, 2, 3, 4, 5]
--
unfoldr :: (a -> Maybe (Word8, a)) -> a -> Buffer
unfoldr f = concat . unfoldChunk 32 64
  where unfoldChunk n n' x =
          case unfoldrN n f x of
            (s, Nothing) -> s : []
            (s, Just x') -> s : unfoldChunk n' (n+n') x'
{-# INLINE unfoldr #-}

-- | /O(n)/ Like 'unfoldr', 'unfoldrN' builds a Buffer from a seed
-- value.  However, the length of the result is limited by the first
-- argument to 'unfoldrN'.  This function is more efficient than 'unfoldr'
-- when the maximum length of the result is known.
--
-- The following equation relates 'unfoldrN' and 'unfoldr':
--
-- > snd (unfoldrN n f s) == take n (unfoldr f s)
--
unfoldrN :: Int -> (a -> Maybe (Word8, a)) -> a -> (Buffer, Maybe a)
unfoldrN i f x0
    | i < 0     = (empty, Just x0)
    | otherwise = unsafePerformIO $ createAndTrim' i $ \p -> go p x0 0
  where STRICT3(go)
        go p x n =
          case f x of
            Nothing      -> return (0, n, Nothing)
            Just (w,x')
             | n == i    -> return (0, n, Just x)
             | otherwise -> do poke p w
                               go (p `plusPtr` 1) x' (n+1)
{-# INLINE unfoldrN #-}

-- ---------------------------------------------------------------------
-- Substrings

-- | /O(1)/ 'take' @n@, applied to a Buffer @xs@, returns the prefix
-- of @xs@ of length @n@, or @xs@ itself if @n > 'length' xs@.
take :: Int -> Buffer -> Buffer
take n xs@(PS _ _ l _)
    | n <= 0    = empty
    | n >= l    = xs
    | otherwise = unsafeTake n xs
{-# INLINE take #-}

-- | /O(1)/ 'drop' @n xs@ returns the suffix of @xs@ after the first @n@
-- elements, or @[]@ if @n > 'length' xs@.
drop  :: Int -> Buffer -> Buffer
drop n xs@(PS _ _ l _)
    | n <= 0    = xs
    | n >= l    = empty
    | otherwise = unsafeDrop n xs
{-# INLINE drop #-}

-- | /O(1)/ 'splitAt' @n xs@ is equivalent to @('take' n xs, 'drop' n xs)@.
splitAt :: Int -> Buffer -> (Buffer, Buffer)
splitAt n xs@(PS _ _ l _)
    | n <= 0    = (empty, xs)
    | n >= l    = (xs, empty)
    | otherwise = unsafeSplitAt n xs
{-# INLINE splitAt #-}

-- | 'takeWhile', applied to a predicate @p@ and a Buffer @xs@,
-- returns the longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@.
takeWhile :: (Word8 -> Bool) -> Buffer -> Buffer
takeWhile f xs = unsafeTake (findIndexOrEnd (not . f) xs) xs
{-# INLINE takeWhile #-}

-- | 'dropWhile' @p xs@ returns the suffix remaining after 'takeWhile' @p xs@.
dropWhile :: (Word8 -> Bool) -> Buffer -> Buffer
dropWhile f xs = unsafeDrop (findIndexOrEnd (not . f) xs) xs
{-# INLINE dropWhile #-}

-- instead of findIndexOrEnd, we could use memchr here.

-- | 'break' @p@ is equivalent to @'span' ('not' . p)@.
--
-- Under GHC, a rewrite rule will transform break (==) into a
-- call to the specialised breakByte:
--
-- > break ((==) x) = breakByte x
-- > break (==x) = breakByte x
--
break :: (Word8 -> Bool) -> Buffer -> (Buffer, Buffer)
break p xs = unsafeSplitAt (findIndexOrEnd p xs) xs
#if __GLASGOW_HASKELL__ 
{-# INLINE [1] break #-}
#endif

#if __GLASGOW_HASKELL__ >= 606
-- This RULE LHS is not allowed by ghc-6.4
{-# RULES
"Buffer specialise break (x==)" forall x.
    break ((==) x) = breakByte x
"Buffer specialise break (==x)" forall x.
    break (==x) = breakByte x
  #-}
#endif

-- INTERNAL:

-- | 'breakByte' breaks its Buffer argument at the first occurence
-- of the specified byte. It is more efficient than 'break' as it is
-- implemented with @memchr(3)@. I.e.
-- 
-- > break (=='c') "abcd" == breakByte 'c' "abcd"
--
breakByte :: Word8 -> Buffer -> (Buffer, Buffer)
breakByte c p = case elemIndex c p of
    Nothing -> (p, empty)
    Just n  -> unsafeSplitAt n p
{-# INLINE breakByte #-}

-- | 'breakEnd' behaves like 'break' but from the end of the 'Buffer'
-- 
-- breakEnd p == spanEnd (not.p)
breakEnd :: (Word8 -> Bool) -> Buffer -> (Buffer, Buffer)
breakEnd  p xs = splitAt (findFromEndUntil p xs) xs

-- | 'span' @p xs@ breaks the Buffer into two segments. It is
-- equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@
span :: (Word8 -> Bool) -> Buffer -> (Buffer, Buffer)
span p xs = break (not . p) xs
#if __GLASGOW_HASKELL__
{-# INLINE [1] span #-}
#endif

-- | 'spanByte' breaks its Buffer argument at the first
-- occurence of a byte other than its argument. It is more efficient
-- than 'span (==)'
--
-- > span  (=='c') "abcd" == spanByte 'c' "abcd"
--
--
-- TODO: replace use of unsafeSplitAt, to avoid traversing the span twice, we know the count of tailbytes
spanByte :: Word8 -> Buffer -> (Buffer, Buffer)
spanByte c xs@(PS x s l e)
    | ix == 0   = (xs, empty)
    | ix == l   = (empty, xs)
    | otherwise = (PS x s ix e', PS x (s + ix) (l - ix) (e - e'))
  where
    ix = inlinePerformIO $ withForeignPtr x $ \p -> go (p `plusPtr` s) 0
    e' = extra c * ix
    STRICT2(go)
    go p i | i >= l    = return l
           | otherwise = do c' <- peekByteOff p i
                            if c /= c'
                                then return i
                                else go p (i+1)
{-# INLINE spanByte #-}

#if __GLASGOW_HASKELL__ >= 606
-- This RULE LHS is not allowed by ghc-6.4
{-# RULES
"Buffer specialise span (x==)" forall x.
    span ((==) x) = spanByte x
"Buffer specialise span (==x)" forall x.
    span (==x) = spanByte x
  #-}
#endif

-- | 'spanEnd' behaves like 'span' but from the end of the 'Buffer'.
-- We have
--
-- > spanEnd (not.isSpace) "x y z" == ("x y ","z")
--
-- and
--
-- > spanEnd (not . isSpace) ps
-- >    == 
-- > let (x,y) = span (not.isSpace) (reverse ps) in (reverse y, reverse x) 
--
spanEnd :: (Word8 -> Bool) -> Buffer -> (Buffer, Buffer)
spanEnd p xs = splitAt (findFromEndUntil (not.p) xs) xs

-- | /O(n)/ Splits a 'Buffer' into components delimited by
-- separators, where the predicate returns True for a separator element.
-- The resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > splitWith (=='a') "aabbaca" == ["","","bb","c",""]
-- > splitWith (=='a') []        == []
--
splitWith :: (Word8 -> Bool) -> Buffer -> [Buffer]

#if defined(__GLASGOW_HASKELL__)
splitWith _pred (PS _  _   0 _) = []
splitWith pred_ (PS fp off len 0) = splitWith0 pred# off len fp
  where pred# c# = pred_ (W8# c#)

        STRICT4(splitWith0)
        splitWith0 pred' off' len' fp' = withPtr fp $ \p ->
            splitLoop pred' p 0 off' len' fp'

        splitLoop :: (Word# -> Bool)
                  -> Ptr Word8
                  -> Int -> Int -> Int
                  -> ForeignPtr Word8
                  -> IO [Buffer]

        splitLoop pred' p idx' off' len' fp'
            | idx' >= len'  = return [PS fp' off' idx' 0]
            | otherwise = do
                w <- peekElemOff p (off'+idx')
                if pred' (case w of W8# w# -> w#)
                   then return (PS fp' off' idx' 0 :
                              splitWith0 pred' (off'+idx'+1) (len'-idx'-1) fp')
                   else splitLoop pred' p (idx'+1) off' len' fp'
splitWith pred_ (PS fp off len _) = splitWith0 pred# off len fp
  where pred# c# = pred_ (W8# c#)

        STRICT4(splitWith0)
        splitWith0 pred' off' len' fp' = withPtr fp $ \p ->
            splitLoop pred' p 0 off' len' fp'

        splitLoop :: (Word# -> Bool)
                  -> Ptr Word8
                  -> Int -> Int -> Int
                  -> ForeignPtr Word8
                  -> IO [Buffer]

        splitLoop pred' p idx' off' len' fp'
            | idx' >= len'  = return [ps fp' off' idx']
            | otherwise = do
                w <- peekElemOff p (off'+idx')
                if pred' (case w of W8# w# -> w#)
                   then return (ps fp' off' idx' :
                              splitWith0 pred' (off'+idx'+1) (len'-idx'-1) fp')
                   else splitLoop pred' p (idx'+1) off' len' fp'
{-# INLINE splitWith #-}

#else
splitWith _ (PS _ _ 0) = []
splitWith p ps = loop p ps
    where
        STRICT2(loop)
        loop q qs = if null rest then [chunk]
                                 else chunk : loop q (unsafeTail rest)
            where (chunk,rest) = break q qs
#endif

-- | /O(n)/ Break a 'Buffer' into pieces separated by the byte
-- argument, consuming the delimiter. I.e.
--
-- > split '\n' "a\nb\nd\ne" == ["a","b","d","e"]
-- > split 'a'  "aXaXaXa"    == ["","X","X","X",""]
-- > split 'x'  "x"          == ["",""]
-- 
-- and
--
-- > intercalate [c] . split c == id
-- > split == splitWith . (==)
-- 
-- As for all splitting functions in this library, this function does
-- not copy the substrings, it just constructs new 'Buffers' that
-- are slices of the original.
--
split :: Word8 -> Buffer -> [Buffer]
split _ (PS _ _ 0 _) = []
split w (PS x s l 0) 
    | w >= 0x80 && w < 0xc0 = [] 
    | otherwise = loop 0
    where
        STRICT1(loop)
        loop n =
            let q = inlinePerformIO $ withForeignPtr x $ \p ->
                      memchr (p `plusPtr` (s+n))
                             w (fromIntegral (l-n))
            in if q == nullPtr
                then [PS x (s+n) (l-n) 0]
                else let i = inlinePerformIO $ withForeignPtr x $ \p ->
                               return (q `minusPtr` (p `plusPtr` s))
                      in PS x (s+n) (i-n) 0 : loop (i+1)
split w (PS x s l _) = loop 0
    where
        STRICT1(loop)
        loop n =
            let q = inlinePerformIO $ withForeignPtr x $ \p ->
                      memchr (p `plusPtr` (s+n))
                             w (fromIntegral (l-n))
            in if q == nullPtr
                then [ps x (s+n) (l-n)]
                else let i = inlinePerformIO $ withForeignPtr x $ \p ->
                               return (q `minusPtr` (p `plusPtr` s))
                      in ps x (s+n) (i-n) : loop (i+1)

{-# INLINE split #-}

{-
-- slower. but stays inside Haskell.
split _ (PS _  _   0) = []
split (W8# w#) (PS fp off len) = splitWith' off len fp
    where
        splitWith' off' len' fp' = withPtr fp $ \p ->
            splitLoop p 0 off' len' fp'

        splitLoop :: Ptr Word8
                  -> Int -> Int -> Int
                  -> ForeignPtr Word8
                  -> IO [Buffer]

        STRICT5(splitLoop)
        splitLoop p idx' off' len' fp'
            | idx' >= len'  = return [PS fp' off' idx']
            | otherwise = do
                (W8# x#) <- peekElemOff p (off'+idx')
                if word2Int# w# ==# word2Int# x#
                   then return (PS fp' off' idx' :
                              splitWith' (off'+idx'+1) (len'-idx'-1) fp')
                   else splitLoop p (idx'+1) off' len' fp'
-}

{-
-- | Like 'splitWith', except that sequences of adjacent separators are
-- treated as a single separator. eg.
-- 
-- > tokens (=='a') "aabbaca" == ["bb","c"]
--
tokens :: (Word8 -> Bool) -> Buffer -> [Buffer]
tokens f = P.filter (not.null) . splitWith f
{-# INLINE tokens #-}
-}

-- | The 'group' function takes a Buffer and returns a list of
-- Buffers such that the concatenation of the result is equal to the
-- argument.  Moreover, each sublist in the result contains only equal
-- elements.  For example,
--
-- > group "Mississippi" = ["M","i","ss","i","ss","i","pp","i"]
--
-- It is a special case of 'groupBy', which allows the programmer to
-- supply their own equality test. It is about 40% faster than 
-- /groupBy (==)/
group :: Buffer -> [Buffer]
group xs
    | null xs   = []
    | otherwise = ys : group zs
    where
        (ys, zs) = spanByte (unsafeHead xs) xs

-- | The 'groupBy' function is the non-overloaded version of 'group'.
groupBy :: (Word8 -> Word8 -> Bool) -> Buffer -> [Buffer]
groupBy k xs
    | null xs   = []
    | otherwise = unsafeTake n xs : groupBy k (unsafeDrop n xs)
    where
        n = 1 + findIndexOrEnd (not . k (unsafeHead xs)) (unsafeTail xs)

-- | /O(n)/ The 'intercalate' function takes a 'Buffer' and a list of
-- 'Buffer's and concatenates the list after interspersing the first
-- argument between each element of the list.
intercalate :: Buffer -> [Buffer] -> Buffer
intercalate s = concat . (List.intersperse s)
{-# INLINE [1] intercalate #-}

{-# RULES
"Buffer specialise intercalate c -> intercalateByte" forall c s1 s2 .
    intercalate (singleton c) (s1 : s2 : []) = intercalateWithByte c s1 s2
  #-}

-- | /O(n)/ intercalateWithByte. An efficient way to join to two Buffers
-- with a char. Around 4 times faster than the generalised join.
--
intercalateWithByte :: Word8 -> Buffer -> Buffer -> Buffer
intercalateWithByte c f@(PS ffp s l n) g@(PS fgp t m o) = unsafeCreate' len $ \ptr ->
    withForeignPtr ffp $ \fp ->
    withForeignPtr fgp $ \gp -> do
        memcpy ptr (fp `plusPtr` s) (fromIntegral l)
        poke (ptr `plusPtr` l) c
        memcpy (ptr `plusPtr` (l + 1)) (gp `plusPtr` t) (fromIntegral m)
        return $! (n + o) `plusExtra` c
    where
      len = length f + length g + 1
{-# INLINE intercalateWithByte #-}

-- ---------------------------------------------------------------------
-- Indexing Buffers

-- | /O(1)/ 'Buffer' index (subscript) operator, starting from 0.
index :: Buffer -> Int -> Word8
index xs n
    | n < 0          = moduleError "index" ("negative index: " ++ show n)
    | n >= length xs = moduleError "index" ("index too large: " ++ show n
                                         ++ ", length = " ++ show (length xs))
    | otherwise      = xs `unsafeIndex` n
{-# INLINE index #-}

-- | /O(n)/ The 'elemIndex' function returns the index of the first
-- element in the given 'Buffer' which is equal to the query
-- element, or 'Nothing' if there is no such element. 
-- This implementation uses memchr(3).
elemIndex :: Word8 -> Buffer -> Maybe Int
elemIndex c (PS x s l e) 
   | e == 0 && c >= 0x80 && c < 0xC0 = Nothing
   | otherwise = inlinePerformIO $ withForeignPtr x $ \p -> do
    let p' = p `plusPtr` s
    q <- memchr p' c (fromIntegral l)
    return $! if q == nullPtr then Nothing else Just $! q `minusPtr` p'
{-# INLINE elemIndex #-}

-- | /O(n)/ The 'elemIndexEnd' function returns the last index of the
-- element in the given 'Buffer' which is equal to the query
-- element, or 'Nothing' if there is no such element. The following
-- holds:
--
-- > elemIndexEnd c xs == 
-- > (-) (length xs - 1) `fmap` elemIndex c (reverse xs)
--
elemIndexEnd :: Word8 -> Buffer -> Maybe Int
elemIndexEnd ch (PS x s l e) 
   | e == 0 && ch >= 0x80 && ch < 0xC0 = Nothing
   | otherwise = inlinePerformIO $ withForeignPtr x $ \p -> go (p `plusPtr` s) (l-1)
  where
    STRICT2(go)
    go p i | i < 0     = return Nothing
           | otherwise = do ch' <- peekByteOff p i
                            if ch == ch'
                                then return $ Just i
                                else go p (i-1)
{-# INLINE elemIndexEnd #-}

-- | /O(n)/ The 'elemIndices' function extends 'elemIndex', by returning
-- the indices of all elements equal to the query element, in ascending order.
-- This implementation uses memchr(3).
elemIndices :: Word8 -> Buffer -> [Int]
elemIndices w (PS x s l e) 
   | e == 0 && w >= 0x80 && w < 0xC0 = []
   | otherwise = loop 0
    where
        STRICT1(loop)
        loop n = let q = inlinePerformIO $ withForeignPtr x $ \p ->
                           memchr (p `plusPtr` (n+s))
                                                w (fromIntegral (l - n))
                 in if q == nullPtr
                        then []
                        else let i = inlinePerformIO $ withForeignPtr x $ \p ->
                                       return (q `minusPtr` (p `plusPtr` s))
                             in i : loop (i+1)
{-# INLINE elemIndices #-}

{-
-- much slower
elemIndices :: Word8 -> Buffer -> [Int]
elemIndices c ps = loop 0 ps
   where STRICT2(loop)
         loop _ ps' | null ps'            = []
         loop n ps' | c == unsafeHead ps' = n : loop (n+1) (unsafeTail ps')
                    | otherwise           = loop (n+1) (unsafeTail ps')
-}

-- | count returns the number of times its argument appears in the Buffer
--
-- > count = length . elemIndices
--
-- But more efficiently than using length on the intermediate list.
count :: Word8 -> Buffer -> Int
count w (PS x s m e) 
    | e == 0 && w >= 0x80 && w < 0xC0 = 0 
    | otherwise = inlinePerformIO $ withForeignPtr x $ \p ->
        fmap fromIntegral $ c_count (p `plusPtr` s) (fromIntegral m) w
{-# INLINE count #-}

{-
--
-- around 30% slower
--
count w (PS x s m) = inlinePerformIO $ withForeignPtr x $ \p ->
     go (p `plusPtr` s) (fromIntegral m) 0
    where
        go :: Ptr Word8 -> CSize -> Int -> IO Int
        STRICT3(go)
        go p l i = do
            q <- memchr p w l
            if q == nullPtr
                then return i
                else do let k = fromIntegral $ q `minusPtr` p
                        go (q `plusPtr` 1) (l-k-1) (i+1)
-}

-- | The 'findIndex' function takes a predicate and a 'Buffer' and
-- returns the index of the first element in the Buffer
-- satisfying the predicate.
findIndex :: (Word8 -> Bool) -> Buffer -> Maybe Int
findIndex k (PS x s l _) = inlinePerformIO $ withForeignPtr x $ \f -> go (f `plusPtr` s) 0
  where
    STRICT2(go)
    go ptr n | n >= l    = return Nothing
             | otherwise = do w <- peek ptr
                              if k w
                                then return (Just n)
                                else go (ptr `plusPtr` 1) (n+1)
{-# INLINE findIndex #-}

-- | The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending order.
findIndices :: (Word8 -> Bool) -> Buffer -> [Int]
findIndices p xs = loop 0 xs
   where
     STRICT2(loop)
     loop n qs | null qs           = []
               | p (unsafeHead qs) = n : loop (n+1) (unsafeTail qs)
               | otherwise         =     loop (n+1) (unsafeTail qs)

-- ---------------------------------------------------------------------
-- Searching Buffers

-- | /O(n)/ 'elem' is the 'Buffer' membership predicate.
elem :: Word8 -> Buffer -> Bool
elem c xs = case elemIndex c xs of Nothing -> False ; _ -> True
{-# INLINE elem #-}

-- | /O(n)/ 'notElem' is the inverse of 'elem'
notElem :: Word8 -> Buffer -> Bool
notElem c xs = not (elem c xs)
{-# INLINE notElem #-}

-- | /O(n)/ 'filter', applied to a predicate and a Buffer,
-- returns a Buffer containing those characters that satisfy the
-- predicate. This function is subject to array fusion.

-- TODO: this could check to see if the input has 0 tail bytes, then perforce
-- the output will as well, avoiding an extra traversal of the trimmed output
-- and in the case when it wasn't 0 it could fuse the extra counting step into the loop
filter :: (Word8 -> Bool) -> Buffer -> Buffer
filter k xs@(PS x s l _)
    | l <= 0 = xs
    | otherwise = unsafePerformIO $ createAndTrim l $ \p -> withForeignPtr x $ \f -> do
        t <- go (f `plusPtr` s) p (f `plusPtr` (s + l))
        return $! t `minusPtr` p -- actual length
    where
        STRICT3(go)
        go f t end | f == end  = return t
                   | otherwise = do
                        w <- peek f
                        if k w
                            then poke t w >> go (f `plusPtr` 1) (t `plusPtr` 1) end
                            else             go (f `plusPtr` 1) t               end
{-# INLINE filter #-}

{-
--
-- | /O(n)/ A first order equivalent of /filter . (==)/, for the common
-- case of filtering a single byte. It is more efficient to use
-- /filterByte/ in this case.
--
-- > filterByte == filter . (==)
--
-- filterByte is around 10x faster, and uses much less space, than its
-- filter equivalent
--
filterByte :: Word8 -> Buffer -> Buffer
filterByte w ps = replicate (count w ps) w
{-# INLINE filterByte #-}

{-# RULES
"Buffer specialise filter (== x)" forall x.
    filter ((==) x) = filterByte x
"Buffer specialise filter (== x)" forall x.
    filter (== x) = filterByte x
  #-}
-}

-- | /O(n)/ The 'find' function takes a predicate and a Buffer,
-- and returns the first element in matching the predicate, or 'Nothing'
-- if there is no such element.
--
-- > find f p = case findIndex f p of Just n -> Just (p ! n) ; _ -> Nothing
--
find :: (Word8 -> Bool) -> Buffer -> Maybe Word8
find f p = case findIndex f p of
                    Just n -> Just (p `unsafeIndex` n)
                    _      -> Nothing
{-# INLINE find #-}

{-
--
-- fuseable, but we don't want to walk the whole array.
-- 
find k = foldl findEFL Nothing
    where findEFL a@(Just _) _ = a
          findEFL _          c | k c       = Just c
                               | otherwise = Nothing
-}

-- | /O(n)/ The 'partition' function takes a predicate a Buffer and returns
-- the pair of Buffers with elements which do and do not satisfy the
-- predicate, respectively; i.e.,
--
-- > partition p bs == (filter p xs, filter (not . p) xs)
--
partition :: (Word8 -> Bool) -> Buffer -> (Buffer, Buffer)
partition p bs = (filter p bs, filter (not . p) bs)
--TODO: use a better implementation

-- ---------------------------------------------------------------------
-- Searching for substrings

-- | /O(n)/ The 'isPrefixOf' function takes two Buffers and returns 'True'
-- iff the first is a prefix of the second.
isPrefixOf :: Buffer -> Buffer -> Bool
isPrefixOf (PS x1 s1 l1 _) (PS x2 s2 l2 _)
    | l1 == 0   = True
    | l2 < l1   = False
    | otherwise = inlinePerformIO $ withForeignPtr x1 $ \p1 ->
        withForeignPtr x2 $ \p2 -> do
            i <- memcmp (p1 `plusPtr` s1) (p2 `plusPtr` s2) (fromIntegral l1)
            return $! i == 0

-- | /O(n)/ The 'isSuffixOf' function takes two Buffers and returns 'True'
-- iff the first is a suffix of the second.
-- 
-- The following holds:
--
-- > isSuffixOf x y == reverse x `isPrefixOf` reverse y
--
-- However, the real implemenation uses memcmp to compare the end of the
-- string only, with no reverse required..
isSuffixOf :: Buffer -> Buffer -> Bool
isSuffixOf (PS x1 s1 l1 _) (PS x2 s2 l2 _)
    | l1 == 0   = True
    | l2 < l1   = False
    | otherwise = inlinePerformIO $ withForeignPtr x1 $ \p1 ->
        withForeignPtr x2 $ \p2 -> do
            i <- memcmp (p1 `plusPtr` s1) (p2 `plusPtr` s2 `plusPtr` (l2 - l1)) (fromIntegral l1)
            return $! i == 0

-- | Check whether one string is a substring of another. @isInfixOf
-- p s@ is equivalent to @not (null (findSubstrings p s))@.
isInfixOf :: Buffer -> Buffer -> Bool
isInfixOf p s = null p || not (null (snd (breakSubstring p s)))

-- | Break a string on a substring, returning a pair of the part of the
-- string prior to the match, and the rest of the string.
--
-- The following relationships hold:
--
-- > break (== c) l == breakSubstring (singleton c) l
--
-- and:
--
-- > findSubstring s l ==
-- >    if null s then Just 0
-- >              else case breakSubstring s l of
-- >                       (x,y) | null y    -> Nothing
-- >                             | otherwise -> Just (length x)
--
-- For example, to tokenise a string, dropping delimiters:
--
-- > tokenise x y = h : if null t then [] else tokenise x (drop (length x) t)
-- >     where (h,t) = breakSubstring x y
--
-- To skip to the first occurence of a string:
-- 
-- > snd (breakSubstring x y) 
--
-- To take the parts of a string before a delimiter:
--
-- > fst (breakSubstring x y) 
--
breakSubstring :: Buffer -- ^ String to search for
               -> Buffer -- ^ String to search in
               -> (Buffer,Buffer) -- ^ Head and tail of string broken at substring

breakSubstring pat src = search 0 src
  where
    STRICT2(search)
    search n s
        | null s             = (src,empty)      -- not found
        | pat `isPrefixOf` s = (take n src,s)
        | otherwise          = search (n+1) (unsafeTail s)

{-
{- This function uses the Knuth-Morris-Pratt string matching algorithm.  -}

findSubstrings pat@(PS _ _ m) str@(PS _ _ n) = search 0 0
  where
      patc x = pat `unsafeIndex` x
      strc x = str `unsafeIndex` x

      -- maybe we should make kmpNext a UArray before using it in search?
      kmpNext = listArray (0,m) (-1:kmpNextL pat (-1))
      kmpNextL p _ | null p = []
      kmpNextL p j = let j' = next (unsafeHead p) j + 1
                         ps = unsafeTail p
                         x = if not (null ps) && unsafeHead ps == patc j'
                                then kmpNext Array.! j' else j'
                        in x:kmpNextL ps j'
      search i j = match ++ rest -- i: position in string, j: position in pattern
        where match = if j == m then [(i - j)] else []
              rest = if i == n then [] else search (i+1) (next (strc i) j + 1)
      next c j | j >= 0 && (j == m || c /= patc j) = next c (kmpNext Array.! j)
               | otherwise = j
-}

-- ---------------------------------------------------------------------
-- Zipping

-- | /O(n)/ 'zip' takes two Buffers and returns a list of
-- corresponding pairs of bytes. If one input Buffer is short,
-- excess elements of the longer Buffer are discarded. This is
-- equivalent to a pair of 'unpack' operations.
zip :: Buffer -> Buffer -> [(Word8,Word8)]
zip xs ys
    | null xs || null ys = []
    | otherwise = (unsafeHead xs, unsafeHead ys) : zip (unsafeTail xs) (unsafeTail ys)

-- | 'zipWith' generalises 'zip' by zipping with the function given as
-- the first argument, instead of a tupling function.  For example,
-- @'zipWith' (+)@ is applied to two Buffers to produce the list of
-- corresponding sums. 
zipWith :: (Word8 -> Word8 -> a) -> Buffer -> Buffer -> [a]
zipWith f xs ys
    | null xs || null ys = []
    | otherwise = f (unsafeHead xs) (unsafeHead ys) : zipWith f (unsafeTail xs) (unsafeTail ys)

--
-- | A specialised version of zipWith for the common case of a
-- simultaneous map over two Buffers, to build a 3rd. Rewrite rules
-- are used to automatically covert zipWith into zipWith' when a pack is
-- performed on the result of zipWith.
--
zipWith' :: (Word8 -> Word8 -> Word8) -> Buffer -> Buffer -> Buffer
zipWith' f (PS fp s l _) (PS fq t m _) = inlinePerformIO $
    withForeignPtr fp $ \a ->
    withForeignPtr fq $ \b ->
    create len $ zipWith_ 0 (a `plusPtr` s) (b `plusPtr` t)
  where
    zipWith_ :: Int -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO ()
    STRICT4(zipWith_)
    zipWith_ n p1 p2 r
       | n >= len = return ()
       | otherwise = do
            x <- peekByteOff p1 n
            y <- peekByteOff p2 n
            pokeByteOff r n (f x y)
            zipWith_ (n+1) p1 p2 r

    len = min l m
{-# INLINE zipWith' #-}

{-# RULES
"Buffer specialise zipWith" forall (f :: Word8 -> Word8 -> Word8) p q .
    zipWith f p q = unpack (zipWith' f p q)
  #-}

-- | /O(n)/ 'unzip' transforms a list of pairs of bytes into a pair of
-- Buffers. Note that this performs two 'pack' operations.
unzip :: [(Word8,Word8)] -> (Buffer,Buffer)
unzip ls = (pack (P.map fst ls), pack (P.map snd ls))
{-# INLINE unzip #-}

-- ---------------------------------------------------------------------
-- Special lists

-- | /O(n)/ Return all initial segments of the given 'Buffer', shortest first.
inits :: Buffer -> [Buffer]
inits (PS x s l _) = go 0 0 where
    STRICT2(go)
    go i e' = PS x s i e' 
            : if i == l 
              then []
              else go i $! 
                   plusExtra e' $! 
                   inlinePerformIO $ withForeignPtr x $ \p -> peekByteOff p (s + i)

-- | /O(n)/ Return all final segments of the given 'Buffer', longest first.
tails :: Buffer -> [Buffer]
tails p | null p    = [empty]
        | otherwise = p : tails (unsafeTail p)

-- less efficent spacewise: tails (PS x s l) = [PS x (s+n) (l-n) | n <- [0..l]]

-- ---------------------------------------------------------------------
-- ** Ordered 'Buffer's

-- | /O(n)/ Sort a Buffer efficiently, using counting sort.
sort :: Buffer -> Buffer
sort (PS input s l e) = unsafeCreate' l $ \p -> 
                        allocaArray 256 $ \arr -> do
    _ <- memset (castPtr arr) 0 $! 256 * fromIntegral (sizeOf (undefined :: CSize))
    withForeignPtr input $ \x -> countOccurrences arr (x `plusPtr` s) l
    let STRICT2(go)
        go 256 _   = return ()
        go i   ptr = do n <- peekElemOff arr i
                        when (n /= 0) $ memset ptr (fromIntegral i) n >> return ()
                        go (i + 1) (ptr `plusPtr` (fromIntegral n))
    go 0 p
    return e
  where
    -- | Count the number of occurrences of each byte.
    -- Used by 'sort'
    --
    countOccurrences :: Ptr CSize -> Ptr Word8 -> Int -> IO ()
    STRICT3(countOccurrences)
    countOccurrences counts str len = go 0
     where
        STRICT1(go)
        go i | i == len    = return ()
             | otherwise = do k <- fromIntegral `fmap` peekElemOff str i
                              x <- peekElemOff counts k
                              pokeElemOff counts k (x + 1)
                              go (i + 1)

{-
sort :: Buffer -> Buffer
sort (PS x s l) = unsafeCreate l $ \p -> withForeignPtr x $ \f -> do
        memcpy p (f `plusPtr` s) l
        c_qsort p l -- inplace
-}

-- The 'sortBy' function is the non-overloaded version of 'sort'.
--
-- Try some linear sorts: radix, counting
-- Or mergesort.
--
-- sortBy :: (Word8 -> Word8 -> Ordering) -> Buffer -> Buffer
-- sortBy f ps = undefined

-- ---------------------------------------------------------------------
-- Low level constructors

-- | /O(n) construction/ Use a @Buffer@ with a function requiring a
-- null-terminated @CString@.  The @CString@ will be freed
-- automatically. This is a memcpy(3).
useAsCString :: Buffer -> (CString -> IO a) -> IO a
useAsCString (PS fp o l _) action = do
 allocaBytes (l+1) $ \buf ->
   withForeignPtr fp $ \p -> do
     memcpy buf (p `plusPtr` o) (fromIntegral l)
     pokeByteOff buf l (0::Word8)
     action (castPtr buf)

-- | /O(n) construction/ Use a @Buffer@ with a function requiring a @CStringLen@.
-- As for @useAsCString@ this function makes a copy of the original @Buffer@.
useAsCStringLen :: Buffer -> (CStringLen -> IO a) -> IO a
useAsCStringLen p@(PS _ _ l _) f = useAsCString p $ \cstr -> f (cstr,l)

------------------------------------------------------------------------

-- | /O(n)./ Construct a new @Buffer@ from a @CString@. The
-- resulting @Buffer@ is an immutable copy of the original
-- @CString@, and is managed on the Haskell heap. The original
-- @CString@ must be null terminated.
packCString :: CString -> IO Buffer
packCString cstr = do
    len <- c_strlen cstr
    packCStringLen (cstr, fromIntegral len)

-- | /O(n)./ Construct a new @Buffer@ from a @CStringLen@. The
-- resulting @Buffer@ is an immutable copy of the original @CStringLen@.
-- The @Buffer@ is a normal Haskell value and will be managed on the
-- Haskell heap.
packCStringLen :: CStringLen -> IO Buffer
packCStringLen (cstr, len) | len >= 0 = create len $ \p ->
    memcpy p (castPtr cstr) (fromIntegral len)
packCStringLen (_, len) =
    moduleError "packCStringLen" ("negative length: " ++ show len)

------------------------------------------------------------------------

-- | /O(n)/ Make a copy of the 'Buffer' with its own storage. 
-- This is mainly useful to allow the rest of the data pointed
-- to by the 'Buffer' to be garbage collected, for example
-- if a large string has been read in, and only a small part of it 
-- is needed in the rest of the program.
-- 
copy :: Buffer -> Buffer
copy (PS x s l e) = unsafeCreate' l $ \p -> withForeignPtr x $ \f -> do
    memcpy p (f `plusPtr` s) (fromIntegral l)
    return e

-- ---------------------------------------------------------------------
-- Line IO

-- | Read a line from stdin.
getLine :: IO Buffer
getLine = hGetLine stdin

-- | Read a line from a handle

hGetLine :: Handle -> IO Buffer

#if !defined(__GLASGOW_HASKELL__)

hGetLine h = System.IO.hGetLine h >>= return . pack . P.map c2w

#elif __GLASGOW_HASKELL__ >= 611

hGetLine h =
  wantReadableHandle_ "Data.Buffer.hGetLine" h $
    \ h_@Handle__{haByteBuffer} -> do
      flushCharReadBuffer h_
      buf <- readIORef haByteBuffer
      if isEmptyBuffer buf
         then fill h_ buf 0 []
         else haveBuf h_ buf 0 []
 where

  fill h_@Handle__{haByteBuffer,haDevice} buf len xss =
    len `seq` do
    (r,buf') <- Buffered.fillReadBuffer haDevice buf
    if r == 0
       then do writeIORef haByteBuffer buf{ bufR=0, bufL=0 }
               if len > 0
                  then mkBigPS len xss
                  else ioe_EOF
       else haveBuf h_ buf' len xss

  haveBuf h_@Handle__{haByteBuffer}
          buf@IOBase.Buffer{ bufRaw=raw, bufR=w, bufL=r }
          len xss =
    do
        off <- findEOL r w raw
        let new_len = len + off - r
        xs <- mkPS raw r off

      -- if eol == True, then off is the offset of the '\n'
      -- otherwise off == w and the buffer is now empty.
        if off /= w
            then do if (w == off + 1)
                            then writeIORef haByteBuffer buf{ bufL=0, bufR=0 }
                            else writeIORef haByteBuffer buf{ bufL = off + 1 }
                    mkBigPS new_len (xs:xss)
            else do
                 fill h_ buf{ bufL=0, bufR=0 } new_len (xs:xss)

  -- find the end-of-line character, if there is one
  findEOL r w raw
        | r == w = return w
        | otherwise =  do
            c <- readWord8Buf raw r
            if c == fromIntegral (ord '\n')
                then return r -- NB. not r+1: don't include the '\n'
                else findEOL (r+1) w raw

mkPS :: RawBuffer Word8 -> Int -> Int -> IO Buffer
mkPS buf start end =
 create len $ \p ->
   withRawBuffer buf $ \pbuf -> do
   copyBytes p (pbuf `plusPtr` start) len
 where
   len = end - start

#else
-- GHC 6.10 and older, pre-Unicode IO library

hGetLine h = wantReadableHandle "Data.Buffer.hGetLine" h $ \ handle_ -> do
    case haBufferMode handle_ of
       NoBuffering -> error "no buffering"
       _other      -> hGetLineBuffered handle_

 where
    hGetLineBuffered handle_ = do
        let ref = haBuffer handle_
        buf <- readIORef ref
        hGetLineBufferedLoop handle_ ref buf 0 []

    hGetLineBufferedLoop handle_ ref
            buf@IOBase.Buffer{ bufRPtr=r, bufWPtr=w, bufBuf=raw } len xss =
        len `seq` do
        off <- findEOL r w raw
        let new_len = len + off - r
        xs <- mkPS raw r off

      -- if eol == True, then off is the offset of the '\n'
      -- otherwise off == w and the buffer is now empty.
        if off /= w
            then do if (w == off + 1)
                            then writeIORef ref buf{ bufRPtr=0, bufWPtr=0 }
                            else writeIORef ref buf{ bufRPtr = off + 1 }
                    mkBigPS new_len (xs:xss)
            else do
                 maybe_buf <- maybeFillReadBuffer (haFD handle_) True (haIsStream handle_)
                                    buf{ bufWPtr=0, bufRPtr=0 }
                 case maybe_buf of
                    -- Nothing indicates we caught an EOF, and we may have a
                    -- partial line to return.
                    Nothing -> do
                         writeIORef ref buf{ bufRPtr=0, bufWPtr=0 }
                         if new_len > 0
                            then mkBigPS new_len (xs:xss)
                            else ioe_EOF
                    Just new_buf ->
                         hGetLineBufferedLoop handle_ ref new_buf new_len (xs:xss)

    -- find the end-of-line character, if there is one
    findEOL r w raw
        | r == w = return w
        | otherwise =  do
            (c,r') <- readCharFromBuffer raw r
            if c == '\n'
                then return r -- NB. not r': don't include the '\n'
                else findEOL r' w raw

    maybeFillReadBuffer fd is_line is_stream buf = catch
        (do buf' <- fillReadBuffer fd is_line is_stream buf
            return (Just buf'))
        (\e -> if isEOFError e then return Nothing else ioError e)

-- TODO, rewrite to use normal memcpy
mkPS :: RawBuffer -> Int -> Int -> IO Buffer
mkPS buf start end =
    let len = end - start
    in create len $ \p -> do
        memcpy_ptr_baoff p buf (fromIntegral start) (fromIntegral len)
        return ()

#endif

mkBigPS :: Int -> [Buffer] -> IO Buffer
mkBigPS _ [xs] = return xs
mkBigPS _ xss = return $! concat (P.reverse xss)

-- ---------------------------------------------------------------------
-- Block IO

-- | Outputs a 'Buffer' to the specified 'Handle'.
hPut :: Handle -> Buffer -> IO ()
hPut _ (PS _ _ 0 _) = return ()
hPut h (PS x s l _) = withForeignPtr x $ \p-> hPutBuf h (p `plusPtr` s) l

-- | A synonym for @hPut@, for compatibility 
hPutStr :: Handle -> Buffer -> IO ()
hPutStr = hPut

-- | Write a Buffer to a handle, appending a newline byte
hPutStrLn :: Handle -> Buffer -> IO ()
hPutStrLn h xs
    | length xs < 1024 = hPut h (xs `snoc` 0x0a)
    | otherwise        = hPut h xs >> hPut h (singleton (0x0a)) -- don't copy

-- | Write a Buffer to stdout
putStr :: Buffer -> IO ()
putStr = hPut stdout

-- | Write a Buffer to stdout, appending a newline byte
putStrLn :: Buffer -> IO ()
putStrLn = hPutStrLn stdout

------------------------------------------------------------------------
-- Low level IO

-- | Read a 'Buffer' directly from the specified 'Handle'.  This
-- is far more efficient than reading the characters into a 'String'
-- and then using 'pack'. First argument is the Handle to read from, 
-- and the second is the number of bytes to read. It returns the bytes
-- read, up to n, or EOF.
--
-- 'hGet' is implemented in terms of 'hGetBuf'.
--
-- If the handle is a pipe or socket, and the writing end
-- is closed, 'hGet' will behave as if EOF was reached.
--
hGet :: Handle -> Int -> IO Buffer
hGet h i
    | i >  0    = createAndTrim i $ \p -> hGetBuf h p i
    | i == 0    = return empty
    | otherwise = illegalBufferSize h "hGet" i

-- | hGetNonBlocking is identical to 'hGet', except that it will never block
-- waiting for data to become available, instead it returns only whatever data
-- is available.
--
hGetNonBlocking :: Handle -> Int -> IO Buffer
#if defined(__GLASGOW_HASKELL__)
hGetNonBlocking h i
    | i >  0    = createAndTrim i $ \p -> hGetBufNonBlocking h p i
    | i == 0    = return empty
    | otherwise = illegalBufferSize h "hGetNonBlocking" i
#else
hGetNonBlocking = hGet
#endif

illegalBufferSize :: Handle -> String -> Int -> IO a
illegalBufferSize handle fn sz =
    ioError (mkIOError illegalOperationErrorType msg (Just handle) Nothing)
    --TODO: System.IO uses InvalidArgument here, but it's not exported :-(
    where
      msg = fn ++ ": illegal Buffer size " ++ showsPrec 9 sz []


-- | Read entire handle contents strictly into a 'Buffer'.
--
-- This function reads chunks at a time, doubling the chunksize on each
-- read. The final buffer is then realloced to the appropriate size. For
-- files > half of available memory, this may lead to memory exhaustion.
-- Consider using 'readFile' in this case.
--
-- The Handle is closed once the contents have been read,
-- or if an exception is thrown.
--
hGetContents :: Handle -> IO Buffer
hGetContents h = always (hClose h) $ do -- strict, so hClose
    let start_size = 1024
    p <- mallocBytes start_size
    i <- hGetBuf h p start_size
    if i < start_size
        then do p' <- reallocBytes p i
                fp <- newForeignPtr finalizerFree p'
                return $! ps fp 0 i
        else f p start_size
    where
        always = flip finally
        f p s = do
            let s' = 2 * s
            p' <- reallocBytes p s'
            i  <- hGetBuf h (p' `plusPtr` s) s
            if i < s
                then do let i' = s + i
                        p'' <- reallocBytes p' i'
                        fp  <- newForeignPtr finalizerFree p''
                        return $! ps fp 0 i'
                else f p' s'

-- | getContents. Read stdin strictly. Equivalent to hGetContents stdin
-- The 'Handle' is closed after the contents have been read.
--
getContents :: IO Buffer
getContents = hGetContents stdin

-- | The interact function takes a function of type @Buffer -> Buffer@
-- as its argument. The entire input from the standard input device is passed
-- to this function as its argument, and the resulting string is output on the
-- standard output device.
--
interact :: (Buffer -> Buffer) -> IO ()
interact transformer = putStr . transformer =<< getContents

-- | Read an entire file strictly into a 'Buffer'.  This is far more
-- efficient than reading the characters into a 'String' and then using
-- 'pack'.  It also may be more efficient than opening the file and
-- reading it using hGet. Files are read using 'binary mode' on Windows,
-- for 'text mode' use the Char8 version of this function.
--
readFile :: FilePath -> IO Buffer
readFile f = bracket (openBinaryFile f ReadMode) hClose
    (\h -> hFileSize h >>= hGet h . fromIntegral)

-- | Write a 'Buffer' to a file.
writeFile :: FilePath -> Buffer -> IO ()
writeFile f txt = bracket (openBinaryFile f WriteMode) hClose
    (\h -> hPut h txt)

-- | Append a 'Buffer' to a file.
appendFile :: FilePath -> Buffer -> IO ()
appendFile f txt = bracket (openBinaryFile f AppendMode) hClose
    (\h -> hPut h txt)

-- ---------------------------------------------------------------------
-- Internal utilities

-- | 'findIndexOrEnd' is a variant of findIndex, that returns the length
-- of the string if no element is found, rather than Nothing.
findIndexOrEnd :: (Word8 -> Bool) -> Buffer -> Int
findIndexOrEnd k (PS x s l _) = inlinePerformIO $ withForeignPtr x $ \f -> go (f `plusPtr` s) 0
  where
    STRICT2(go)
    go ptr n | n >= l    = return l
             | otherwise = do w <- peek ptr
                              if k w
                                then return n
                                else go (ptr `plusPtr` 1) (n+1)
{-# INLINE findIndexOrEnd #-}

-- | Perform an operation with a temporary Buffer
withPtr :: ForeignPtr a -> (Ptr a -> IO b) -> b
withPtr fp io = inlinePerformIO (withForeignPtr fp io)
{-# INLINE withPtr #-}

-- Common up near identical calls to `error' to reduce the number
-- constant strings created when compiled:
errorEmptyList :: String -> a
errorEmptyList fun = moduleError fun "empty Buffer"
{-# NOINLINE errorEmptyList #-}

moduleError :: String -> String -> a
moduleError fun msg = error ("Data.Buffer." ++ fun ++ ':':' ':msg)
{-# NOINLINE moduleError #-}

-- Find from the end of the string using predicate
findFromEndUntil :: (Word8 -> Bool) -> Buffer -> Int
STRICT2(findFromEndUntil)
findFromEndUntil f xs@(PS x s l _) =
    if l <= 0 then 0
    else if f (last xs) then l
         else findFromEndUntil f (PS x s (l-1) 0) -- HACK: invariant broken temporarily, but not saved

