-- We cannot actually specify all the language pragmas, see ghc ticket #
-- If we could, these are what they would be:
{- LANGUAGE MagicHash, UnboxedTuples,
            NamedFieldPuns, BangPatterns, RecordWildCards -}
{- OPTIONS_HADDOCK prune -}

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
-- Excludes a number of methods that change meaning based on contents. 
-- Look in "Data.Buffer.Word8" or "Data.Buffer.Char" for:
-- 
-- > sort      :: Buffer -> Buffer
-- > null      :: Buffer -> Bool
-- > tail      :: Buffer -> Buffer
-- > init      :: Buffer -> Buffer
-- > length    :: Buffer -> Int
-- > take      :: Int -> Buffer -> Buffer
-- > drop      :: Int -> Buffer -> Buffer
-- > group     :: Buffer -> [Buffer]
-- > inits     :: Buffer -> [Buffer]
-- > tails     :: Buffer -> [Buffer]
-- > lines     :: Buffer -> Buffer
-- > reverse   :: Buffer -> Buffer
-- > words     :: Buffer -> Buffer
-- > transpose :: [Buffer] -> [Buffer]
-- > take      :: Int -> Buffer -> Buffer 
-- > drop      :: Int -> Buffer -> Buffer
-- > splitAt   :: Int -> Buffer -> (Buffer, Buffer)

module Data.Buffer (
    -- * Unambiguous methods
    null,               -- :: Buffer -> Bool
    empty,              -- :: Buffer
    concat,             -- :: [Buffer] -> Buffer
    unlines,            -- :: [Buffer] -> Buffer
    unwords,            -- :: [Buffer] -> Buffer
    intercalate,        -- :: Buffer -> [Buffer] -> Buffer

    -- * Reading from @Buffer@s
    readInt,                -- :: Buffer -> Maybe (Int, Buffer)     -- ViewL Buffer Int
    readInteger,            -- :: Buffer -> Maybe (Integer, Buffer) -- ViewL Buffer Integer

    -- ** Searching for substrings
    isPrefixOf,             -- :: Buffer -> Buffer -> Bool
    isSuffixOf,             -- :: Buffer -> Buffer -> Bool
    isSubstringOf,          -- :: Buffer -> Buffer -> Bool

    -- ** Packing CStrings and pointers
    packCString,            -- :: CString -> Buffer
    packCStringLen,         -- :: CString -> Buffer
    packMallocCString,      -- :: CString -> Buffer

    -- ** Using Buffers as CStrings
    useAsCString,           -- :: Buffer -> (CString -> IO a) -> IO a
    useAsCStringLen,        -- :: Buffer -> (CStringLen -> IO a) -> IO a

    -- * Copying Buffers
    copy,                   -- :: Buffer -> Buffer
    copyCString,            -- :: CString -> IO Buffer
    copyCStringLen,         -- :: CStringLen -> IO Buffer

    -- * I\/O with @Buffer@s

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
    hGetNonBlocking,        -- :: Handle -> Int -> IO Buffer
    hGetContents,           -- :: Handle -> IO Buffer   
    hGet,                   -- :: Handle -> Int -> IO Buffer
    hPut,                   -- :: Handle -> Buffer -> IO () 
    hPutStr,                -- :: Handle -> Buffer -> IO ()
    hPutStrLn,              -- :: Handle -> Buffer -> IO () 
    packAddress,            -- :: Addr# -> Buffer
    unsafePackAddress,      -- :: Int -> Addr# -> Buffer


    singleton,      -- :: Buffered e =>  Buffered e => e -> Buffer

    viewl,          -- :: Buffered e => Buffer -> ViewL Buffer e
    viewr,          -- :: Buffered e => Buffer -> ViewR Buffer e

    uncons,         -- :: Buffered e => Buffer -> Maybe (e, Buffer)
    unsnoc,         -- :: Buffered e => Buffer -> Maybe (Buffer, e)
    
    pack,           -- :: Buffered e => [e] -> Buffer
    unpack,         -- :: Buffered e => Buffer -> [e]

    cons,           -- :: Buffered e => e -> Buffer -> Buffer
    snoc,           -- :: Buffered e => Buffer -> e -> Buffer

    head,           -- :: Buffered e => Buffer -> e
    last,           -- :: Buffered e => Buffer -> e

    foldl,          -- :: Buffered e => (a -> e -> a) -> a -> Buffer -> a
    foldl',         -- :: Buffered e => (a -> e -> a) -> a -> Buffer -> a
    foldl1,         -- :: Buffered e => (e -> e -> e) -> Buffer -> e
    foldl1',        -- :: Buffered e => (e -> e -> e) -> Buffer -> e

    foldr,          -- :: Buffered e => (e -> a -> e) -> a -> Buffer -> a
    foldr',         -- :: Buffered e => (e -> a -> a) -> a -> Buffer -> a
    foldr1,         -- :: Buffered e => (e -> e -> e) -> Buffer -> e
    foldr1',        -- :: Buffered e => (e -> e -> e) -> Buffer -> e

    concatMap,      -- :: Buffered e => (e -> Buffer) -> Buffer -> Buffer

    any,            -- :: Buffered e => (e -> Bool) -> Buffer -> Buffer
    all,            -- :: Buffered e => (e -> Bool) -> Buffer -> BUffer

    maximum,        -- :: Buffered e => Buffer -> e
    minimum,        -- :: Buffered e => Buffer -> e

    scanl,          -- :: Buffered e =>(e -> e -> e) -> e -> Buffer -> Buffer
    scanl1,         -- :: Buffered e => (e -> e -> e) -> Buffer -> Buffer
    scanr,          -- :: Buffered e => (e -> e -> e) -> e -> Buffer -> Buffer
    scanr1,         -- :: Buffered e => (e -> e -> e) -> Buffer -> Buffer

    mapAccumL,      -- :: Buffered e => (acc -> e -> (acc, e)) -> acc -> Buffer -> (acc, Buffer)
    mapAccumR,      -- :: Buffered e => (acc -> e -> (acc, e)) -> acc -> Buffer -> (acc, Buffer)

    mapIndexed,     -- :: Buffered e => (Int -> e -> e) -> Buffer -> Buffer

    replicate,      -- :: Buffered e => Int -> e -> Buffer

    unfoldr,        -- :: Buffered e => (a -> Maybe (e, a)) -> a -> Buffer
    unfoldrN,       -- :: Buffered e => Int -> (a -> Maybe (e, a)) -> a -> (Buffer, Maybe a)

    takeWhile,      -- :: Buffered e => (e -> Bool) -> Buffer -> Buffer
    dropWhile,      -- :: Buffered e => (e -> Bool) -> Buffer -> Buffer

    span,           -- :: Buffered e => (e -> Bool) -> Buffer -> (Buffer, Buffer)
    spanEnd,        -- :: Buffered e => (e -> Bool) -> Buffer -> (Buffer, Buffer)

    break,          -- :: Buffered e => (e -> Bool) -> Buffer -> (Buffer, Buffer)
    breakEnd,       -- :: Buffered e => (e -> Bool) -> Buffer -> (Buffer, Buffer)

    groupBy,        -- :: Buffered e => (e -> e -> Bool) -> Buffer -> [Buffer]

    split,          -- :: Buffered e => e -> Buffer -> Buffer
    splitWith,      -- :: Buffered e => (e -> Bool) -> Buffer -> Buffer
    intersperse,    -- :: Buffered e => e -> Buffer -> Buffer

    elem,           -- :: Buffered e => e -> Buffer -> Bool
    notElem,        -- :: Buffered e => e -> Buffer -> Bool
     
    find,           -- :: Buffered e => (e -> Bool) -> Buffer -> Maybe e
    filter,         -- :: Buffered e => (e -> Bool) -> Buffer -> Buffer

    index,          -- :: Buffered e => Buffer -> Int -> e

    elemIndex,      -- :: Buffered e => e -> Buffer -> Maybe Int
    elemIndices,    -- :: Buffered e => e -> Buffer -> [Int]
    elemIndexEnd,   -- :: Buffered e => e -> Buffer -> Maybe Int

    findIndex,      -- :: Buffered e => (e -> Bool) -> Buffer -> Maybe Int
    findIndices,    -- :: Buffered e => e -> Buffer -> [Int]

    count,          -- :: Buffered e => e -> Buffer -> Int

    zip,            -- :: Buffered e => Buffer -> Buffer -> [(e,e)]
    zipWith,        -- :: Buffered e => (e -> e -> a) -> Buffer -> Buffer -> [a]
    unzip           -- :: Buffered e => [(e,e)] -> (Buffer,Buffer)
  ) where


import Prelude hiding
    (reverse,head,tail,last,init,null,length,map,lines,foldl,foldr,unlines
    ,concat,any,take,drop,splitAt,takeWhile,dropWhile,span,break,elem,filter,maximum
    ,minimum,all,concatMap,foldl1,foldr1,scanl, scanl1, scanr, scanr1
    ,repeat, cycle, interact, iterate,readFile,writeFile,appendFile,replicate
    ,getContents,getLine,putStr,putStrLn ,zip,zipWith,unzip,notElem)

import Data.Buffer.Word8 (
    null, empty, concat, unlines, unwords, intercalate, isPrefixOf, packCString,
    packCStringLen, packMallocCString, useAsCString, useAsCStringLen, copy, 
    copyCString, copyCStringLen, getLine, getContents, putStr, putStrLn, 
    interact, readFile, writeFile, appendFile, hGetLine, hGetNonBlocking, 
    hGetContents, hGet, hPut, hPutStr, hPutStrLn, packAddress, unsafePackAddress, 
    null, empty, readInt, readInteger)

import qualified Data.Buffer.Word8 as W
import qualified Data.Buffer.Char as C

import Data.Buffer.View
import Data.Buffer.Internal (null, empty)

class Ord e => Buffered e where

    singleton :: e -> Buffer

    viewl :: Buffer -> ViewL Buffer e
    viewr :: Buffer -> ViewR Buffer e

    uncons :: Buffer -> Maybe (e, Buffer)
    unsnoc :: Buffer -> Maybe (Buffer, e)
    
    pack :: [e] -> Buffer
    unpack :: Buffer -> [e]

    cons :: e -> Buffer -> Buffer
    snoc :: Buffer -> e -> Buffer

    head :: Buffer -> e
    last :: Buffer -> e

    foldl :: (a -> e -> a) -> a -> Buffer -> a
    foldl' :: (a -> e -> a) -> a -> Buffer -> a
    foldl1 :: (e -> e -> e) -> Buffer -> e
    foldl1' :: (e -> e -> e) -> Buffer -> e

    foldr :: (e -> a -> e) -> a -> Buffer -> a
    foldr' :: (e -> a -> a) -> a -> Buffer -> a
    foldr1 :: (e -> e -> e) -> Buffer -> e
    foldr1' :: (e -> e -> e) -> Buffer -> e

    concatMap :: (e -> Buffer) -> Buffer -> Buffer

    any :: (e -> Bool) -> Buffer -> Buffer

    all :: (e -> Bool) -> Buffer -> BUffer

    maximum :: Buffer -> e
    minimum :: Buffer -> e

    scanl ::(e -> e -> e) -> e -> Buffer -> Buffer
    scanl1 :: (e -> e -> e) -> Buffer -> Buffer
    scanr :: (e -> e -> e) -> e -> Buffer -> Buffer
    scanr1 :: (e -> e -> e) -> Buffer -> Buffer

    mapAccumL :: (acc -> e -> (acc, e)) -> acc -> Buffer -> (acc, Buffer)
    mapAccumR :: (acc -> e -> (acc, e)) -> acc -> Buffer -> (acc, Buffer)

    mapIndexed :: (Int -> e -> e) -> Buffer -> Buffer

    replicate :: Int -> e -> Buffer

    unfoldr :: (a -> Maybe (e, a)) -> a -> Buffer
    unfoldrN :: Int -> (a -> Maybe (e, a)) -> a -> (Buffer, Maybe a)

    takeWhile :: (e -> Bool) -> Buffer -> Buffer
    dropWhile :: (e -> Bool) -> Buffer -> Buffer

    span :: (e -> Bool) -> Buffer -> (Buffer, Buffer)
    spanEnd :: (e -> Bool) -> Buffer -> (Buffer, Buffer)

    break :: (e -> Bool) -> Buffer -> (Buffer, Buffer)
    breakEnd :: (e -> Bool) -> Buffer -> (Buffer, Buffer)

    groupBy :: (e -> e -> Bool) -> Buffer -> [Buffer]

    split :: e -> Buffer -> Buffer
    splitWith :: (e -> Bool) -> Buffer -> Buffer
    intersperse :: e -> Buffer -> Buffer

    elem :: e -> Buffer -> Bool
    notElem :: e -> Buffer -> Bool
     
    find :: (e -> Bool) -> Buffer -> Maybe e
    filter :: (e -> Bool) -> Buffer -> Buffer

    index :: Buffer -> Int -> e

    elemIndex :: e -> Buffer -> Maybe Int
    elemIndices :: e -> Buffer -> [Int]
    elemIndexEnd :: e -> Buffer -> Maybe Int

    findIndex :: (e -> Bool) -> Buffer -> Maybe Int
    findIndices :: e -> Buffer -> [Int]

    count :: e -> Buffer -> Int

    zip :: Buffer -> Buffer -> [(e,e)]
    zip as bs = zip (unpack as) (unpack bs)

    zipWith :: (e -> e -> a) -> Buffer -> Buffer -> [a]
    zipWith f as bs = P.zipWith f (unpack as) (unpack bs)

    unzip :: [(e,e)] -> (Buffer, Buffer)
    unzip ls = (pack (P.map fst ls), pack (P.map snd ls))


instance Buffered Word8 where
    singleton = W.singleton

    viewl = W.viewl
    viewr = W.viewr

    uncons = W.uncons
    unsnoc = W.unsnoc

    head = W.head
    last = W.last

    cons = W.cons
    snoc = W.snoc

    foldl = W.foldl
    foldl' = W.foldl'
    foldl1 = W.foldl1
    foldl1' = W.foldl1'

    foldr = W.foldr
    foldr' = W.foldr'
    foldr1 = W.foldr1
    foldr1' = W.foldr1'

    concatMap = W.concatMap

    any = W.any
    all = W.all

    minimum = W.minimum
    maximum = W.maximum

    scanl = W.scanl
    scanl1 = W.scanl1
    scanr = W.scanr
    scanr1 = W.scanr1
    
    mapAccumL = W.mapAccumL
    mapAccumR = W.mapAccumR
    
    mapIndexed = W.mapIndexed

    replicate = W.replicate
    unfoldr = W.unfoldr
    unfoldrN = W.unfoldN
    takeWhile = W.takeWhile
    dropWhile = W.dropWhile
    span = W.span
    spanEnd = W.spanEnd

    break = W.break
    breakEnd = W.breakEnd
    groupBy = W.groupBy

    split = W.split
    splitWith = W.splitWith
    
    intersperse = W.intersperse

    elem = W.elem
    notElem = W.notElem
     
    find = W.find
    filter = W.filter
    index = W.index 

    elemIndex = W.elemIndex
    elemIndices = W.elemIndices
    elemIndexEnd = W.elemIndexEnd 

    findIndex = W.findIndex 
    findIndices = W.findIndices

    count = W.count
    zip = W.zip
    zipWith = W.zipWith
    unzip = W.unzip

{-

instance Buffered Char where
    singleton = C.singleton

    viewl = C.viewl
    viewr = C.viewr

    uncons = C.uncons
    unsnoc = C.unsnoc

    head = C.head
    last = C.last

    foldl = C.foldl
    foldl' = C.foldl'
    foldl1 = C.foldl1
    foldl1' = C.foldl1'

    foldr = C.foldr
    foldr' = C.foldr'
    foldr1 = C.foldr1
    foldr1' = C.foldr1'

    concatMap = C.concatMap

    any = C.any
    all = C.all

    minimum = C.minimum
    maximum = C.maximum

    scanl = C.scanl
    scanl1 = C.scanl1
    scanr = C.scanr
    scanr1 = C.scanr1
    
    mapAccumL = C.mapAccumL
    mapAccumR = C.mapAccumR
    
    mapIndexed = C.mapIndexed

    replicate = C.replicate
    unfoldr = C.unfoldr
    unfoldrN = C.unfoldN
    takeChile = C.takeChile
    dropChile = C.dropChile
    span = C.span
    spanEnd = C.spanEnd

    break = C.break
    breakEnd = C.breakEnd
    groupBy = C.groupBy

    split = C.split
    splitCith = C.splitCith
    
    intersperse = C.intersperse

    elem = C.elem
    notElem = C.notElem
     
    find = C.find
    filter = C.filter
    index = C.index 

    elemIndex = C.elemIndex
    elemIndices = C.elemIndices
    elemIndexEnd = C.elemIndexEnd 

    findIndex = C.findIndex 
    findIndices = C.findIndices

    count = C.count
    zip = C.zip
    zipCith = C.zipCith
    unzip = C.unzip

-}
