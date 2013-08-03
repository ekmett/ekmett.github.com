-- We cannot actually specify all the language pragmas, see ghc ticket #
-- If we could, these are what they would be:
{- LANGUAGE MagicHash, UnboxedTuples,
            NamedFieldPuns, BangPatterns, RecordWildCards -}
{- OPTIONS_HADDOCK prune -}

-- |
-- Module      : Data.Buffer.Rope
-- Copyright   : (c) Edward Kmett 2010
-- License     : BSD-style
-- Maintainer  : ekmett@gmail.com
-- Stability   : experimental
-- Portability : portable
-- 
-- Excludes a number of methods that change meaning based on contents. 
-- Look in "Data.Buffer.Rope.Word8" or "Data.Buffer.Rope.Char" for:
-- 
-- > sort      :: Rope -> Rope
-- > null      :: Rope -> Bool
-- > tail      :: Rope -> Rope
-- > init      :: Rope -> Rope
-- > length    :: Rope -> Int
-- > take      :: Int -> Rope -> Rope
-- > drop      :: Int -> Rope -> Rope
-- > group     :: Rope -> [Rope]
-- > inits     :: Rope -> [Rope]
-- > tails     :: Rope -> [Rope]
-- > lines     :: Rope -> Rope
-- > reverse   :: Rope -> Rope
-- > words     :: Rope -> Rope
-- > transpose :: [Rope] -> [Rope]
-- > take      :: Int -> Rope -> Rope 
-- > drop      :: Int -> Rope -> Rope
-- > splitAt   :: Int -> Rope -> (Rope, Rope)

module Data.Buffer.Rope (
    -- * Unambiguous methods
    null,               -- :: Rope -> Bool
    empty,              -- :: Rope

    concat,             -- :: [Rope] -> Rope

    unlines,            -- :: [Rope] -> Rope
    unwords,            -- :: [Rope] -> Rope

    intercalate,        -- :: Rope -> [Rope] -> Rope
{-
    -- * Reading from @Rope@s
    readInt,                -- :: Rope -> Maybe (Int, Rope)     -- ViewL Rope Int
    readInteger,            -- :: Rope -> Maybe (Integer, Rope) -- ViewL Rope Integer

    -- ** Searching for substrings
    isPrefixOf,             -- :: Rope -> Rope -> Bool
    isSuffixOf,             -- :: Rope -> Rope -> Bool
    isSubstringOf,          -- :: Rope -> Rope -> Bool

    -- * Copying Ropes
    copy,                   -- :: Rope -> Rope

    -- * I\/O with @Rope@s

    -- ** Standard input and output
    getLine,                -- :: IO Rope
    getContents,            -- :: IO Rope
    putStr,                 -- :: Rope -> IO ()
    putStrLn,               -- :: Rope -> IO ()
    interact,               -- :: (Rope -> Rope) -> IO ()

    -- ** Files
    readFile,               -- :: FilePath -> IO Rope
    writeFile,              -- :: FilePath -> Rope -> IO ()
    appendFile,             -- :: FilePath -> Rope -> IO ()

    -- ** I\/O with Handles
    hGetLine,               -- :: Handle -> IO Rope
    hGetNonBlocking,        -- :: Handle -> Int -> IO Rope
    hGetContents,           -- :: Handle -> IO Rope   
    hGet,                   -- :: Handle -> Int -> IO Rope
    hPut,                   -- :: Handle -> Rope -> IO () 
    hPutStr,                -- :: Handle -> Rope -> IO ()
    hPutStrLn,              -- :: Handle -> Rope -> IO () 
-}


    singleton,      -- :: Roped e => e -> Rope

    viewl,          -- :: Roped e => Rope -> ViewL Rope e
    viewr,          -- :: Roped e => Rope -> ViewR Rope e

    uncons,         -- :: Roped e => Rope -> Maybe (e, Rope)
    unsnoc,         -- :: Roped e => Rope -> Maybe (Rope, e)
    
    pack,           -- :: Roped e => [e] -> Rope
    unpack,         -- :: Roped e => Rope -> [e]

    cons,           -- :: Roped e => e -> Rope -> Rope
    snoc,           -- :: Roped e => Rope -> e -> Rope

    head,           -- :: Roped e => Rope -> e
    last,           -- :: Roped e => Rope -> e

{-
    foldl,          -- :: Roped e => (a -> e -> a) -> a -> Rope -> a
    foldl',         -- :: Roped e => (a -> e -> a) -> a -> Rope -> a
    foldl1,         -- :: Roped e => (e -> e -> e) -> Rope -> e
    foldl1',        -- :: Roped e => (e -> e -> e) -> Rope -> e

    foldr,          -- :: Roped e => (e -> a -> e) -> a -> Rope -> a
    foldr',         -- :: Roped e => (e -> a -> a) -> a -> Rope -> a
    foldr1,         -- :: Roped e => (e -> e -> e) -> Rope -> e
    foldr1',        -- :: Roped e => (e -> e -> e) -> Rope -> e

    concatMap,      -- :: Roped e => (e -> Rope) -> Rope -> Rope

    any,            -- :: Roped e => (e -> Bool) -> Rope -> Rope
    all,            -- :: Roped e => (e -> Bool) -> Rope -> BUffer

    maximum,        -- :: Roped e => Rope -> e
    minimum,        -- :: Roped e => Rope -> e

    scanl,          -- :: Roped e =>(e -> e -> e) -> e -> Rope -> Rope
    scanl1,         -- :: Roped e => (e -> e -> e) -> Rope -> Rope
    scanr,          -- :: Roped e => (e -> e -> e) -> e -> Rope -> Rope
    scanr1,         -- :: Roped e => (e -> e -> e) -> Rope -> Rope

    mapAccumL,      -- :: Roped e => (acc -> e -> (acc, e)) -> acc -> Rope -> (acc, Rope)
    mapAccumR,      -- :: Roped e => (acc -> e -> (acc, e)) -> acc -> Rope -> (acc, Rope)

    mapIndexed,     -- :: Roped e => (Int -> e -> e) -> Rope -> Rope

    replicate,      -- :: Roped e => Int -> e -> Rope

    unfoldr,        -- :: Roped e => (a -> Maybe (e, a)) -> a -> Rope
    unfoldrN,       -- :: Roped e => Int -> (a -> Maybe (e, a)) -> a -> (Rope, Maybe a)

    takeWhile,      -- :: Roped e => (e -> Bool) -> Rope -> Rope
    dropWhile,      -- :: Roped e => (e -> Bool) -> Rope -> Rope

    span,           -- :: Roped e => (e -> Bool) -> Rope -> (Rope, Rope)
    spanEnd,        -- :: Roped e => (e -> Bool) -> Rope -> (Rope, Rope)

    break,          -- :: Roped e => (e -> Bool) -> Rope -> (Rope, Rope)
    breakEnd,       -- :: Roped e => (e -> Bool) -> Rope -> (Rope, Rope)

    groupBy,        -- :: Roped e => (e -> e -> Bool) -> Rope -> [Rope]

    split,          -- :: Roped e => e -> Rope -> Rope
    splitWith,      -- :: Roped e => (e -> Bool) -> Rope -> Rope
    intersperse,    -- :: Roped e => e -> Rope -> Rope

    elem,           -- :: Roped e => e -> Rope -> Bool
    notElem,        -- :: Roped e => e -> Rope -> Bool
     
    find,           -- :: Roped e => (e -> Bool) -> Rope -> Maybe e
    filter,         -- :: Roped e => (e -> Bool) -> Rope -> Rope

    index,          -- :: Roped e => Rope -> Int -> e

    elemIndex,      -- :: Roped e => e -> Rope -> Maybe Int
    elemIndices,    -- :: Roped e => e -> Rope -> [Int]
    elemIndexEnd,   -- :: Roped e => e -> Rope -> Maybe Int

    findIndex,      -- :: Roped e => (e -> Bool) -> Rope -> Maybe Int
    findIndices,    -- :: Roped e => e -> Rope -> [Int]

    count,          -- :: Roped e => e -> Rope -> Int

    zip,            -- :: Roped e => Rope -> Rope -> [(e,e)]
    zipWith,        -- :: Roped e => (e -> e -> a) -> Rope -> Rope -> [a]
    unzip           -- :: Roped e => [(e,e)] -> (Rope,Rope)
-}
  ) where


import Prelude hiding
    (reverse,head,tail,last,init,null,length,map,lines,foldl,foldr,unlines
    ,concat,any,take,drop,splitAt,takeWhile,dropWhile,span,break,elem,filter,maximum
    ,minimum,all,concatMap,foldl1,foldr1,scanl, scanl1, scanr, scanr1
    ,repeat, cycle, interact, iterate,readFile,writeFile,appendFile,replicate
    ,getContents,getLine,putStr,putStrLn ,zip,zipWith,unzip,notElem)

import Data.Rope.Word8 (
    null, empty, concat, unlines, unwords, intercalate, 
    {-
    isPrefixOf, copy,
    getLine, getContents, putStr, putStrLn, 
    interact, readFile, writeFile, appendFile, hGetLine, hGetNonBlocking, 
    hGetContents, hGet, hPut, hPutStr, hPutStrLn, packAddress, unsafePackAddress, 
    , readInt, readInteger
    -} )

import qualified Data.Rope.Word8 as W
import qualified Data.Rope.Char as C

import Data.Rope.View
import Data.Rope.Internal (null, empty)

class Ord e => Roped e where

    singleton :: e -> Rope

    viewl :: Rope -> ViewL Rope e
    viewr :: Rope -> ViewR Rope e

    uncons :: Rope -> Maybe (e, Rope)
    unsnoc :: Rope -> Maybe (Rope, e)
    
    pack :: [e] -> Rope
    unpack :: Rope -> [e]

    cons :: e -> Rope -> Rope
    snoc :: Rope -> e -> Rope

    head :: Rope -> e
    last :: Rope -> e

{-
    foldl :: (a -> e -> a) -> a -> Rope -> a
    foldl' :: (a -> e -> a) -> a -> Rope -> a
    foldl1 :: (e -> e -> e) -> Rope -> e
    foldl1' :: (e -> e -> e) -> Rope -> e

    foldr :: (e -> a -> e) -> a -> Rope -> a
    foldr' :: (e -> a -> a) -> a -> Rope -> a
    foldr1 :: (e -> e -> e) -> Rope -> e
    foldr1' :: (e -> e -> e) -> Rope -> e

    concatMap :: (e -> Rope) -> Rope -> Rope

    any :: (e -> Bool) -> Rope -> Rope

    all :: (e -> Bool) -> Rope -> BUffer

    maximum :: Rope -> e
    minimum :: Rope -> e

    scanl ::(e -> e -> e) -> e -> Rope -> Rope
    scanl1 :: (e -> e -> e) -> Rope -> Rope
    scanr :: (e -> e -> e) -> e -> Rope -> Rope
    scanr1 :: (e -> e -> e) -> Rope -> Rope

    mapAccumL :: (acc -> e -> (acc, e)) -> acc -> Rope -> (acc, Rope)
    mapAccumR :: (acc -> e -> (acc, e)) -> acc -> Rope -> (acc, Rope)

    mapIndexed :: (Int -> e -> e) -> Rope -> Rope

    replicate :: Int -> e -> Rope

    unfoldr :: (a -> Maybe (e, a)) -> a -> Rope
    unfoldrN :: Int -> (a -> Maybe (e, a)) -> a -> (Rope, Maybe a)

    takeWhile :: (e -> Bool) -> Rope -> Rope
    dropWhile :: (e -> Bool) -> Rope -> Rope

    span :: (e -> Bool) -> Rope -> (Rope, Rope)
    spanEnd :: (e -> Bool) -> Rope -> (Rope, Rope)

    break :: (e -> Bool) -> Rope -> (Rope, Rope)
    breakEnd :: (e -> Bool) -> Rope -> (Rope, Rope)

    groupBy :: (e -> e -> Bool) -> Rope -> [Rope]

    split :: e -> Rope -> Rope
    splitWith :: (e -> Bool) -> Rope -> Rope
    intersperse :: e -> Rope -> Rope

    elem :: e -> Rope -> Bool
    notElem :: e -> Rope -> Bool
     
    find :: (e -> Bool) -> Rope -> Maybe e
    filter :: (e -> Bool) -> Rope -> Rope

    index :: Rope -> Int -> e

    elemIndex :: e -> Rope -> Maybe Int
    elemIndices :: e -> Rope -> [Int]
    elemIndexEnd :: e -> Rope -> Maybe Int

    findIndex :: (e -> Bool) -> Rope -> Maybe Int
    findIndices :: e -> Rope -> [Int]

    count :: e -> Rope -> Int

    zip :: Rope -> Rope -> [(e,e)]
    zip as bs = zip (unpack as) (unpack bs)

    zipWith :: (e -> e -> a) -> Rope -> Rope -> [a]
    zipWith f as bs = P.zipWith f (unpack as) (unpack bs)

    unzip :: [(e,e)] -> (Rope, Rope)
    unzip ls = (pack (P.map fst ls), pack (P.map snd ls))
-}

instance Roped Word8 where
    singleton = W.singleton

    viewl = W.viewl
    viewr = W.viewr

    uncons = W.uncons
    unsnoc = W.unsnoc

    head = W.head
    last = W.last

    cons = W.cons
    snoc = W.snoc

{-
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
-}

instance Roped Char where
    singleton = C.singleton

    viewl = C.viewl
    viewr = C.viewr

    uncons = C.uncons
    unsnoc = C.unsnoc

    head = C.head
    last = C.last

{-
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
