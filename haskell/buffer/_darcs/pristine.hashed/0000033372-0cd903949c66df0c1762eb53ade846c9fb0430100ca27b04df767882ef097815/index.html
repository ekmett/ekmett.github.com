{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module      : Data.Buffer.Lazy.Char8
-- Copyright   : (c) Don Stewart 2006
-- License     : BSD-style
--
-- Maintainer  : dons@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : non-portable (imports Data.Buffer.Lazy)
--
-- Manipulate /lazy/ 'Buffer's using 'Char' operations. All Chars will
-- be truncated to 8 bits. It can be expected that these functions will
-- run at identical speeds to their 'Data.Word.Word8' equivalents in
-- "Data.Buffer.Lazy".
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions.  eg.
--
-- > import qualified Data.Buffer.Lazy.Char8 as C
--

module Data.Buffer.Lazy.Char8 (

        -- * The @Buffer@ type
        Buffer,             -- instances: Eq, Ord, Show, Read, Data, Typeable

        -- * Introducing and eliminating 'Buffer's
        empty,                  -- :: Buffer
        singleton,              -- :: Char   -> Buffer
        pack,                   -- :: String -> Buffer
        unpack,                 -- :: Buffer -> String
        fromChunks,             -- :: [Strict.Buffer] -> Buffer
        toChunks,               -- :: Buffer -> [Strict.Buffer]

        -- * Basic interface
        cons,                   -- :: Char -> Buffer -> Buffer
        cons',                  -- :: Char -> Buffer -> Buffer
        snoc,                   -- :: Buffer -> Char -> Buffer
        append,                 -- :: Buffer -> Buffer -> Buffer
        head,                   -- :: Buffer -> Char
        uncons,                 -- :: Buffer -> Maybe (Char, Buffer)
        last,                   -- :: Buffer -> Char
        tail,                   -- :: Buffer -> Buffer
        init,                   -- :: Buffer -> Buffer
        null,                   -- :: Buffer -> Bool
        length,                 -- :: Buffer -> Int64

        -- * Transforming Buffers
        map,                    -- :: (Char -> Char) -> Buffer -> Buffer
        reverse,                -- :: Buffer -> Buffer
        intersperse,            -- :: Char -> Buffer -> Buffer
        intercalate,            -- :: Buffer -> [Buffer] -> Buffer
        transpose,              -- :: [Buffer] -> [Buffer]

        -- * Reducing 'Buffer's (folds)
        foldl,                  -- :: (a -> Char -> a) -> a -> Buffer -> a
        foldl',                 -- :: (a -> Char -> a) -> a -> Buffer -> a
        foldl1,                 -- :: (Char -> Char -> Char) -> Buffer -> Char
        foldl1',                -- :: (Char -> Char -> Char) -> Buffer -> Char
        foldr,                  -- :: (Char -> a -> a) -> a -> Buffer -> a
        foldr1,                 -- :: (Char -> Char -> Char) -> Buffer -> Char

        -- ** Special folds
        concat,                 -- :: [Buffer] -> Buffer
        concatMap,              -- :: (Char -> Buffer) -> Buffer -> Buffer
        any,                    -- :: (Char -> Bool) -> Buffer -> Bool
        all,                    -- :: (Char -> Bool) -> Buffer -> Bool
        maximum,                -- :: Buffer -> Char
        minimum,                -- :: Buffer -> Char

        -- * Building Buffers
        -- ** Scans
        scanl,                  -- :: (Char -> Char -> Char) -> Char -> Buffer -> Buffer
--      scanl1,                 -- :: (Char -> Char -> Char) -> Buffer -> Buffer
--      scanr,                  -- :: (Char -> Char -> Char) -> Char -> Buffer -> Buffer
--      scanr1,                 -- :: (Char -> Char -> Char) -> Buffer -> Buffer

        -- ** Accumulating maps
        mapAccumL,              -- :: (acc -> Char -> (acc, Char)) -> acc -> Buffer -> (acc, Buffer)
        mapAccumR,              -- :: (acc -> Char -> (acc, Char)) -> acc -> Buffer -> (acc, Buffer)

        -- ** Infinite Buffers
        repeat,                 -- :: Char -> Buffer
        replicate,              -- :: Int64 -> Char -> Buffer
        cycle,                  -- :: Buffer -> Buffer
        iterate,                -- :: (Char -> Char) -> Char -> Buffer

        -- ** Unfolding Buffers
        unfoldr,                -- :: (a -> Maybe (Char, a)) -> a -> Buffer

        -- * Substrings

        -- ** Breaking strings
        take,                   -- :: Int64 -> Buffer -> Buffer
        drop,                   -- :: Int64 -> Buffer -> Buffer
        splitAt,                -- :: Int64 -> Buffer -> (Buffer, Buffer)
        takeWhile,              -- :: (Char -> Bool) -> Buffer -> Buffer
        dropWhile,              -- :: (Char -> Bool) -> Buffer -> Buffer
        span,                   -- :: (Char -> Bool) -> Buffer -> (Buffer, Buffer)
        break,                  -- :: (Char -> Bool) -> Buffer -> (Buffer, Buffer)
        group,                  -- :: Buffer -> [Buffer]
        groupBy,                -- :: (Char -> Char -> Bool) -> Buffer -> [Buffer]
        inits,                  -- :: Buffer -> [Buffer]
        tails,                  -- :: Buffer -> [Buffer]

        -- ** Breaking into many substrings
        split,                  -- :: Char -> Buffer -> [Buffer]
        splitWith,              -- :: (Char -> Bool) -> Buffer -> [Buffer]

        -- ** Breaking into lines and words
        lines,                  -- :: Buffer -> [Buffer]
        words,                  -- :: Buffer -> [Buffer]
        unlines,                -- :: [Buffer] -> Buffer
        unwords,                -- :: Buffer -> [Buffer]

        -- * Predicates
        isPrefixOf,             -- :: Buffer -> Buffer -> Bool
--      isSuffixOf,             -- :: Buffer -> Buffer -> Bool

        -- * Searching Buffers

        -- ** Searching by equality
        elem,                   -- :: Char -> Buffer -> Bool
        notElem,                -- :: Char -> Buffer -> Bool

        -- ** Searching with a predicate
        find,                   -- :: (Char -> Bool) -> Buffer -> Maybe Char
        filter,                 -- :: (Char -> Bool) -> Buffer -> Buffer
--      partition               -- :: (Char -> Bool) -> Buffer -> (Buffer, Buffer)

        -- * Indexing Buffers
        index,                  -- :: Buffer -> Int64 -> Char
        elemIndex,              -- :: Char -> Buffer -> Maybe Int64
        elemIndices,            -- :: Char -> Buffer -> [Int64]
        findIndex,              -- :: (Char -> Bool) -> Buffer -> Maybe Int64
        findIndices,            -- :: (Char -> Bool) -> Buffer -> [Int64]
        count,                  -- :: Char -> Buffer -> Int64

        -- * Zipping and unzipping Buffers
        zip,                    -- :: Buffer -> Buffer -> [(Char,Char)]
        zipWith,                -- :: (Char -> Char -> c) -> Buffer -> Buffer -> [c]
--      unzip,                  -- :: [(Char,Char)] -> (Buffer,Buffer)

        -- * Ordered Buffers
--        sort,                   -- :: Buffer -> Buffer

        -- * Low level conversions
        -- ** Copying Buffers
        copy,                   -- :: Buffer -> Buffer

        -- * Reading from Buffers
        readInt,
        readInteger,

        -- * I\/O with 'Buffer's

        -- ** Standard input and output
        getContents,            -- :: IO Buffer
        putStr,                 -- :: Buffer -> IO ()
        putStrLn,               -- :: Buffer -> IO ()
        interact,               -- :: (Buffer -> Buffer) -> IO ()

        -- ** Files
        readFile,               -- :: FilePath -> IO Buffer
        writeFile,              -- :: FilePath -> Buffer -> IO ()
        appendFile,             -- :: FilePath -> Buffer -> IO ()

        -- ** I\/O with Handles
        hGetContents,           -- :: Handle -> IO Buffer
        hGet,                   -- :: Handle -> Int64 -> IO Buffer
        hGetNonBlocking,        -- :: Handle -> Int64 -> IO Buffer
        hPut,                   -- :: Handle -> Buffer -> IO ()

  ) where

-- Functions transparently exported
import Data.Buffer.Lazy.Word8
        (fromChunks, toChunks
        ,empty,null,length,tail,init,append,reverse,transpose,cycle
        ,concat,take,drop,splitAt,intercalate,isPrefixOf,group,inits,tails,copy
        ,hGetContents, hGet, hPut, getContents
        ,hGetNonBlocking
        ,putStr, putStrLn, interact)

-- Functions we need to wrap.
import qualified Data.Buffer.Lazy.Word8 as L
import qualified Data.Buffer as S (Buffer) -- typename only
import qualified Data.Buffer as B
import qualified Data.Buffer.Unsafe as B
import Data.Buffer.Lazy.Internal

import Data.Buffer.Internal (w2c, c2w, isSpaceWord8)

import Data.Int (Int64)
import qualified Data.List as List

import Prelude hiding           
        (reverse,head,tail,last,init,null,length,map,lines,foldl,foldr,unlines
        ,concat,any,take,drop,splitAt,takeWhile,dropWhile,span,break,elem,filter
        ,unwords,words,maximum,minimum,all,concatMap,scanl,scanl1,foldl1,foldr1
        ,readFile,writeFile,appendFile,replicate,getContents,getLine,putStr,putStrLn
        ,zip,zipWith,unzip,notElem,repeat,iterate,interact,cycle)

import System.IO            (hClose,openFile,IOMode(..))
#ifndef __NHC__
import Control.Exception    (bracket)
#else
import IO                   (bracket)
#endif

#if __GLASGOW_HASKELL__ >= 608
import Data.String
#endif

#define STRICT1(f) f a | a `seq` False = undefined
#define STRICT2(f) f a b | a `seq` b `seq` False = undefined
#define STRICT3(f) f a b c | a `seq` b `seq` c `seq` False = undefined
#define STRICT4(f) f a b c d | a `seq` b `seq` c `seq` d `seq` False = undefined
#define STRICT5(f) f a b c d e | a `seq` b `seq` c `seq` d `seq` e `seq` False = undefined
#define STRICT5_(f) f a b c d _ | a `seq` b `seq` c `seq` d `seq` False = undefined

------------------------------------------------------------------------

-- | /O(1)/ Convert a 'Char' into a 'Buffer'
singleton :: Char -> Buffer
singleton = L.singleton . c2w
{-# INLINE singleton #-}

#if __GLASGOW_HASKELL__ >= 608
instance IsString Buffer where
    fromString = pack
    {-# INLINE fromString #-}
#endif

-- | /O(n)/ Convert a 'String' into a 'Buffer'. 
pack :: [Char] -> Buffer
pack = L.pack. List.map c2w

-- | /O(n)/ Converts a 'Buffer' to a 'String'.
unpack :: Buffer -> [Char]
unpack = List.map w2c . L.unpack
{-# INLINE unpack #-}

-- | /O(1)/ 'cons' is analogous to '(:)' for lists.
cons :: Char -> Buffer -> Buffer
cons = L.cons . c2w
{-# INLINE cons #-}

-- | /O(1)/ Unlike 'cons', 'cons\'' is
-- strict in the Buffer that we are consing onto. More precisely, it forces
-- the head and the first chunk. It does this because, for space efficiency, it
-- may coalesce the new byte onto the first \'chunk\' rather than starting a
-- new \'chunk\'.
--
-- So that means you can't use a lazy recursive contruction like this:
--
-- > let xs = cons\' c xs in xs
--
-- You can however use 'cons', as well as 'repeat' and 'cycle', to build
-- infinite lazy Buffers.
--
cons' :: Char -> Buffer -> Buffer
cons' = L.cons' . c2w
{-# INLINE cons' #-}

-- | /O(n)/ Append a Char to the end of a 'Buffer'. Similar to
-- 'cons', this function performs a memcpy.
snoc :: Buffer -> Char -> Buffer
snoc p = L.snoc p . c2w
{-# INLINE snoc #-}

-- | /O(1)/ Extract the first element of a Buffer, which must be non-empty.
head :: Buffer -> Char
head = w2c . L.head
{-# INLINE head #-}

-- | /O(1)/ Extract the head and tail of a Buffer, returning Nothing
-- if it is empty.
uncons :: Buffer -> Maybe (Char, Buffer)
uncons bs = case L.uncons bs of
                  Nothing -> Nothing
                  Just (w, bs') -> Just (w2c w, bs')
{-# INLINE uncons #-}

-- | /O(1)/ Extract the last element of a packed string, which must be non-empty.
last :: Buffer -> Char
last = w2c . L.last
{-# INLINE last #-}

-- | /O(n)/ 'map' @f xs@ is the Buffer obtained by applying @f@ to each element of @xs@
map :: (Char -> Char) -> Buffer -> Buffer
map f = L.map (c2w . f . w2c)
{-# INLINE map #-}

-- | /O(n)/ The 'intersperse' function takes a Char and a 'Buffer'
-- and \`intersperses\' that Char between the elements of the
-- 'Buffer'.  It is analogous to the intersperse function on Lists.
intersperse :: Char -> Buffer -> Buffer
intersperse = L.intersperse . c2w
{-# INLINE intersperse #-}

-- | 'foldl', applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and a Buffer, reduces the
-- Buffer using the binary operator, from left to right.
foldl :: (a -> Char -> a) -> a -> Buffer -> a
foldl f = L.foldl (\a c -> f a (w2c c))
{-# INLINE foldl #-}

-- | 'foldl\'' is like foldl, but strict in the accumulator.
foldl' :: (a -> Char -> a) -> a -> Buffer -> a
foldl' f = L.foldl' (\a c -> f a (w2c c))
{-# INLINE foldl' #-}

-- | 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a packed string,
-- reduces the packed string using the binary operator, from right to left.
foldr :: (Char -> a -> a) -> a -> Buffer -> a
foldr f = L.foldr (\c a -> f (w2c c) a)
{-# INLINE foldr #-}

-- | 'foldl1' is a variant of 'foldl' that has no starting value
-- argument, and thus must be applied to non-empty 'Buffers'.
foldl1 :: (Char -> Char -> Char) -> Buffer -> Char
foldl1 f ps = w2c (L.foldl1 (\x y -> c2w (f (w2c x) (w2c y))) ps)
{-# INLINE foldl1 #-}

-- | 'foldl1\'' is like 'foldl1', but strict in the accumulator.
foldl1' :: (Char -> Char -> Char) -> Buffer -> Char
foldl1' f ps = w2c (L.foldl1' (\x y -> c2w (f (w2c x) (w2c y))) ps)

-- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- and thus must be applied to non-empty 'Buffer's
foldr1 :: (Char -> Char -> Char) -> Buffer -> Char
foldr1 f ps = w2c (L.foldr1 (\x y -> c2w (f (w2c x) (w2c y))) ps)
{-# INLINE foldr1 #-}

-- | Map a function over a 'Buffer' and concatenate the results
concatMap :: (Char -> Buffer) -> Buffer -> Buffer
concatMap f = L.concatMap (f . w2c)
{-# INLINE concatMap #-}

-- | Applied to a predicate and a Buffer, 'any' determines if
-- any element of the 'Buffer' satisfies the predicate.
any :: (Char -> Bool) -> Buffer -> Bool
any f = L.any (f . w2c)
{-# INLINE any #-}

-- | Applied to a predicate and a 'Buffer', 'all' determines if
-- all elements of the 'Buffer' satisfy the predicate.
all :: (Char -> Bool) -> Buffer -> Bool
all f = L.all (f . w2c)
{-# INLINE all #-}

-- | 'maximum' returns the maximum value from a 'Buffer'
maximum :: Buffer -> Char
maximum = w2c . L.maximum
{-# INLINE maximum #-}

-- | 'minimum' returns the minimum value from a 'Buffer'
minimum :: Buffer -> Char
minimum = w2c . L.minimum
{-# INLINE minimum #-}

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
scanl :: (Char -> Char -> Char) -> Char -> Buffer -> Buffer
scanl f z = L.scanl (\a b -> c2w (f (w2c a) (w2c b))) (c2w z)

-- | The 'mapAccumL' function behaves like a combination of 'map' and
-- 'foldl'; it applies a function to each element of a Buffer,
-- passing an accumulating parameter from left to right, and returning a
-- final value of this accumulator together with the new Buffer.
mapAccumL :: (acc -> Char -> (acc, Char)) -> acc -> Buffer -> (acc, Buffer)
mapAccumL f = L.mapAccumL (\a w -> case f a (w2c w) of (a',c) -> (a', c2w c))

-- | The 'mapAccumR' function behaves like a combination of 'map' and
-- 'foldr'; it applies a function to each element of a Buffer,
-- passing an accumulating parameter from right to left, and returning a
-- final value of this accumulator together with the new Buffer.
mapAccumR :: (acc -> Char -> (acc, Char)) -> acc -> Buffer -> (acc, Buffer)
mapAccumR f = L.mapAccumR (\acc w -> case f acc (w2c w) of (acc', c) -> (acc', c2w c))

------------------------------------------------------------------------
-- Generating and unfolding Buffers

-- | @'iterate' f x@ returns an infinite Buffer of repeated applications
-- of @f@ to @x@:
--
-- > iterate f x == [x, f x, f (f x), ...]
--
iterate :: (Char -> Char) -> Char -> Buffer
iterate f = L.iterate (c2w . f . w2c) . c2w

-- | @'repeat' x@ is an infinite Buffer, with @x@ the value of every
-- element.
--
repeat :: Char -> Buffer
repeat = L.repeat . c2w

-- | /O(n)/ @'replicate' n x@ is a Buffer of length @n@ with @x@
-- the value of every element.
--
replicate :: Int64 -> Char -> Buffer
replicate w c = L.replicate w (c2w c)

-- | /O(n)/ The 'unfoldr' function is analogous to the List \'unfoldr\'.
-- 'unfoldr' builds a Buffer from a seed value.  The function takes
-- the element and returns 'Nothing' if it is done producing the
-- Buffer or returns 'Just' @(a,b)@, in which case, @a@ is a
-- prepending to the Buffer and @b@ is used as the next element in a
-- recursive call.
unfoldr :: (a -> Maybe (Char, a)) -> a -> Buffer
unfoldr f = L.unfoldr $ \a -> case f a of
                                    Nothing      -> Nothing
                                    Just (c, a') -> Just (c2w c, a')

------------------------------------------------------------------------

-- | 'takeWhile', applied to a predicate @p@ and a Buffer @xs@,
-- returns the longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@.
takeWhile :: (Char -> Bool) -> Buffer -> Buffer
takeWhile f = L.takeWhile (f . w2c)
{-# INLINE takeWhile #-}

-- | 'dropWhile' @p xs@ returns the suffix remaining after 'takeWhile' @p xs@.
dropWhile :: (Char -> Bool) -> Buffer -> Buffer
dropWhile f = L.dropWhile (f . w2c)
{-# INLINE dropWhile #-}

-- | 'break' @p@ is equivalent to @'span' ('not' . p)@.
break :: (Char -> Bool) -> Buffer -> (Buffer, Buffer)
break f = L.break (f . w2c)
{-# INLINE break #-}

-- | 'span' @p xs@ breaks the Buffer into two segments. It is
-- equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@
span :: (Char -> Bool) -> Buffer -> (Buffer, Buffer)
span f = L.span (f . w2c)
{-# INLINE span #-}

{-
-- | 'breakChar' breaks its Buffer argument at the first occurence
-- of the specified Char. It is more efficient than 'break' as it is
-- implemented with @memchr(3)@. I.e.
-- 
-- > break (=='c') "abcd" == breakChar 'c' "abcd"
--
breakChar :: Char -> Buffer -> (Buffer, Buffer)
breakChar = L.breakByte . c2w
{-# INLINE breakChar #-}

-- | 'spanChar' breaks its Buffer argument at the first
-- occurence of a Char other than its argument. It is more efficient
-- than 'span (==)'
--
-- > span  (=='c') "abcd" == spanByte 'c' "abcd"
--
spanChar :: Char -> Buffer -> (Buffer, Buffer)
spanChar = L.spanByte . c2w
{-# INLINE spanChar #-}
-}

--
-- TODO, more rules for breakChar*
--

-- | /O(n)/ Break a 'Buffer' into pieces separated by the byte
-- argument, consuming the delimiter. I.e.
--
-- > split '\n' "a\nb\nd\ne" == ["a","b","d","e"]
-- > split 'a'  "aXaXaXa"    == ["","X","X","X"]
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
split :: Char -> Buffer -> [Buffer]
split = L.split . c2w
{-# INLINE split #-}

-- | /O(n)/ Splits a 'Buffer' into components delimited by
-- separators, where the predicate returns True for a separator element.
-- The resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > splitWith (=='a') "aabbaca" == ["","","bb","c",""]
--
splitWith :: (Char -> Bool) -> Buffer -> [Buffer]
splitWith f = L.splitWith (f . w2c)
{-# INLINE splitWith #-}

-- | The 'groupBy' function is the non-overloaded version of 'group'.
groupBy :: (Char -> Char -> Bool) -> Buffer -> [Buffer]
groupBy k = L.groupBy (\a b -> k (w2c a) (w2c b))

-- | /O(1)/ 'Buffer' index (subscript) operator, starting from 0.
index :: Buffer -> Int64 -> Char
index = (w2c .) . L.index
{-# INLINE index #-}

-- | /O(n)/ The 'elemIndex' function returns the index of the first
-- element in the given 'Buffer' which is equal (by memchr) to the
-- query element, or 'Nothing' if there is no such element.
elemIndex :: Char -> Buffer -> Maybe Int64
elemIndex = L.elemIndex . c2w
{-# INLINE elemIndex #-}

-- | /O(n)/ The 'elemIndices' function extends 'elemIndex', by returning
-- the indices of all elements equal to the query element, in ascending order.
elemIndices :: Char -> Buffer -> [Int64]
elemIndices = L.elemIndices . c2w
{-# INLINE elemIndices #-}

-- | The 'findIndex' function takes a predicate and a 'Buffer' and
-- returns the index of the first element in the Buffer satisfying the predicate.
findIndex :: (Char -> Bool) -> Buffer -> Maybe Int64
findIndex f = L.findIndex (f . w2c)
{-# INLINE findIndex #-}

-- | The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending order.
findIndices :: (Char -> Bool) -> Buffer -> [Int64]
findIndices f = L.findIndices (f . w2c)

-- | count returns the number of times its argument appears in the Buffer
--
-- > count      == length . elemIndices
-- > count '\n' == length . lines
--
-- But more efficiently than using length on the intermediate list.
count :: Char -> Buffer -> Int64
count c = L.count (c2w c)

-- | /O(n)/ 'elem' is the 'Buffer' membership predicate. This
-- implementation uses @memchr(3)@.
elem :: Char -> Buffer -> Bool
elem c = L.elem (c2w c)
{-# INLINE elem #-}

-- | /O(n)/ 'notElem' is the inverse of 'elem'
notElem :: Char -> Buffer -> Bool
notElem c = L.notElem (c2w c)
{-# INLINE notElem #-}

-- | /O(n)/ 'filter', applied to a predicate and a Buffer,
-- returns a Buffer containing those characters that satisfy the
-- predicate.
filter :: (Char -> Bool) -> Buffer -> Buffer
filter f = L.filter (f . w2c)
{-# INLINE filter #-}

{-
-- | /O(n)/ and /O(n\/c) space/ A first order equivalent of /filter .
-- (==)/, for the common case of filtering a single Char. It is more
-- efficient to use /filterChar/ in this case.
--
-- > filterChar == filter . (==)
--
-- filterChar is around 10x faster, and uses much less space, than its
-- filter equivalent
--
filterChar :: Char -> Buffer -> Buffer
filterChar c ps = replicate (count c ps) c
{-# INLINE filterChar #-}

{-# RULES
  "Buffer specialise filter (== x)" forall x.
      filter ((==) x) = filterChar x
  #-}

{-# RULES
  "Buffer specialise filter (== x)" forall x.
     filter (== x) = filterChar x
  #-}
-}

-- | /O(n)/ The 'find' function takes a predicate and a Buffer,
-- and returns the first element in matching the predicate, or 'Nothing'
-- if there is no such element.
find :: (Char -> Bool) -> Buffer -> Maybe Char
find f ps = w2c `fmap` L.find (f . w2c) ps
{-# INLINE find #-}

{-
-- | /O(n)/ A first order equivalent of /filter . (==)/, for the common
-- case of filtering a single Char. It is more efficient to use
-- filterChar in this case.
--
-- > filterChar == filter . (==)
--
-- filterChar is around 10x faster, and uses much less space, than its
-- filter equivalent
--
filterChar :: Char -> Buffer -> Buffer
filterChar c = L.filterByte (c2w c)
{-# INLINE filterChar #-}

-- | /O(n)/ A first order equivalent of /filter . (\/=)/, for the common
-- case of filtering a single Char out of a list. It is more efficient
-- to use /filterNotChar/ in this case.
--
-- > filterNotChar == filter . (/=)
--
-- filterNotChar is around 3x faster, and uses much less space, than its
-- filter equivalent
--
filterNotChar :: Char -> Buffer -> Buffer
filterNotChar c = L.filterNotByte (c2w c)
{-# INLINE filterNotChar #-}
-}

-- | /O(n)/ 'zip' takes two Buffers and returns a list of
-- corresponding pairs of Chars. If one input Buffer is short,
-- excess elements of the longer Buffer are discarded. This is
-- equivalent to a pair of 'unpack' operations, and so space
-- usage may be large for multi-megabyte Buffers
zip :: Buffer -> Buffer -> [(Char,Char)]
zip ps qs
    | L.null ps || L.null qs = []
    | otherwise = (head ps, head qs) : zip (L.tail ps) (L.tail qs)

-- | 'zipWith' generalises 'zip' by zipping with the function given as
-- the first argument, instead of a tupling function.  For example,
-- @'zipWith' (+)@ is applied to two Buffers to produce the list
-- of corresponding sums.
zipWith :: (Char -> Char -> a) -> Buffer -> Buffer -> [a]
zipWith f = L.zipWith ((. w2c) . f . w2c)

-- | 'lines' breaks a Buffer up into a list of Buffers at
-- newline Chars. The resulting strings do not contain newlines.
--
-- As of bytestring 0.9.0.3, this function is stricter than its 
-- list cousin.
--
lines :: Buffer -> [Buffer]
lines Empty          = []
lines (Chunk c0 cs0) = loop0 c0 cs0
    where
    -- this is a really performance sensitive function but the
    -- chunked representation makes the general case a bit expensive
    -- however assuming a large chunk size and normalish line lengths
    -- we will find line endings much more frequently than chunk
    -- endings so it makes sense to optimise for that common case.
    -- So we partition into two special cases depending on whether we
    -- are keeping back a list of chunks that will eventually be output
    -- once we get to the end of the current line.

    -- the common special case where we have no existing chunks of
    -- the current line
    loop0 :: S.Buffer -> Buffer -> [Buffer]
    loop0 c cs =
        case B.elemIndex (c2w '\n') c of
            Nothing -> case cs of
                           Empty  | B.null c  ->                 []
                                  | otherwise -> Chunk c Empty : []
                           (Chunk c' cs')
                               | B.null c  -> loop0 c'     cs'
                               | otherwise -> loop  c' [c] cs'

            Just n | n /= 0    -> Chunk (B.unsafeTake n c) Empty
                                : loop0 (B.unsafeDrop (n+1) c) cs
                   | otherwise -> Empty
                                : loop0 (B.unsafeTail c) cs

    -- the general case when we are building a list of chunks that are
    -- part of the same line
    loop :: S.Buffer -> [S.Buffer] -> Buffer -> [Buffer]
    loop c line cs =
        case B.elemIndex (c2w '\n') c of
            Nothing ->
                case cs of
                    Empty -> let c' = revChunks (c : line)
                              in c' `seq` (c' : [])

                    (Chunk c' cs') -> loop c' (c : line) cs'

            Just n ->
                let c' = revChunks (B.unsafeTake n c : line)
                 in c' `seq` (c' : loop0 (B.unsafeDrop (n+1) c) cs)

{-

This function is too strict!  Consider,

> prop_lazy =
    (L.unpack . head . lazylines $ L.append (L.pack "a\nb\n") (error "failed"))
  ==
    "a"

fails.  Here's a properly lazy version of 'lines' for lazy bytestrings

    lazylines           :: L.Buffer -> [L.Buffer]
    lazylines s
        | L.null s  = []
        | otherwise =
            let (l,s') = L.break ((==) '\n') s
            in l : if L.null s' then []
                                else lazylines (L.tail s')

we need a similarly lazy, but efficient version.

-}


-- | 'unlines' is an inverse operation to 'lines'.  It joins lines,
-- after appending a terminating newline to each.
unlines :: [Buffer] -> Buffer
unlines [] = empty
unlines ss = (concat $ List.intersperse nl ss) `append` nl -- half as much space
    where nl = singleton '\n'

-- | 'words' breaks a Buffer up into a list of words, which
-- were delimited by Chars representing white space. And
--
-- > tokens isSpace = words
--
words :: Buffer -> [Buffer]
words = List.filter (not . L.null) . L.splitWith isSpaceWord8
{-# INLINE words #-}

-- | The 'unwords' function is analogous to the 'unlines' function, on words.
unwords :: [Buffer] -> Buffer
unwords = intercalate (singleton ' ')
{-# INLINE unwords #-}

-- | readInt reads an Int from the beginning of the Buffer.  If
-- there is no integer at the beginning of the string, it returns
-- Nothing, otherwise it just returns the int read, and the rest of the
-- string.

{-
-- Faster:

data MaybeS = NothingS
            | JustS {-# UNPACK #-} !Int {-# UNPACK #-} !Buffer
-}

readInt :: Buffer -> Maybe (Int, Buffer)
{-# INLINE readInt #-}
readInt Empty        = Nothing
readInt (Chunk x xs) = case w2c (B.unsafeHead x) of
    '-' -> loop True  0 0 (B.unsafeTail x) xs
    '+' -> loop False 0 0 (B.unsafeTail x) xs
    _   -> loop False 0 0 x xs

    where loop :: Bool -> Int -> Int
                -> S.Buffer -> Buffer -> Maybe (Int, Buffer)
          STRICT5_(loop)
          loop neg i n c cs
              | B.null c = case cs of
                             Empty          -> end  neg i n c  cs
                             (Chunk c' cs') -> loop neg i n c' cs'
              | otherwise =
                  case B.unsafeHead c of
                    w | w >= 0x30
                     && w <= 0x39 -> loop neg (i+1)
                                          (n * 10 + (fromIntegral w - 0x30))
                                          (B.unsafeTail c) cs
                      | otherwise -> end neg i n c cs

          {-# INLINE end #-}
          end _   0 _ _  _ = Nothing
          end neg _ n c cs = e `seq` e
                where n' = if neg then negate n else n
                      c' = chunk c cs
                      e  = n' `seq` c' `seq` Just $! (n',c')
         --                  in n' `seq` c' `seq` JustS n' c'


-- | readInteger reads an Integer from the beginning of the Buffer.  If
-- there is no integer at the beginning of the string, it returns Nothing,
-- otherwise it just returns the int read, and the rest of the string.
readInteger :: Buffer -> Maybe (Integer, Buffer)
readInteger Empty = Nothing
readInteger (Chunk c0 cs0) =
        case w2c (B.unsafeHead c0) of
            '-' -> first (B.unsafeTail c0) cs0 >>= \(n, cs') -> return (-n, cs')
            '+' -> first (B.unsafeTail c0) cs0
            _   -> first c0 cs0

    where first c cs
              | B.null c = case cs of
                  Empty          -> Nothing
                  (Chunk c' cs') -> first' c' cs'
              | otherwise = first' c cs

          first' c cs = case B.unsafeHead c of
              w | w >= 0x30 && w <= 0x39 -> Just $
                  loop 1 (fromIntegral w - 0x30) [] (B.unsafeTail c) cs
                | otherwise              -> Nothing

          loop :: Int -> Int -> [Integer]
               -> S.Buffer -> Buffer -> (Integer, Buffer)
          STRICT5_(loop)
          loop d acc ns c cs
              | B.null c = case cs of
                             Empty          -> combine d acc ns c cs
                             (Chunk c' cs') -> loop d acc ns c' cs'
              | otherwise =
                  case B.unsafeHead c of
                   w | w >= 0x30 && w <= 0x39 ->
                       if d < 9 then loop (d+1)
                                          (10*acc + (fromIntegral w - 0x30))
                                          ns (B.unsafeTail c) cs
                                else loop 1 (fromIntegral w - 0x30)
                                          (fromIntegral acc : ns)
                                          (B.unsafeTail c) cs
                     | otherwise -> combine d acc ns c cs

          combine _ acc [] c cs = end (fromIntegral acc) c cs
          combine d acc ns c cs =
              end (10^d * combine1 1000000000 ns + fromIntegral acc) c cs

          combine1 _ [n] = n
          combine1 b ns  = combine1 (b*b) $ combine2 b ns

          combine2 b (n:m:ns) = let t = n+m*b in t `seq` (t : combine2 b ns)
          combine2 _ ns       = ns

          end n c cs = let c' = chunk c cs
                        in c' `seq` (n, c')

-- | Read an entire file /lazily/ into a 'Buffer'. Use 'text mode'
-- on Windows to interpret newlines
readFile :: FilePath -> IO Buffer
readFile f = openFile f ReadMode >>= hGetContents

-- | Write a 'Buffer' to a file.
writeFile :: FilePath -> Buffer -> IO ()
writeFile f txt = bracket (openFile f WriteMode) hClose
    (\hdl -> hPut hdl txt)

-- | Append a 'Buffer' to a file.
appendFile :: FilePath -> Buffer -> IO ()
appendFile f txt = bracket (openFile f AppendMode) hClose
    (\hdl -> hPut hdl txt)


-- ---------------------------------------------------------------------
-- Internal utilities

-- reverse a list of possibly-empty chunks into a lazy Buffer
revChunks :: [S.Buffer] -> Buffer
revChunks cs = List.foldl' (flip chunk) Empty cs
