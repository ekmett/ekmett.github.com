{-# LANGUAGE CPP #-}
-- We cannot actually specify all the language pragmas, see ghc ticket #
-- If we could, these are what they would be:
{- LANGUAGE MagicHash, UnboxedTuples -}
{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module      : Data.Buffer.Char8
-- Copyright   : (c) Don Stewart 2006-2008, (c) Edward Kmett 2009-2010
-- License     : BSD-style
--
-- Maintainer  : ekmett@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Manipulate 'Buffer's using 'Char' operations. All Chars will be
-- truncated to 8 bits. It can be expected that these functions will run
-- at identical speeds to their 'Word8' equivalents in "Data.Buffer".
--
-- More specifically these byte strings are taken to be in the
-- subset of Unicode covered by code points 0-255. This covers
-- Unicode Basic Latin, Latin-1 Supplement and C0+C1 Controls.
-- 
-- See: 
--
--  * <http://www.unicode.org/charts/>
--
--  * <http://www.unicode.org/charts/PDF/U0000.pdf>
--
--  * <http://www.unicode.org/charts/PDF/U0080.pdf>
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions.  eg.
--
-- > import qualified Data.Buffer.Char8 as B
--
-- The Char8 interface to bytestrings provides an instance of IsString
-- for the Buffer type, enabling you to use string literals, and
-- have them implicitly packed to Buffers. Use -XOverloadedStrings
-- to enable this.
--

module Data.Buffer.Char8 (

        -- * The @Buffer@ type
        Buffer,             -- abstract, instances: Eq, Ord, Show, Read, Data, Typeable, Monoid

        -- * Introducing and eliminating 'Buffer's
        empty,                  -- :: Buffer
        singleton,              -- :: Char   -> Buffer
        pack,                   -- :: String -> Buffer
        unpack,                 -- :: Buffer -> String

        -- * Basic interface
        cons,                   -- :: Char -> Buffer -> Buffer
        snoc,                   -- :: Buffer -> Char -> Buffer
        append,                 -- :: Buffer -> Buffer -> Buffer
        head,                   -- :: Buffer -> Char
        uncons,                 -- :: Buffer -> Maybe (Char, Buffer)
        last,                   -- :: Buffer -> Char
        tail,                   -- :: Buffer -> Buffer
        init,                   -- :: Buffer -> Buffer
        null,                   -- :: Buffer -> Bool
        length,                 -- :: Buffer -> Int

        -- * Transformating Buffers
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
        foldr',                 -- :: (Char -> a -> a) -> a -> Buffer -> a
        foldr1,                 -- :: (Char -> Char -> Char) -> Buffer -> Char
        foldr1',                -- :: (Char -> Char -> Char) -> Buffer -> Char

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
        scanl1,                 -- :: (Char -> Char -> Char) -> Buffer -> Buffer
        scanr,                  -- :: (Char -> Char -> Char) -> Char -> Buffer -> Buffer
        scanr1,                 -- :: (Char -> Char -> Char) -> Buffer -> Buffer

        -- ** Accumulating maps
        mapAccumL,              -- :: (acc -> Char -> (acc, Char)) -> acc -> Buffer -> (acc, Buffer)
        mapAccumR,              -- :: (acc -> Char -> (acc, Char)) -> acc -> Buffer -> (acc, Buffer)

        -- ** Generating and unfolding Buffers
        replicate,              -- :: Int -> Char -> Buffer
        unfoldr,                -- :: (a -> Maybe (Char, a)) -> a -> Buffer
        unfoldrN,               -- :: Int -> (a -> Maybe (Char, a)) -> a -> (Buffer, Maybe a)

        -- * Substrings

        -- ** Breaking strings
        take,                   -- :: Int -> Buffer -> Buffer
        drop,                   -- :: Int -> Buffer -> Buffer
        splitAt,                -- :: Int -> Buffer -> (Buffer, Buffer)
        takeWhile,              -- :: (Char -> Bool) -> Buffer -> Buffer
        dropWhile,              -- :: (Char -> Bool) -> Buffer -> Buffer
        span,                   -- :: (Char -> Bool) -> Buffer -> (Buffer, Buffer)
        spanEnd,                -- :: (Char -> Bool) -> Buffer -> (Buffer, Buffer)
        break,                  -- :: (Char -> Bool) -> Buffer -> (Buffer, Buffer)
        breakEnd,               -- :: (Char -> Bool) -> Buffer -> (Buffer, Buffer)
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
        isSuffixOf,             -- :: Buffer -> Buffer -> Bool
        isInfixOf,              -- :: Buffer -> Buffer -> Bool

        -- ** Search for arbitrary substrings
        breakSubstring,         -- :: Buffer -> Buffer -> (Buffer,Buffer)
        findSubstring,          -- :: Buffer -> Buffer -> Maybe Int
        findSubstrings,         -- :: Buffer -> Buffer -> [Int]

        -- * Searching Buffers

        -- ** Searching by equality
        elem,                   -- :: Char -> Buffer -> Bool
        notElem,                -- :: Char -> Buffer -> Bool

        -- ** Searching with a predicate
        find,                   -- :: (Char -> Bool) -> Buffer -> Maybe Char
        filter,                 -- :: (Char -> Bool) -> Buffer -> Buffer
--      partition               -- :: (Char -> Bool) -> Buffer -> (Buffer, Buffer)

        -- * Indexing Buffers
        index,                  -- :: Buffer -> Int -> Char
        elemIndex,              -- :: Char -> Buffer -> Maybe Int
        elemIndices,            -- :: Char -> Buffer -> [Int]
        elemIndexEnd,           -- :: Char -> Buffer -> Maybe Int
        findIndex,              -- :: (Char -> Bool) -> Buffer -> Maybe Int
        findIndices,            -- :: (Char -> Bool) -> Buffer -> [Int]
        count,                  -- :: Char -> Buffer -> Int

        -- * Zipping and unzipping Buffers
        zip,                    -- :: Buffer -> Buffer -> [(Char,Char)]
        zipWith,                -- :: (Char -> Char -> c) -> Buffer -> Buffer -> [c]
        unzip,                  -- :: [(Char,Char)] -> (Buffer,Buffer)

        -- * Ordered Buffers
        sort,                   -- :: Buffer -> Buffer

        -- * Reading from Buffers
        readInt,                -- :: Buffer -> Maybe (Int, Buffer)
        readInteger,            -- :: Buffer -> Maybe (Integer, Buffer)

        -- * Low level CString conversions

        -- ** Copying Buffers
        copy,                   -- :: Buffer -> Buffer

        -- ** Packing CStrings and pointers
        packCString,            -- :: CString -> IO Buffer
        packCStringLen,         -- :: CStringLen -> IO Buffer

        -- ** Using Buffers as CStrings
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
--      mmapFile,               -- :: FilePath -> IO Buffer

        -- ** I\/O with Handles
        hGetLine,               -- :: Handle -> IO Buffer
        hGetContents,           -- :: Handle -> IO Buffer
        hGet,                   -- :: Handle -> Int -> IO Buffer
        hGetNonBlocking,        -- :: Handle -> Int -> IO Buffer
        hPut,                   -- :: Handle -> Buffer -> IO ()
        hPutStr,                -- :: Handle -> Buffer -> IO ()
        hPutStrLn,              -- :: Handle -> Buffer -> IO ()

  ) where

import qualified Prelude as P
import Prelude hiding           (reverse,head,tail,last,init,null
                                ,length,map,lines,foldl,foldr,unlines
                                ,concat,any,take,drop,splitAt,takeWhile
                                ,dropWhile,span,break,elem,filter,unwords
                                ,words,maximum,minimum,all,concatMap
                                ,scanl,scanl1,scanr,scanr1
                                ,appendFile,readFile,writeFile
                                ,foldl1,foldr1,replicate
                                ,getContents,getLine,putStr,putStrLn,interact
                                ,zip,zipWith,unzip,notElem)

import qualified Data.Buffer.Word8 as B
import qualified Data.Buffer.Internal as B
import qualified Data.Buffer.Unsafe as B

-- Listy functions transparently exported
import Data.Buffer (empty,null,length,tail,init,append
                       ,inits,tails,reverse,transpose
                       ,concat,take,drop,splitAt,intercalate
                       ,sort,isPrefixOf,isSuffixOf,isInfixOf
                       ,findSubstring,findSubstrings,breakSubstring,copy,group

                       ,getLine, getContents, putStr, putStrLn, interact
                       ,hGetContents, hGet, hPut, hPutStr, hPutStrLn
                       ,hGetLine, hGetNonBlocking
                       ,packCString,packCStringLen
                       ,useAsCString,useAsCStringLen
                       )

import Data.Buffer.Internal (Buffer(PS), c2w, w2c, isSpaceWord8
                                ,inlinePerformIO)

import Data.Char    ( isSpace )
import qualified Data.List as List (intersperse)

import System.IO                (openFile,hClose,hFileSize,IOMode(..))
#ifndef __NHC__
import Control.Exception        (bracket)
#else
import IO			(bracket)
#endif
import Foreign

#if defined(__GLASGOW_HASKELL__)
import GHC.Base                 (Char(..),unpackCString#,ord#,int2Word#)
#if __GLASGOW_HASKELL__ >= 611
import GHC.IO                   (stToIO)
#else
import GHC.IOBase               (stToIO)
#endif
import GHC.Prim                 (Addr#,writeWord8OffAddr#,plusAddr#)
import GHC.Ptr                  (Ptr(..))
import GHC.ST                   (ST(..))
#endif

#if __GLASGOW_HASKELL__ >= 608
import Data.String
#endif

#define STRICT1(f) f a | a `seq` False = undefined
#define STRICT2(f) f a b | a `seq` b `seq` False = undefined
#define STRICT3(f) f a b c | a `seq` b `seq` c `seq` False = undefined
#define STRICT4(f) f a b c d | a `seq` b `seq` c `seq` d `seq` False = undefined

------------------------------------------------------------------------

-- | /O(1)/ Convert a 'Char' into a 'Buffer'
singleton :: Char -> Buffer
singleton = B.singleton . c2w
{-# INLINE singleton #-}

#if __GLASGOW_HASKELL__ >= 608
instance IsString Buffer where
    fromString = pack
    {-# INLINE fromString #-}
#endif

-- | /O(n)/ Convert a 'String' into a 'Buffer'
--
-- For applications with large numbers of string literals, pack can be a
-- bottleneck.
pack :: String -> Buffer
#if !defined(__GLASGOW_HASKELL__)

pack str = B.unsafeCreate (P.length str) $ \p -> go p str
    where go _ []     = return ()
          go p (x:xs) = poke p (c2w x) >> go (p `plusPtr` 1) xs

#else /* hack away */

pack str = B.unsafeCreate (P.length str) $ \(Ptr p) -> stToIO (go p str)
  where
    go :: Addr# -> [Char] -> ST a ()
    go _ []        = return ()
    go p (C# c:cs) = writeByte p (int2Word# (ord# c)) >> go (p `plusAddr#` 1#) cs

    writeByte p c = ST $ \s# ->
        case writeWord8OffAddr# p 0# c s# of s2# -> (# s2#, () #)
    {-# INLINE writeByte #-}
{-# INLINE [1] pack #-}

{-# RULES
"Buffer pack/packAddress" forall s .
   pack (unpackCString# s) = inlinePerformIO (B.unsafePackAddress s)
 #-}

#endif

-- | /O(n)/ Converts a 'Buffer' to a 'String'.
unpack :: Buffer -> [Char]
unpack = P.map w2c . B.unpack
{-# INLINE unpack #-}

-- | /O(n)/ 'cons' is analogous to (:) for lists, but of different
-- complexity, as it requires a memcpy.
cons :: Char -> Buffer -> Buffer
cons = B.cons . c2w
{-# INLINE cons #-}

-- | /O(n)/ Append a Char to the end of a 'Buffer'. Similar to
-- 'cons', this function performs a memcpy.
snoc :: Buffer -> Char -> Buffer
snoc p = B.snoc p . c2w
{-# INLINE snoc #-}

-- | /O(1)/ Extract the head and tail of a Buffer, returning Nothing
-- if it is empty.
uncons :: Buffer -> Maybe (Char, Buffer)
uncons bs = case B.uncons bs of
                  Nothing -> Nothing
                  Just (w, bs') -> Just (w2c w, bs')
{-# INLINE uncons #-}

-- | /O(1)/ Extract the first element of a Buffer, which must be non-empty.
head :: Buffer -> Char
head = w2c . B.head
{-# INLINE head #-}

-- | /O(1)/ Extract the last element of a packed string, which must be non-empty.
last :: Buffer -> Char
last = w2c . B.last
{-# INLINE last #-}

-- | /O(n)/ 'map' @f xs@ is the Buffer obtained by applying @f@ to each element of @xs@
map :: (Char -> Char) -> Buffer -> Buffer
map f = B.map (c2w . f . w2c)
{-# INLINE map #-}

-- | /O(n)/ The 'intersperse' function takes a Char and a 'Buffer'
-- and \`intersperses\' that Char between the elements of the
-- 'Buffer'.  It is analogous to the intersperse function on Lists.
intersperse :: Char -> Buffer -> Buffer
intersperse = B.intersperse . c2w
{-# INLINE intersperse #-}

-- | 'foldl', applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and a Buffer, reduces the
-- Buffer using the binary operator, from left to right.
foldl :: (a -> Char -> a) -> a -> Buffer -> a
foldl f = B.foldl (\a c -> f a (w2c c))
{-# INLINE foldl #-}

-- | 'foldl\'' is like foldl, but strict in the accumulator.
foldl' :: (a -> Char -> a) -> a -> Buffer -> a
foldl' f = B.foldl' (\a c -> f a (w2c c))
{-# INLINE foldl' #-}

-- | 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a packed string,
-- reduces the packed string using the binary operator, from right to left.
foldr :: (Char -> a -> a) -> a -> Buffer -> a
foldr f = B.foldr (\c a -> f (w2c c) a)
{-# INLINE foldr #-}

-- | 'foldr\'' is a strict variant of foldr
foldr' :: (Char -> a -> a) -> a -> Buffer -> a
foldr' f = B.foldr' (\c a -> f (w2c c) a)
{-# INLINE foldr' #-}

-- | 'foldl1' is a variant of 'foldl' that has no starting value
-- argument, and thus must be applied to non-empty 'Buffers'.
foldl1 :: (Char -> Char -> Char) -> Buffer -> Char
foldl1 f ps = w2c (B.foldl1 (\x y -> c2w (f (w2c x) (w2c y))) ps)
{-# INLINE foldl1 #-}

-- | A strict version of 'foldl1'
foldl1' :: (Char -> Char -> Char) -> Buffer -> Char
foldl1' f ps = w2c (B.foldl1' (\x y -> c2w (f (w2c x) (w2c y))) ps)
{-# INLINE foldl1' #-}

-- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- and thus must be applied to non-empty 'Buffer's
foldr1 :: (Char -> Char -> Char) -> Buffer -> Char
foldr1 f ps = w2c (B.foldr1 (\x y -> c2w (f (w2c x) (w2c y))) ps)
{-# INLINE foldr1 #-}

-- | A strict variant of foldr1
foldr1' :: (Char -> Char -> Char) -> Buffer -> Char
foldr1' f ps = w2c (B.foldr1' (\x y -> c2w (f (w2c x) (w2c y))) ps)
{-# INLINE foldr1' #-}

-- | Map a function over a 'Buffer' and concatenate the results
concatMap :: (Char -> Buffer) -> Buffer -> Buffer
concatMap f = B.concatMap (f . w2c)
{-# INLINE concatMap #-}

-- | Applied to a predicate and a Buffer, 'any' determines if
-- any element of the 'Buffer' satisfies the predicate.
any :: (Char -> Bool) -> Buffer -> Bool
any f = B.any (f . w2c)
{-# INLINE any #-}

-- | Applied to a predicate and a 'Buffer', 'all' determines if
-- all elements of the 'Buffer' satisfy the predicate.
all :: (Char -> Bool) -> Buffer -> Bool
all f = B.all (f . w2c)
{-# INLINE all #-}

-- | 'maximum' returns the maximum value from a 'Buffer'
maximum :: Buffer -> Char
maximum = w2c . B.maximum
{-# INLINE maximum #-}

-- | 'minimum' returns the minimum value from a 'Buffer'
minimum :: Buffer -> Char
minimum = w2c . B.minimum
{-# INLINE minimum #-}

-- | The 'mapAccumL' function behaves like a combination of 'map' and
-- 'foldl'; it applies a function to each element of a Buffer,
-- passing an accumulating parameter from left to right, and returning a
-- final value of this accumulator together with the new list.
mapAccumL :: (acc -> Char -> (acc, Char)) -> acc -> Buffer -> (acc, Buffer)
mapAccumL f = B.mapAccumL (\acc w -> case f acc (w2c w) of (acc', c) -> (acc', c2w c))

-- | The 'mapAccumR' function behaves like a combination of 'map' and
-- 'foldr'; it applies a function to each element of a Buffer,
-- passing an accumulating parameter from right to left, and returning a
-- final value of this accumulator together with the new Buffer.
mapAccumR :: (acc -> Char -> (acc, Char)) -> acc -> Buffer -> (acc, Buffer)
mapAccumR f = B.mapAccumR (\acc w -> case f acc (w2c w) of (acc', c) -> (acc', c2w c))

-- | 'scanl' is similar to 'foldl', but returns a list of successive
-- reduced values from the left:
--
-- > scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
--
-- Note that
--
-- > last (scanl f z xs) == foldl f z xs.
scanl :: (Char -> Char -> Char) -> Char -> Buffer -> Buffer
scanl f z = B.scanl (\a b -> c2w (f (w2c a) (w2c b))) (c2w z)

-- | 'scanl1' is a variant of 'scanl' that has no starting value argument:
--
-- > scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]
scanl1 :: (Char -> Char -> Char) -> Buffer -> Buffer
scanl1 f = B.scanl1 (\a b -> c2w (f (w2c a) (w2c b)))

-- | scanr is the right-to-left dual of scanl.
scanr :: (Char -> Char -> Char) -> Char -> Buffer -> Buffer
scanr f z = B.scanr (\a b -> c2w (f (w2c a) (w2c b))) (c2w z)

-- | 'scanr1' is a variant of 'scanr' that has no starting value argument.
scanr1 :: (Char -> Char -> Char) -> Buffer -> Buffer
scanr1 f = B.scanr1 (\a b -> c2w (f (w2c a) (w2c b)))

-- | /O(n)/ 'replicate' @n x@ is a Buffer of length @n@ with @x@
-- the value of every element. The following holds:
--
-- > replicate w c = unfoldr w (\u -> Just (u,u)) c
--
-- This implemenation uses @memset(3)@
replicate :: Int -> Char -> Buffer
replicate w = B.replicate w . c2w
{-# INLINE replicate #-}

-- | /O(n)/, where /n/ is the length of the result.  The 'unfoldr' 
-- function is analogous to the List \'unfoldr\'.  'unfoldr' builds a 
-- Buffer from a seed value.  The function takes the element and 
-- returns 'Nothing' if it is done producing the Buffer or returns 
-- 'Just' @(a,b)@, in which case, @a@ is the next character in the string, 
-- and @b@ is the seed value for further production.
--
-- Examples:
--
-- > unfoldr (\x -> if x <= '9' then Just (x, succ x) else Nothing) '0' == "0123456789"
unfoldr :: (a -> Maybe (Char, a)) -> a -> Buffer
unfoldr f x0 = B.unfoldr (fmap k . f) x0
    where k (i, j) = (c2w i, j)

-- | /O(n)/ Like 'unfoldr', 'unfoldrN' builds a Buffer from a seed
-- value.  However, the length of the result is limited by the first
-- argument to 'unfoldrN'.  This function is more efficient than 'unfoldr'
-- when the maximum length of the result is known.
--
-- The following equation relates 'unfoldrN' and 'unfoldr':
--
-- > unfoldrN n f s == take n (unfoldr f s)
unfoldrN :: Int -> (a -> Maybe (Char, a)) -> a -> (Buffer, Maybe a)
unfoldrN n f w = B.unfoldrN n ((k `fmap`) . f) w
    where k (i,j) = (c2w i, j)
{-# INLINE unfoldrN #-}

-- | 'takeWhile', applied to a predicate @p@ and a Buffer @xs@,
-- returns the longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@.
takeWhile :: (Char -> Bool) -> Buffer -> Buffer
takeWhile f = B.takeWhile (f . w2c)
{-# INLINE takeWhile #-}

-- | 'dropWhile' @p xs@ returns the suffix remaining after 'takeWhile' @p xs@.
dropWhile :: (Char -> Bool) -> Buffer -> Buffer
dropWhile f = B.dropWhile (f . w2c)
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] dropWhile #-}
#endif

{-# RULES
"Buffer specialise dropWhile isSpace -> dropSpace"
    dropWhile isSpace = dropSpace
  #-}

-- | 'break' @p@ is equivalent to @'span' ('not' . p)@.
break :: (Char -> Bool) -> Buffer -> (Buffer, Buffer)
break f = B.break (f . w2c)
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] break #-}
#endif

#if __GLASGOW_HASKELL__ >= 606
-- This RULE LHS is not allowed by ghc-6.4
{-# RULES
"Buffer specialise break (x==)" forall x.
    break ((==) x) = breakChar x
"Buffer specialise break (==x)" forall x.
    break (==x) = breakChar x
  #-}
#endif

-- INTERNAL:

-- | 'breakChar' breaks its Buffer argument at the first occurence
-- of the specified char. It is more efficient than 'break' as it is
-- implemented with @memchr(3)@. I.e.
-- 
-- > break (=='c') "abcd" == breakChar 'c' "abcd"
--
breakChar :: Char -> Buffer -> (Buffer, Buffer)
breakChar c p = case elemIndex c p of
    Nothing -> (p,empty)
    Just n  -> (B.unsafeTake n p, B.unsafeDrop n p)
{-# INLINE breakChar #-}

-- | 'span' @p xs@ breaks the Buffer into two segments. It is
-- equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@
span :: (Char -> Bool) -> Buffer -> (Buffer, Buffer)
span f = B.span (f . w2c)
{-# INLINE span #-}

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
spanEnd :: (Char -> Bool) -> Buffer -> (Buffer, Buffer)
spanEnd f = B.spanEnd (f . w2c)
{-# INLINE spanEnd #-}

-- | 'breakEnd' behaves like 'break' but from the end of the 'Buffer'
-- 
-- breakEnd p == spanEnd (not.p)
breakEnd :: (Char -> Bool) -> Buffer -> (Buffer, Buffer)
breakEnd f = B.breakEnd (f . w2c)
{-# INLINE breakEnd #-}

{-
-- | 'breakChar' breaks its Buffer argument at the first occurence
-- of the specified Char. It is more efficient than 'break' as it is
-- implemented with @memchr(3)@. I.e.
-- 
-- > break (=='c') "abcd" == breakChar 'c' "abcd"
--
breakChar :: Char -> Buffer -> (Buffer, Buffer)
breakChar = B.breakByte . c2w
{-# INLINE breakChar #-}

-- | 'spanChar' breaks its Buffer argument at the first
-- occurence of a Char other than its argument. It is more efficient
-- than 'span (==)'
--
-- > span  (=='c') "abcd" == spanByte 'c' "abcd"
--
spanChar :: Char -> Buffer -> (Buffer, Buffer)
spanChar = B.spanByte . c2w
{-# INLINE spanChar #-}
-}

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
split :: Char -> Buffer -> [Buffer]
split = B.split . c2w
{-# INLINE split #-}

-- | /O(n)/ Splits a 'Buffer' into components delimited by
-- separators, where the predicate returns True for a separator element.
-- The resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > splitWith (=='a') "aabbaca" == ["","","bb","c",""]
--
splitWith :: (Char -> Bool) -> Buffer -> [Buffer]
splitWith f = B.splitWith (f . w2c)
{-# INLINE splitWith #-}
-- the inline makes a big difference here.

{-
-- | Like 'splitWith', except that sequences of adjacent separators are
-- treated as a single separator. eg.
-- 
-- > tokens (=='a') "aabbaca" == ["bb","c"]
--
tokens :: (Char -> Bool) -> Buffer -> [Buffer]
tokens f = B.tokens (f . w2c)
{-# INLINE tokens #-}
-}

-- | The 'groupBy' function is the non-overloaded version of 'group'.
groupBy :: (Char -> Char -> Bool) -> Buffer -> [Buffer]
groupBy k = B.groupBy (\a b -> k (w2c a) (w2c b))

-- | /O(1)/ 'Buffer' index (subscript) operator, starting from 0.
index :: Buffer -> Int -> Char
index = (w2c .) . B.index
{-# INLINE index #-}

-- | /O(n)/ The 'elemIndex' function returns the index of the first
-- element in the given 'Buffer' which is equal (by memchr) to the
-- query element, or 'Nothing' if there is no such element.
elemIndex :: Char -> Buffer -> Maybe Int
elemIndex = B.elemIndex . c2w
{-# INLINE elemIndex #-}

-- | /O(n)/ The 'elemIndexEnd' function returns the last index of the
-- element in the given 'Buffer' which is equal to the query
-- element, or 'Nothing' if there is no such element. The following
-- holds:
--
-- > elemIndexEnd c xs == 
-- > (-) (length xs - 1) `fmap` elemIndex c (reverse xs)
--
elemIndexEnd :: Char -> Buffer -> Maybe Int
elemIndexEnd = B.elemIndexEnd . c2w
{-# INLINE elemIndexEnd #-}

-- | /O(n)/ The 'elemIndices' function extends 'elemIndex', by returning
-- the indices of all elements equal to the query element, in ascending order.
elemIndices :: Char -> Buffer -> [Int]
elemIndices = B.elemIndices . c2w
{-# INLINE elemIndices #-}

-- | The 'findIndex' function takes a predicate and a 'Buffer' and
-- returns the index of the first element in the Buffer satisfying the predicate.
findIndex :: (Char -> Bool) -> Buffer -> Maybe Int
findIndex f = B.findIndex (f . w2c)
{-# INLINE findIndex #-}

-- | The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending order.
findIndices :: (Char -> Bool) -> Buffer -> [Int]
findIndices f = B.findIndices (f . w2c)

-- | count returns the number of times its argument appears in the Buffer
--
-- > count = length . elemIndices
-- 
-- Also
--  
-- > count '\n' == length . lines
--
-- But more efficiently than using length on the intermediate list.
count :: Char -> Buffer -> Int
count c = B.count (c2w c)

-- | /O(n)/ 'elem' is the 'Buffer' membership predicate. This
-- implementation uses @memchr(3)@.
elem :: Char -> Buffer -> Bool
elem    c = B.elem (c2w c)
{-# INLINE elem #-}

-- | /O(n)/ 'notElem' is the inverse of 'elem'
notElem :: Char -> Buffer -> Bool
notElem c = B.notElem (c2w c)
{-# INLINE notElem #-}

-- | /O(n)/ 'filter', applied to a predicate and a Buffer,
-- returns a Buffer containing those characters that satisfy the
-- predicate.
filter :: (Char -> Bool) -> Buffer -> Buffer
filter f = B.filter (f . w2c)
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
"Buffer specialise filter (== x)" forall x.
    filter (== x) = filterChar x
  #-}
-}

-- | /O(n)/ The 'find' function takes a predicate and a Buffer,
-- and returns the first element in matching the predicate, or 'Nothing'
-- if there is no such element.
find :: (Char -> Bool) -> Buffer -> Maybe Char
find f ps = w2c `fmap` B.find (f . w2c) ps
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
filterChar c = B.filterByte (c2w c)
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
filterNotChar c = B.filterNotByte (c2w c)
{-# INLINE filterNotChar #-}
-}

-- | /O(n)/ 'zip' takes two Buffers and returns a list of
-- corresponding pairs of Chars. If one input Buffer is short,
-- excess elements of the longer Buffer are discarded. This is
-- equivalent to a pair of 'unpack' operations, and so space
-- usage may be large for multi-megabyte Buffers
zip :: Buffer -> Buffer -> [(Char,Char)]
zip ps qs
    | B.null ps || B.null qs = []
    | otherwise = (unsafeHead ps, unsafeHead qs) : zip (B.unsafeTail ps) (B.unsafeTail qs)

-- | 'zipWith' generalises 'zip' by zipping with the function given as
-- the first argument, instead of a tupling function.  For example,
-- @'zipWith' (+)@ is applied to two Buffers to produce the list
-- of corresponding sums.
zipWith :: (Char -> Char -> a) -> Buffer -> Buffer -> [a]
zipWith f = B.zipWith ((. w2c) . f . w2c)

-- | 'unzip' transforms a list of pairs of Chars into a pair of
-- Buffers. Note that this performs two 'pack' operations.
unzip :: [(Char,Char)] -> (Buffer,Buffer)
unzip ls = (pack (P.map fst ls), pack (P.map snd ls))
{-# INLINE unzip #-}

-- | A variety of 'head' for non-empty Buffers. 'unsafeHead' omits
-- the check for the empty case, which is good for performance, but
-- there is an obligation on the programmer to provide a proof that the
-- Buffer is non-empty.
unsafeHead :: Buffer -> Char
unsafeHead  = w2c . B.unsafeHead
{-# INLINE unsafeHead #-}

-- ---------------------------------------------------------------------
-- Things that depend on the encoding

{-# RULES
"Buffer specialise break -> breakSpace"
    break isSpace = breakSpace
  #-}

-- | 'breakSpace' returns the pair of Buffers when the argument is
-- broken at the first whitespace byte. I.e.
-- 
-- > break isSpace == breakSpace
--
breakSpace :: Buffer -> (Buffer,Buffer)
breakSpace (PS x s l) = inlinePerformIO $ withForeignPtr x $ \p -> do
    i <- firstspace (p `plusPtr` s) 0 l
    return $! case () of {_
        | i == 0    -> (empty, PS x s l)
        | i == l    -> (PS x s l, empty)
        | otherwise -> (PS x s i, PS x (s+i) (l-i))
    }
{-# INLINE breakSpace #-}

firstspace :: Ptr Word8 -> Int -> Int -> IO Int
STRICT3(firstspace)
firstspace ptr n m
    | n >= m    = return n
    | otherwise = do w <- peekByteOff ptr n
                     if (not . isSpaceWord8) w then firstspace ptr (n+1) m else return n

-- | 'dropSpace' efficiently returns the 'Buffer' argument with
-- white space Chars removed from the front. It is more efficient than
-- calling dropWhile for removing whitespace. I.e.
-- 
-- > dropWhile isSpace == dropSpace
--
dropSpace :: Buffer -> Buffer
dropSpace (PS x s l) = inlinePerformIO $ withForeignPtr x $ \p -> do
    i <- firstnonspace (p `plusPtr` s) 0 l
    return $! if i == l then empty else PS x (s+i) (l-i)
{-# INLINE dropSpace #-}

firstnonspace :: Ptr Word8 -> Int -> Int -> IO Int
STRICT3(firstnonspace)
firstnonspace ptr n m
    | n >= m    = return n
    | otherwise = do w <- peekElemOff ptr n
                     if isSpaceWord8 w then firstnonspace ptr (n+1) m else return n

{-
-- | 'dropSpaceEnd' efficiently returns the 'Buffer' argument with
-- white space removed from the end. I.e.
-- 
-- > reverse . (dropWhile isSpace) . reverse == dropSpaceEnd
--
-- but it is more efficient than using multiple reverses.
--
dropSpaceEnd :: Buffer -> Buffer
dropSpaceEnd (PS x s l) = inlinePerformIO $ withForeignPtr x $ \p -> do
    i <- lastnonspace (p `plusPtr` s) (l-1)
    return $! if i == (-1) then empty else PS x s (i+1)
{-# INLINE dropSpaceEnd #-}

lastnonspace :: Ptr Word8 -> Int -> IO Int
STRICT2(lastnonspace)
lastnonspace ptr n
    | n < 0     = return n
    | otherwise = do w <- peekElemOff ptr n
                     if isSpaceWord8 w then lastnonspace ptr (n-1) else return n
-}

-- | 'lines' breaks a Buffer up into a list of Buffers at
-- newline Chars. The resulting strings do not contain newlines.
--
lines :: Buffer -> [Buffer]
lines ps
    | null ps = []
    | otherwise = case search ps of
             Nothing -> [ps]
             Just n  -> take n ps : lines (drop (n+1) ps)
    where search = elemIndex '\n'

{-
-- Just as fast, but more complex. Should be much faster, I thought.
lines :: Buffer -> [Buffer]
lines (PS _ _ 0) = []
lines (PS x s l) = inlinePerformIO $ withForeignPtr x $ \p -> do
        let ptr = p `plusPtr` s

            STRICT1(loop)
            loop n = do
                let q = memchr (ptr `plusPtr` n) 0x0a (fromIntegral (l-n))
                if q == nullPtr
                    then return [PS x (s+n) (l-n)]
                    else do let i = q `minusPtr` ptr
                            ls <- loop (i+1)
                            return $! PS x (s+n) (i-n) : ls
        loop 0
-}

-- | 'unlines' is an inverse operation to 'lines'.  It joins lines,
-- after appending a terminating newline to each.
unlines :: [Buffer] -> Buffer
unlines [] = empty
unlines ss = (concat $ List.intersperse nl ss) `append` nl -- half as much space
    where nl = singleton '\n'

-- | 'words' breaks a Buffer up into a list of words, which
-- were delimited by Chars representing white space.
words :: Buffer -> [Buffer]
words = P.filter (not . B.null) . B.splitWith isSpaceWord8
{-# INLINE words #-}

-- | The 'unwords' function is analogous to the 'unlines' function, on words.
unwords :: [Buffer] -> Buffer
unwords = intercalate (singleton ' ')
{-# INLINE unwords #-}

-- ---------------------------------------------------------------------
-- Reading from Buffers

-- | readInt reads an Int from the beginning of the Buffer.  If there is no
-- integer at the beginning of the string, it returns Nothing, otherwise
-- it just returns the int read, and the rest of the string.
readInt :: Buffer -> Maybe (Int, Buffer)
readInt as
    | null as   = Nothing
    | otherwise =
        case unsafeHead as of
            '-' -> loop True  0 0 (B.unsafeTail as)
            '+' -> loop False 0 0 (B.unsafeTail as)
            _   -> loop False 0 0 as

    where loop :: Bool -> Int -> Int -> Buffer -> Maybe (Int, Buffer)
          STRICT4(loop)
          loop neg i n ps
              | null ps   = end neg i n ps
              | otherwise =
                  case B.unsafeHead ps of
                    w | w >= 0x30
                     && w <= 0x39 -> loop neg (i+1)
                                          (n * 10 + (fromIntegral w - 0x30))
                                          (B.unsafeTail ps)
                      | otherwise -> end neg i n ps

          end _    0 _ _  = Nothing
          end True _ n ps = Just (negate n, ps)
          end _    _ n ps = Just (n, ps)

-- | readInteger reads an Integer from the beginning of the Buffer.  If
-- there is no integer at the beginning of the string, it returns Nothing,
-- otherwise it just returns the int read, and the rest of the string.
readInteger :: Buffer -> Maybe (Integer, Buffer)
readInteger as
    | null as   = Nothing
    | otherwise =
        case unsafeHead as of
            '-' -> first (B.unsafeTail as) >>= \(n, bs) -> return (-n, bs)
            '+' -> first (B.unsafeTail as)
            _   -> first as

    where first ps | null ps   = Nothing
                   | otherwise =
                       case B.unsafeHead ps of
                        w | w >= 0x30 && w <= 0x39 -> Just $
                            loop 1 (fromIntegral w - 0x30) [] (B.unsafeTail ps)
                          | otherwise              -> Nothing

          loop :: Int -> Int -> [Integer]
               -> Buffer -> (Integer, Buffer)
          STRICT4(loop)
          loop d acc ns ps
              | null ps   = combine d acc ns empty
              | otherwise =
                  case B.unsafeHead ps of
                   w | w >= 0x30 && w <= 0x39 ->
                       if d == 9 then loop 1 (fromIntegral w - 0x30)
                                           (toInteger acc : ns)
                                           (B.unsafeTail ps)
                                 else loop (d+1)
                                           (10*acc + (fromIntegral w - 0x30))
                                           ns (B.unsafeTail ps)
                     | otherwise -> combine d acc ns ps

          combine _ acc [] ps = (toInteger acc, ps)
          combine d acc ns ps =
              ((10^d * combine1 1000000000 ns + toInteger acc), ps)

          combine1 _ [n] = n
          combine1 b ns  = combine1 (b*b) $ combine2 b ns

          combine2 b (n:m:ns) = let t = m*b + n in t `seq` (t : combine2 b ns)
          combine2 _ ns       = ns

------------------------------------------------------------------------
-- For non-binary text processing:

-- | Read an entire file strictly into a 'Buffer'.  This is far more
-- efficient than reading the characters into a 'String' and then using
-- 'pack'.  It also may be more efficient than opening the file and
-- reading it using hGet.
readFile :: FilePath -> IO Buffer
readFile f = bracket (openFile f ReadMode) hClose
    (\h -> hFileSize h >>= hGet h . fromIntegral)

-- | Write a 'Buffer' to a file.
writeFile :: FilePath -> Buffer -> IO ()
writeFile f txt = bracket (openFile f WriteMode) hClose
    (\h -> hPut h txt)

-- | Append a 'Buffer' to a file.
appendFile :: FilePath -> Buffer -> IO ()
appendFile f txt = bracket (openFile f AppendMode) hClose
    (\h -> hPut h txt)

