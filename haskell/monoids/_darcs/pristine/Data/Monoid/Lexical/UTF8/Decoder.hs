{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Lexical.UTF8.Decoder
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (MPTCs)
--
-- UTF8 encoded unicode characters can be parsed both forwards and backwards,
-- since the start of each 'Char' is clearly marked. This 'Monoid' accumulates
-- information about the characters represented and reduces that information
-- using a 'CharReducer', which is just a 'Reducer' 'Monoid' that knows what 
-- it wants to do about an 'invalidChar' -- a  string of 'Word8' values that 
-- don't form a valid UTF8 character.
--
-- As this monoid parses chars it just feeds them upstream to the underlying
-- CharReducer. Efficient left-to-right and right-to-left traversals are 
-- supplied so that a lazy 'ByteString' can be parsed efficiently by 
-- chunking it into strict chunks, and batching the traversals over each
-- before stitching the edges together.
--
-- Because this needs to be a 'Monoid' and should return the exact same result
-- regardless of forward or backwards parsing, it chooses to parse only 
-- canonical UTF8 unlike most Haskell UTF8 parsers, which will blissfully 
-- accept illegal alternative long encodings of a character. 
--
-- This actually fixes a potential class of security issues in some scenarios:
--
-- <http://prowebdevelopmentblog.com/content/big-overhaul-java-utf-8-charset>
--
-- NB: Due to naive use of a list to track the tail of an unfinished character 
-- this may exhibit @O(n^2)@ behavior parsing backwards along an invalid sequence 
-- of a large number of bytes that all claim to be in the tail of a character.
--
-----------------------------------------------------------------------------


module Data.Monoid.Lexical.UTF8.Decoder 
    ( module Data.Monoid.Reducer.Char
    , UTF8
    , runUTF8
    ) where
    
import Data.Bits (shiftL,(.&.),(.|.))
import Data.Word (Word8)


import Control.Functor.Pointed

import Data.Monoid.Reducer.Char

-- Incrementally reduce canonical RFC3629 UTF-8 Characters

-- utf8 characters are at most 4 characters long, so we need only retain state for 3 of them
-- moreover their length is able to be determined a priori, so lets store that intrinsically in the constructor
data H = H0
       | H2_1 {-# UNPACK #-} !Word8 
       | H3_1 {-# UNPACK #-} !Word8
       | H3_2 {-# UNPACK #-} !Word8 !Word8
       | H4_1 {-# UNPACK #-} !Word8
       | H4_2 {-# UNPACK #-} !Word8 !Word8
       | H4_3 {-# UNPACK #-} !Word8 !Word8 !Word8

-- words expressing the tail of a character, each between 0x80 and 0xbf
-- this is arbitrary length to simplify making the parser truly monoidal
-- this probably means we have O(n^2) worst case performance in the face of very long runs of chars that look like 10xxxxxx
type T = [Word8]

-- S is a segment that contains a possible tail of a character, the result of reducing some full characters, and the start of another character
-- T contains a list of bytes each between 0x80 and 0xbf
data UTF8 m = S T m !H
            | T T

-- flush any extra characters in a head, when the next character isn't between 0x80 and 0xbf
flushH :: CharReducer m => H -> m
flushH (H0) = mempty
flushH (H2_1 x) = invalidChar [x]
flushH (H3_1 x) = invalidChar [x]
flushH (H3_2 x y) = invalidChar [x,y]
flushH (H4_1 x) = invalidChar [x]
flushH (H4_2 x y) = invalidChar [x,y]
flushH (H4_3 x y z) = invalidChar [x,y,z]

-- flush a character tail 
flushT :: CharReducer m => [Word8] -> m
flushT = invalidChar

snocH :: CharReducer m => H -> Word8 -> (m -> H -> UTF8 m) -> m -> UTF8 m
snocH H0 c k m 
    | c < 0x80 = k (m `mappend` b1 c) H0
    | c < 0xc0 = k (m `mappend` invalidChar [c]) H0
    | c < 0xe0 = k m (H2_1 c)
    | c < 0xf0 = k m (H3_1 c)
    | c < 0xf5 = k m (H4_1 c)
    | otherwise = k (m `mappend` invalidChar [c]) H0
snocH (H2_1 c) d k m
    | d >= 0x80 && d < 0xc0 = k (m `mappend` b2 c d) H0
    | otherwise = k (m `mappend` invalidChar [c]) H0
snocH (H3_1 c) d k m 
    | d >= 0x80 && d < 0xc0 = k m (H3_2 c d)
    | otherwise = k (m `mappend` invalidChar [c]) H0
snocH (H3_2 c d) e k m 
    | d >= 0x80 && d < 0xc0 = k (m `mappend` b3 c d e) H0
    | otherwise = k (m `mappend` invalidChar [c,d]) H0
snocH (H4_1 c) d k m 
    | d >= 0x80 && d < 0xc0 = k m (H4_2 c d)
    | otherwise = k (m `mappend` invalidChar [c,d]) H0
snocH (H4_2 c d) e k m 
    | d >= 0x80 && d < 0xc0 = k m (H4_3 c d e)
    | otherwise = k (m `mappend` invalidChar [c,d,e]) H0
snocH (H4_3 c d e) f k m 
    | d >= 0x80 && d < 0xc0 = k (m `mappend` b4 c d e f) H0
    | otherwise = k (m `mappend` invalidChar [c,d,e,f]) H0

mask :: Word8 -> Word8 -> Int
mask c m = fromEnum (c .&. m) 

combine :: Int -> Word8 -> Int
combine a r = shiftL a 6 .|. fromEnum (r .&. 0x3f)

b1 :: CharReducer m => Word8 -> m
b1 c | c < 0x80 = fromChar . toEnum $ fromEnum c
     | otherwise = invalidChar [c]

b2 :: CharReducer m => Word8 -> Word8 -> m
b2 c d | valid_b2 c d = fromChar (toEnum (combine (mask c 0x1f) d))
       | otherwise = invalidChar [c,d]

b3 :: CharReducer m => Word8 -> Word8 -> Word8 -> m
b3 c d e | valid_b3 c d e = fromChar (toEnum (combine (combine (mask c 0x0f) d) e))
         | otherwise = invalidChar [c,d,e]


b4 :: CharReducer m => Word8 -> Word8 -> Word8 -> Word8 -> m
b4 c d e f | valid_b4 c d e f = fromChar (toEnum (combine (combine (combine (mask c 0x07) d) e) f))
           | otherwise = invalidChar [c,d,e,f]

valid_b2 :: Word8 -> Word8 -> Bool
valid_b2 c d = (c >= 0xc2 && c <= 0xdf && d >= 0x80 && d <= 0xbf)

valid_b3 :: Word8 -> Word8 -> Word8 -> Bool
valid_b3 c d e = (c == 0xe0 && d >= 0xa0 && d <= 0xbf && e >= 0x80 && e <= 0xbf) || 
                 (c >= 0xe1 && c <= 0xef && d >= 0x80 && d <= 0xbf && e >= 0x80 && e <= 0xbf)

valid_b4 :: Word8 -> Word8 -> Word8 -> Word8 -> Bool
valid_b4 c d e f = (c == 0xf0 && d >= 0x90 && d <= 0xbf && e >= 0x80 && e <= 0xbf && f >= 0x80 && f <= 0xbf) ||
      (c >= 0xf1 && c <= 0xf3 && d >= 0x80 && d <= 0xbf && e >= 0x80 && e <= 0xbf && f >= 0x80 && f <= 0xbf) ||
                   (c == 0xf4 && d >= 0x80 && d <= 0x8f && e >= 0x80 && e <= 0xbf && f >= 0x80 && f <= 0xbf)

consT :: CharReducer m => Word8 -> T -> (H -> UTF8 m) -> (m -> UTF8 m) -> (T -> UTF8 m) -> UTF8 m
consT c cs h m t
             | c < 0x80 = m $ b1 c `mappend` invalidChars cs
             | c < 0xc0 = t (c:cs)
             | c < 0xe0 = case cs of
                        [] -> h $ H2_1 c
                        (d:ds) -> m $ b2 c d `mappend` invalidChars ds
             | c < 0xf0 = case cs of
                        [] -> h $ H3_1 c
                        [d] -> h $ H3_2 c d
                        (d:e:es) -> m $ b3 c d e `mappend` invalidChars es
             | c < 0xf5 = case cs of
                        [] -> h $ H4_1 c
                        [d] -> h $ H4_2 c d 
                        [d,e] -> h $ H4_3 c d e 
                        (d:e:f:fs) -> m $ b4 c d e f `mappend` invalidChars fs
             | otherwise = mempty

invalidChars :: CharReducer m => [Word8] -> m
invalidChars = foldr (mappend . invalidChar . return) mempty

merge :: CharReducer m => H -> T -> (m -> a) -> (H -> a) -> a
merge H0 cs k _               = k $ invalidChars cs
merge (H2_1 c) [] _ p         = p $ H2_1 c
merge (H2_1 c) (d:ds) k _     = k $ b2 c d `mappend` invalidChars ds
merge (H3_1 c) [] _ p         = p $ H3_1 c
merge (H3_1 c) [d] _ p        = p $ H3_2 c d
merge (H3_1 c) (d:e:es) k _   = k $ b3 c d e `mappend` invalidChars es
merge (H3_2 c d) [] _ p       = p $ H3_2 c d
merge (H3_2 c d) (e:es) k _   = k $ b3 c d e `mappend` invalidChars es
merge (H4_1 c) [] _ p         = p $ H4_1 c
merge (H4_1 c) [d] _ p        = p $ H4_2 c d
merge (H4_1 c) [d,e] _ p      = p $ H4_3 c d e
merge (H4_1 c) (d:e:f:fs) k _ = k $ b4 c d e f `mappend` invalidChars fs
merge (H4_2 c d) [] _ p       = p $ H4_2 c d 
merge (H4_2 c d) [e] _ p      = p $ H4_3 c d e
merge (H4_2 c d) (e:f:fs) k _ = k $ b4 c d e f `mappend` invalidChars fs
merge (H4_3 c d e) [] _ p     = p $ H4_3 c d e
merge (H4_3 c d e) (f:fs) k _ = k $ b4 c d e f `mappend` invalidChars fs

instance CharReducer m => Monoid (UTF8 m) where
    mempty = T []
    T c `mappend` T d = T (c ++ d)
    T c `mappend` S l m r = S (c ++ l) m r
    S l m c `mappend` S c' m' r = S l (m `mappend` merge c c' id flushH `mappend` m') r
    s@(S _ _ _) `mappend` T [] = s
    S l m c `mappend` T c' = merge c c' k (S l m) where
        k m' = S l (m `mappend` m') H0

instance CharReducer m => Reducer Word8 (UTF8 m) where
    unit c | c >= 0x80 && c < 0xc0 = T [c]
           | otherwise = snocH H0 c (S []) mempty
    S t m h `snoc` c        = snocH h c (S t) m
    T t     `snoc` c        | c >= 0x80 && c < 0xc0 = T (t ++ [c])
                            | otherwise = snocH H0 c (S t) mempty

    c       `cons` T cs     = consT c cs (S [] mempty) (flip (S []) H0) T
    c       `cons` S cs m h = consT c cs k1 k2 k3 where
        k1 h' = S [] (flushH h' `mappend` m) h
        k2 m' = S [] (m' `mappend` m) h
        k3 t' = S t' m h
    
instance Functor UTF8 where
    fmap f (S t x h) = S t (f x) h
    fmap _ (T t) = T t

instance Pointed UTF8 where
    point f = S [] f H0

runUTF8 :: CharReducer m => UTF8 m -> m 
runUTF8 (T t) = flushT t
runUTF8 (S t m h) = flushT t `mappend` m `mappend` flushH h
