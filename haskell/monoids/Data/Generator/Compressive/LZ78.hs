{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generator.Compressive.LZ78
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Compression algorithms are all about exploiting redundancy. When applying
-- an expensive 'Reducer' to a redundant source, it may be better to 
-- extract the structural redundancy that is present. 'LZ78' is a compression
-- algorithm that does so, without requiring the dictionary to be populated
-- with all of the possible values of a data type unlike its later 
-- refinement LZW, and which has fewer comparison reqirements during encoding
-- than its earlier counterpart LZ77. Since we aren't storing these as a 
-- bitstream the LZSS refinement of only encoding pointers once you cross
-- the break-even point is a net loss. 
-----------------------------------------------------------------------------


module Data.Generator.Compressive.LZ78 
    ( module Data.Generator
    -- * Lempel-Ziv 78 
    , LZ78
    -- * Decoding
    , decode
    -- * Encoding
    , encode
    , encodeEq
    -- * QuickCheck Properties
    , prop_decode_encode
    , prop_decode_encodeEq
    ) where

import qualified Data.Sequence as Seq
import Data.Sequence (Seq,(|>))
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.List as List
import Data.Generator
import Data.Foldable
import Data.Monoid.Self

-- | An LZ78 compressing 'Generator', which supports efficient 'mapReduce' operations

data Token a = Token a {-# UNPACK #-} !Int 
    deriving (Eq,Ord,Show,Read)

-- after using the Functor instance the encoding may no longer be minimal
instance Functor Token where
    fmap f (Token a n) = Token (f a) n

newtype LZ78 a = LZ78 { getLZ78 :: [Token a] } 
    deriving (Eq,Ord,Show)

emptyDict :: Monoid m => Seq m
emptyDict = Seq.singleton mempty

instance Generator (LZ78 a) where
    type Elem (LZ78 a) = a
    mapTo f m (LZ78 xs) = mapTo' f m emptyDict xs

instance Functor LZ78 where
    fmap f = LZ78 . fmap (fmap f) . getLZ78

instance Foldable LZ78 where
    foldMap f = getSelf . mapReduce f
    fold = getSelf . reduce
    
mapTo' :: (e `Reducer` m) => (a -> e) -> m -> Seq m -> [Token a] -> m
mapTo' _ m _ [] = m
mapTo' f m s (Token c w:ws) = m `mappend` mapTo' f v (s |> v) ws 
    where 
        v = Seq.index s w `mappend` unit (f c)

-- | a type-constrained 'reduce' operation
    
decode :: LZ78 a -> [a]
decode = reduce

-- | contruct an LZ78-compressed 'Generator' using a 'Map' internally, requires an instance of Ord.

encode :: Ord a => [a] -> LZ78 a
encode = LZ78 . encode' Map.empty 1 0

encode' :: Ord a => Map (Token a) Int -> Int -> Int -> [a] -> [Token a]
encode' _ _ p [c] = [Token c p]
encode' d f p (c:cs) = let t = Token c p in case Map.lookup t d of
    Just p' -> encode' d f p' cs
    Nothing -> t : encode' (Map.insert t f d) (succ f) 0 cs
encode' _ _ _ [] = []

-- | contruct an LZ78-compressed 'Generator' using a list internally, requires an instance of Eq.

encodeEq :: Eq a => [a] -> LZ78 a
encodeEq = LZ78 . encodeEq' [] 1 0

encodeEq' :: Eq a => [(Token a,Int)] -> Int -> Int -> [a] -> [Token a]
encodeEq' _ _ p [c] = [Token c p]
encodeEq' d f p (c:cs) = let t = Token c p in case List.lookup t d of
    Just p' -> encodeEq' d f p' cs
    Nothing -> t : encodeEq' ((t,f):d) (succ f) 0 cs
encodeEq' _ _ _ [] = []

-- | QuickCheck property: decode . encode = id
prop_decode_encode :: Ord a => [a] -> Bool
prop_decode_encode xs = decode (encode xs) == xs

-- | QuickCheck property: decode . encodeEq = id
prop_decode_encodeEq :: Eq a => [a] -> Bool
prop_decode_encodeEq xs = decode (encodeEq xs) == xs
