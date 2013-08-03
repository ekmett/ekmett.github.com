{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, TypeOperators, FlexibleInstances, FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generator.Compressive.RLE
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Compression algorithms are all about exploiting redundancy. When applying
-- an expensive 'Reducer' to a redundant source, it may be better to 
-- extract the structural redundancy that is present. Run length encoding
-- can do so for long runs of identical inputs.
-----------------------------------------------------------------------------

module Data.Generator.Compressive.RLE
    ( module Data.Generator
    , RLE(RLE, getRLE)
    , Run(Run)
    , decode
    , encode
    , encodeList
    , prop_decode_encode
    , prop_decode_encodeList
    ) where

import qualified Data.Sequence as Seq
import Data.Sequence (Seq,(|>),(<|),ViewL(..),ViewR(..),(><),viewl,viewr)
import Data.Foldable
import Data.Generator
import qualified Data.Monoid.Combinators as Monoid 
import Control.Functor.Pointed

-- | A single run with a strict length.
data Run a = Run a {-# UNPACK #-} !Int

instance Functor Run where
    fmap f (Run a n) = Run (f a) n

instance Pointed Run where
    point a = Run a 1

-- | A 'Generator' which supports efficient 'mapReduce' operations over run-length encoded data.
newtype RLE f a = RLE { getRLE :: f (Run a) } 

instance Functor f => Functor (RLE f) where
    fmap f = RLE . fmap (fmap f) . getRLE

instance Foldable f => Generator (RLE f a) where
    type Elem (RLE f a) = a
    mapReduce f = foldMap run . getRLE where
        run (Run a n) = unit (f a) `Monoid.replicate` n

decode :: Foldable f => RLE f a -> [a]
decode = reduce

-- | naive left to right encoder, which can handle infinite data

encodeList :: Eq a => [a] -> RLE [] a
encodeList [] = RLE []
encodeList (a:as) = RLE (point a `before` as)

before :: Eq a => Run a -> [a] -> [Run a]
r           `before` []                 = [r]
r@(Run a n) `before` (b:bs) | a == b    = Run a (n+1) `before` bs
                            | otherwise = r : point b `before` bs

-- | QuickCheck property: decode . encode = id
prop_decode_encodeList :: Eq a => [a] -> Bool
prop_decode_encodeList xs = decode (encode xs) == xs

-- One nice property that run-length encoding has is that it can be computed monoidally as follows
-- However, this monoid cannot be used to handle infinite sources.

instance Eq a => Monoid (RLE Seq a) where
    mempty = RLE Seq.empty
    RLE l `mappend` RLE r = viewr l `merge` viewl r where
        (l' :> Run a m) `merge` (Run b n :< r')
            | a == b     = RLE ((l' |> Run a (m+n)) >< r')
            | otherwise  = RLE (l >< r)
        EmptyR `merge` _ = RLE r
        _ `merge` EmptyL = RLE l

instance Eq a => Reducer a (RLE Seq a) where
    unit = RLE . Seq.singleton . point
    cons a (RLE r) = case viewl r of
            Run b n :< r' | a == b    -> RLE (Run a (n+1) <| r')
                          | otherwise -> RLE (Run a 1     <| r )
            EmptyL                    -> RLE (return (point a))
    snoc (RLE l) a = case viewr l of
            l' :> Run b n | a == b    -> RLE (l' |> Run b (n+1))
                          | otherwise -> RLE (l  |> Run a 1    )
            EmptyR                    -> RLE (return (point a))

encode :: (Generator c, Eq (Elem c)) => c -> RLE Seq (Elem c)
encode = reduce

prop_decode_encode :: (Generator c, Eq (Elem c)) => c -> Bool
prop_decode_encode xs = decode (encode xs) == reduce xs
