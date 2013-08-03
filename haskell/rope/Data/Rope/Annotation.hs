{-# LANGUAGE TypeOperators #-}
module Data.Rope.Annotation
    ( MonoidA(..)
    , ReducerA(..)
    , BreakableA(..)
    ) where

import Data.Rope (Rope)

class MonoidA f where
    -- | build an empty 'Annotation'
    emptyA   :: f a
    -- | append two annotations
    appendA  :: Rope -> f a -> Rope -> f b -> f c

class MonoidA f => ReducerA f where
    -- | construct an 'Annotation' from a 'Rope' out of whole cloth
    unitA    :: Rope -> f a
    -- | The 'Rope' has been updated to contains n more bytes on the right than the one used to build the 'Annotation', update the 'Annotation'
    snocA    :: Int -> Rope -> f a -> f b
    -- | The 'Rope' contains n more bytes on the left than the one used to build the 'Annotation', update the 'Annotation'
    consA    :: Int -> Rope -> f a -> f b
    
class BreakableA f where

    -- | split an 'Annotation' about a 'Rope' into two annotations, one about the first n bytes, the other about the remainder
    splitAtA :: Int -> Rope -> f a -> (f b, f c)
    -- | truncate the 'Annotation' to 'length' n
    takeA    :: Int -> Rope -> f a -> f b
    -- | drop the first n bytes from the 'Annotation'
    dropA    :: Int -> Rope -> f a -> f b


    takeA n r = fst . splitAtA n r
    dropA n r = snd . splitAtA n r
