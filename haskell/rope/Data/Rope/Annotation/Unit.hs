{-# LANGUAGE TypeOperators, EmptyDataDecls #-}
module Data.Rope.Annotation.Unit
    ( Unit
    ) where

import Data.Rope.Annotation

data Unit a

instance MonoidA Unit where
    emptyA = undefined
    appendA _ _ _ _ = undefined

instance ReducerA Unit where
    unitA _ = undefined
    snocA _ _ _ = undefined
    consA _ _ _ = undefined

instance BreakableA Unit where
    takeA _ _ _ = undefined
    dropA _ _ _ = undefined
    splitAtA _ _ _ = (undefined, undefined)
