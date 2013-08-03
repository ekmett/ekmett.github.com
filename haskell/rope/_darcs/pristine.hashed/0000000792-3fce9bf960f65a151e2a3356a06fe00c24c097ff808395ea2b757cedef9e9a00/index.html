{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances, MultiParamTypeClasses, UndecidableInstances, TypeOperators, DeriveDataTypeable #-}
module Data.Rope.Classes
    ( Breakable(..)
    , Unpackable(..)
    ) where

import Prelude hiding (break, span, takeWhile, dropWhile, unpack, head, last, uncons, unsnoc)

class Breakable a r where
    break :: (a -> Bool) -> r -> (r, r)
    span :: (a -> Bool) -> r -> (r, r)
    takeWhile :: (a -> Bool) -> r -> r
    dropWhile :: (a -> Bool) -> r -> r

    span f = break (not . f)
    takeWhile f = fst . span f
    dropWhile f = snd . span f

class Unpackable a r where
    unpack :: r -> [a]
    head :: r -> a
    last :: r -> a
    uncons :: r -> Maybe (a, r )
    unsnoc :: r -> Maybe (r, a)

    head = Prelude.head . unpack
