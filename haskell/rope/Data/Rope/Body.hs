{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, MultiParamTypeClasses, FlexibleContexts, TypeSynonymInstances #-}
module Data.Rope.Body
    ( Body
    , Offset(..)
    , Chunk(..)
    , measureBody
    , cons'
    , snoc'
    ) where

import Prelude hiding (null, length)
import Data.FingerTree (FingerTree,(<|),(|>),Measured,measure,empty, singleton)
import Data.Data
import Data.Typeable
import Data.Monoid
import Data.Rope.Util.Reducer
import Data.ByteString (ByteString, null, length)

newtype Offset = Offset { getOffset :: Int } deriving (Eq,Ord,Num,Show,Read,Enum,Data,Typeable)

instance Monoid Offset where
    mempty = 0
    mappend = (+)

newtype Chunk = Chunk { unchunk :: ByteString } deriving (Eq,Ord,Show,Read,Data,Typeable)

instance Measured Offset Chunk where
    measure = Offset . length . unchunk

type Body = FingerTree Offset Chunk 

measureBody :: Measured Offset a => FingerTree Offset a -> Int
measureBody = getOffset . measure

cons' :: ByteString -> Body -> Body
b `cons'` t | null b = t
            | otherwise = Chunk b <| t

snoc' :: Body -> ByteString -> Body
t `snoc'` b | null b = t
            | otherwise = t |> Chunk b

instance Reducer ByteString Body where
    unit b | null b = empty
           | otherwise = singleton (Chunk b)
    cons = cons'
    snoc = snoc'
