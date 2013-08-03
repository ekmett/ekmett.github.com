module Data.Rope ( 
    -- * Size
      Rope
    , length    -- :: Rope -> Int
    , null      -- :: Rope -> Bool
    -- * Slicing
    , Breakable(..)
    , splitAt   -- :: Int -> Rope -> (Rope, Rope)
    , take      -- :: Int -> Rope -> Rope
    , drop      -- :: Int -> Rope -> Rope
    -- * Walking
    -- ** construction
    , Reducer(..)
    , empty              -- :: Rope
    , fromByteString     -- :: ByteString -> Rope
    , fromChunks         -- :: [ByteString] -> Rope
    , fromLazyByteString -- :: L.ByteString -> Rope
    , fromWords          -- :: [Word8] -> Rope
    , fromChar           -- :: Char -> Rope
    , fromWord8          -- :: Word8 -> Rope
    , fromString         -- :: String -> Rope
    -- * Deconstructing 'Rope's
    , Unpackable(..)
    , toChunks           -- :: Rope -> [ByteString]
    , toLazyByteString   -- :: Rope -> L.ByteString
    , toString           -- :: Rope -> String
    ) where

import Prelude hiding (null,head,length,drop,take,splitAt, last)
import Data.Rope.Util.Reducer (Reducer(..))
import Data.Rope.Internal 
    ( Rope
    , empty
    , length
    , null
    , fromChunks
    , fromByteString
    , fromLazyByteString
    , fromWords
    , fromChar
    , fromWord8
    , fromString
    , toString
    , toChunks
    , toLazyByteString
    , Breakable(..)
    , Unpackable(..)
    , splitAt
    , take
    , drop)
