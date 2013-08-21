{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, GeneralizedNewtypeDeriving, ParallelListComp, TypeFamilies, OverloadedStrings, UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Lexical.Words
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (MPTCs, OverloadedStrings)
--
-- A simple demonstration of tokenizing a 'Generator' into distinct words 
-- and/or lines using a word-parsing 'Monoid' that accumulates partial 
-- information about words and then builds up a token stream.
--
-----------------------------------------------------------------------------

module Data.Monoid.Lexical.Words 
    ( module Data.Monoid.Reducer.Char
    -- * Words
    , Words
    , runWords
    , Unspaced(runUnspaced)
    , wordsFrom
    -- * Lines
    , Lines
    , runLines
    , Unlined(runUnlined)
    , linesFrom
    ) where

import Data.String
import Data.Char (isSpace)
import Data.Maybe (maybeToList)
import Data.Monoid.Reducer.Char
import Data.Generator
import Control.Functor.Pointed

-- | A 'CharReducer' transformer that breaks a 'Char' 'Generator' into distinct words, feeding a 'Char' 'Reducer' each line in turn
data Words m = Chunk (Maybe m)
             | Segment (Maybe m) [m] (Maybe m)
    deriving (Show,Read)

-- | Extract the matched words from the 'Words' 'Monoid'
runWords :: Words m -> [m]
runWords (Chunk m) = maybeToList m
runWords (Segment l m r) = maybeToList l ++ m ++ maybeToList r

instance Monoid m => Monoid (Words m) where
    mempty = Chunk mempty
    Chunk l `mappend` Chunk r = Chunk (l `mappend` r)
    Chunk l `mappend` Segment l' m r = Segment (l `mappend` l') m r
    Segment l m r `mappend` Chunk r' = Segment l m (r `mappend` r')
    Segment l m r `mappend` Segment l' m' r' = Segment l (m ++ maybeToList (r `mappend` l') ++ m') r'

instance Reducer Char m => Reducer Char (Words m) where
    unit c | isSpace c = Segment (Just (unit c)) [] mempty
           | otherwise = Chunk (Just (unit c))

instance Functor Words where
    fmap f (Chunk m) = Chunk (fmap f m)
    fmap f (Segment m ms m') = Segment (fmap f m) (fmap f ms) (fmap f m')

instance (CharReducer m) => CharReducer (Words m) where
    invalidChar xs = Segment (Just (invalidChar xs)) [] mempty

instance Reducer Char m => IsString (Words m) where
    fromString = reduce

-- | A 'CharReducer' transformer that breaks a 'Char' 'Generator' into distinct lines, feeding a 'Char' 'Reducer' each line in turn.
newtype Lines m = Lines (Words m) deriving (Show,Read,Monoid,Functor)

instance Reducer Char m => Reducer Char (Lines m) where
    unit '\n' = Lines $ Segment (Just (unit '\n')) [] mempty
    unit c = Lines $ Chunk (Just (unit c))

instance (CharReducer m) => CharReducer (Lines m) where
    invalidChar xs = Lines $ Segment (Just (invalidChar xs)) [] mempty

instance Reducer Char m => IsString (Lines m) where
    fromString = reduce

-- | Extract the matched lines from the 'Lines' 'Monoid'
runLines :: Lines m -> [m]
runLines (Lines x) = runWords x

-- | A 'CharReducer' transformer that strips out any character matched by `isSpace`
newtype Unspaced m = Unspaced { runUnspaced :: m }  deriving (Eq,Ord,Show,Read,Monoid)

instance Reducer Char m => Reducer Char (Unspaced m) where
    unit c | isSpace c = mempty
           | otherwise = Unspaced (unit c)

instance CharReducer m => CharReducer (Unspaced m) where
    invalidChar = Unspaced . invalidChar

instance Functor Unspaced where
    fmap f (Unspaced x) = Unspaced (f x)

instance Pointed Unspaced where
    point = Unspaced

instance Copointed Unspaced where
    extract = runUnspaced

instance Reducer Char m => IsString (Unspaced m) where
    fromString = reduce

-- | A 'CharReducer' transformer that strips out newlines
newtype Unlined m = Unlined { runUnlined :: m }  deriving (Eq,Ord,Show,Read,Monoid)

instance Reducer Char m => Reducer Char (Unlined m) where
    unit '\n' = mempty
    unit c = Unlined (unit c)

instance CharReducer m => CharReducer (Unlined m) where
    invalidChar = Unlined . invalidChar

instance Functor Unlined where
    fmap f (Unlined x) = Unlined (f x)

instance Pointed Unlined where
    point = Unlined

instance Copointed Unlined where
    extract = runUnlined

instance Reducer Char m => IsString (Unlined m) where
    fromString = reduce

-- | Utility function to extract words using accumulator, inside-word, and until-next-word monoids
wordsFrom :: (Generator c, Elem c ~ Char, Char `Reducer` m, Char `Reducer` n, Char `Reducer` o) => m -> c -> [(m,n,o)]
wordsFrom s c = [(x,runUnlined y,z) | x <- scanl mappend s ls | (y,z) <- rs ] where
    (ls,rs) = unzip (runWords (mapReduce id c))

-- | Utility function to extract lines using accumulator, inside-line, and until-next-line monoids
linesFrom :: (Generator c, Elem c ~ Char, Char `Reducer` m, Char `Reducer` n, Char `Reducer` o) => m -> c -> [(m,n,o)]
linesFrom s c = [(x,runUnlined y,z) | x <- scanl mappend s ls | (y,z) <- rs ] where
    (ls,rs) = unzip (runLines (mapReduce id c))
