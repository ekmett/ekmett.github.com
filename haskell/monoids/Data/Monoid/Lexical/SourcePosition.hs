{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, BangPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Lexical.SourcePosition
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (MPTCs, OverloadedStrings)
--
-- Incrementally determine locations in a source file through local information
-- This allows for efficient recomputation of line #s and token locations
-- while the file is being interactively updated by storing this as a supplemental
-- measure on a 'FingerTree'.
--
-- The general idea is to use this as part of a measure in a 'FingerTree' so you can
-- use `mappend` to prepend a 'startOfFile' with the file information.
-----------------------------------------------------------------------------

module Data.Monoid.Lexical.SourcePosition
    ( module Data.Monoid.Reducer.Char
    , nextTab
    , SourcePosition(Pos,Lines,Columns,Tab)
    , SourceLine
    , SourceColumn
    , sourceLine
    , sourceColumn
    , startOfFile
    , showSourcePosition
    ) where

import Control.Functor.Extras
import Control.Functor.Pointed
import Data.Monoid.Reducer.Char
import Data.Generator
import Data.String



type SourceLine = Int
type SourceColumn = Int

-- | A 'Monoid' of partial information about locations in a source file.
--   This is polymorphic in the kind of information you want to maintain about each source file.
data SourcePosition file 
        = Pos file {-# UNPACK #-} !SourceLine {-# UNPACK #-} !SourceColumn -- ^ An absolute position in a file is known, or an overriding #line directive has been seen
        | Lines {-# UNPACK #-} !SourceLine {-# UNPACK #-} !SourceColumn    -- ^ We've seen some carriage returns.
        | Columns {-# UNPACK #-} !SourceColumn                             -- ^ We've only seen part of a line.
        | Tab {-# UNPACK #-} !SourceColumn {-# UNPACK #-} !SourceColumn    -- ^ We have an unhandled tab to deal with.
    deriving (Read,Show,Eq)

-- | Compute the location of the next standard 8-column aligned tab
nextTab :: Int -> Int
nextTab !x = x + (8 - (x-1) `mod` 8)

instance Functor SourcePosition where
    fmap g (Pos f l c) = Pos (g f) l c
    fmap _ (Lines l c) = Lines l c
    fmap _ (Columns c) = Columns c
    fmap _ (Tab x y) = Tab x y

instance Pointed SourcePosition where
    point f = Pos f 1 1

instance FunctorZero SourcePosition where
    fzero = mempty

instance FunctorPlus SourcePosition where
    fplus = mappend

instance IsString (SourcePosition file) where
    fromString = reduce

-- accumulate partial information
instance Monoid (SourcePosition file) where
    mempty = Columns 0

    Pos f l _ `mappend` Lines m d = Pos f (l + m) d
    Pos f l c `mappend` Columns d = Pos f l (c + d)
    Pos f l c `mappend` Tab x y   = Pos f l (nextTab (c + x) + y)
    Lines l _ `mappend` Lines m d = Lines (l + m) d
    Lines l c `mappend` Columns d = Lines l (c + d)
    Lines l c `mappend` Tab x y   = Lines l (nextTab (c + x) + y)
    Columns c `mappend` Columns d  = Columns (c + d)
    Columns c `mappend` Tab x y    = Tab (c + x) y
    Tab _ _   `mappend` Lines m d  = Lines m d
    Tab x y   `mappend` Columns d  = Tab x (y + d)
    Tab x y   `mappend` Tab x' y'  = Tab x (nextTab (y + x') + y')
    _         `mappend` pos        = pos

instance Reducer Char (SourcePosition file) where
    unit '\n' = Lines 1 1
    unit '\t' = Tab 0 0 
    unit _    = Columns 1

-- Indicate that we ignore invalid characters to the UTF8 parser
instance CharReducer (SourcePosition file)
    
-- | lift information about a source file into a starting 'SourcePosition' for that file
startOfFile :: f -> SourcePosition f
startOfFile = point

-- | extract partial information about the current column, even in the absence of knowledge of the source file
sourceColumn :: SourcePosition f -> Maybe SourceColumn
sourceColumn (Pos _ _ c) = Just c
sourceColumn (Lines _ c) = Just c
sourceColumn _ = Nothing

-- | extract partial information about the current line number if possible
sourceLine :: SourcePosition f -> Maybe SourceLine
sourceLine (Pos _ l _) = Just l
sourceLine _ = Nothing

-- | extract the standard format for an absolute source position
showSourcePosition :: SourcePosition String -> String
showSourcePosition pos = showSourcePosition' (point "-" `mappend` pos) where
    showSourcePosition' (Pos f l c) = f ++ ":" ++ show l ++ ":" ++ show c
    showSourcePosition' _ = undefined
