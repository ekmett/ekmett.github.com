{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Lexical.Binary
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (MPTCs, OverloadedStrings)
--
-- Reduce
--
-----------------------------------------------------------------------------

module Data.Monoid.Lexical.Binary
    ( module Data.Monoid.Reducer
    ) where

import Prelude hiding (lex)
import Control.Functor.Extras
import Control.Functor.Pointed
import Data.Monoid.Reducer.Char
import Data.Generator
import Data.String


