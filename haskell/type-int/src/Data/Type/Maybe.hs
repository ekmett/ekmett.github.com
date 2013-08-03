{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Type.Maybe
-- Copyright   :  (C) 2006 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (MPTC)
--
-- Simple type-level Maybe w/ Just/Nothing Types
----------------------------------------------------------------------------

module Data.Type.Maybe (
	TNothing(..), tNothing,
	TJust(..), tJust,
	TFromJust, tFromJust,
	TMaybe
) where

data TNothing
tNothing :: TNothing
tNothing = undefined

data TJust x

tJust :: t -> TJust t 
tJust = undefined

tFromJust :: TJust t -> t
tFromJust = undefined

instance Show TNothing where show _ = "TNothing"
instance (TJust a, Show a) => Show (TJust a) where show x = "TJust " ++ show (tFromJust x)

class TMaybe a b
instance TMaybe TNothing b
instance TMaybe (TJust b) b
