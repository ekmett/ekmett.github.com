{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parsimony.Parsec
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (type families)
--
-- A minimalist interpreter that evaluates a Parsimony parser with Parsec
--
-----------------------------------------------------------------------------

module Text.Parsimony.Parsec where

import Control.Applicative
import Text.Parsimony.Prim
import Text.Parsimony.Util
import Text.Parsec as Parsec

eval :: Parsec.Stream s m Char => Parser Parsing Char e -> Parsec.ParsecT s u m e
eval expr = case expr of 
    App f x     -> eval f <*> eval x
    Pure (Id a) -> pure a
    Alt as      -> Parsec.choice (Parsec.try . eval <$> as)
    Greedy as   -> Parsec.choice (eval <$> as)
    Satisfy f p -> f <$> Parsec.satisfy p 
    Name n _    -> eval n
    Labels l ls -> Parsec.labels (eval l) ls
    Skip s      -> eval_ s

eval_ :: (Mode mode, Parsec.Stream s m Char) => Parser mode Char e -> Parsec.ParsecT s u m ()
eval_ expr = case expr of 
    App f x     -> eval_ f >> eval_ x
    Pure _      -> pure ()
    Alt as      -> Parsec.choice (Parsec.try . eval_ <$> as) 
    Greedy as   -> Parsec.choice (eval_ <$> as)
    Satisfy _ p -> () <$ Parsec.satisfy p
    Name p _    -> eval_ p
    Labels p ls -> Parsec.labels (eval_ p) ls
    Skip p      -> eval_ p -- unexpected Skip in Skip
