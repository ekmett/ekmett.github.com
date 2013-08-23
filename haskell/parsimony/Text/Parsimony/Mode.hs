{-# LANGUAGE TypeOperators #-}
module Text.Parsimony.Mode where

import Control.Applicative
import Data.Generator
import Text.Parsimony.Prim
import Text.Parsimony.Error

class ParseMode m where
    parseMap :: (Generator c, Alternative f) => (Elem c -> e) -> Parser m e a -> c -> Either (ParseError file) (f a)

    parse :: (Generator c, Alternative f) => Parser m (Elem c) a -> c -> Either (ParseError file) (f a)
    parse = parseMap id

class RecognizeMode m where
    recognizeMap :: Generator c => (Elem c -> e) -> Parser m e a -> c -> Bool

    recognize :: Generator c => Parser m (Elem c) a -> c -> Bool
    recognize = recognizeMap id
