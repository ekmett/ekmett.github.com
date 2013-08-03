{-# LANGUAGE TypeFamilies, FlexibleContexts  #-}
module Text.Parsimony.Combinators 
    ( oneOf
    , noneOf
    , anyToken
    , option
    , count
    , many1
    , endBy
    , endBy1
    , sepBy
    , sepBy1
    , sepEndBy
    , sepEndBy1
    , skipMany
    , skipMany1
    , between 
    ) where

import Control.Applicative
import Text.Parsimony.Prim
import Data.Traversable

oneOf :: (Mode m, Eq t) => [t] -> Parser m t t
oneOf cs = satisfy (\c -> Prelude.elem c cs)

noneOf :: (Mode m, Eq t) => [t] -> Parser m t t
noneOf cs = satisfy (\c -> not (Prelude.elem c cs))

many1 :: Alternative f => f a -> f [a]
many1 = some

anyToken :: Mode m => Parser m t t
anyToken = satisfy (const True) <?> "anything"

option :: Alternative f => a -> f a -> f a
option a p = p <|> pure a 

count :: Applicative f => Int -> f a -> f [a]
count n p 
    | n <= 0 = pure []
    | otherwise = sequenceA (replicate n p)

endBy :: Mode m => Parser m t a -> Parser Recognizing t sep -> Parser m t [a]
endBy p sep = many (p <<* sep) 

endBy1 :: Mode m => Parser m t a -> Parser Recognizing t sep -> Parser m t [a]
endBy1 p sep = some (p <<* sep)

sepBy :: Mode m => Parser m t a -> Parser Recognizing t sep -> Parser m t [a]
sepBy p sep = option [] (sepBy1 p sep)

sepBy1 :: Mode m => Parser m t a -> Parser Recognizing t sep -> Parser m t [a]
sepBy1 p sep = (:) <$> p <*> many (sep *>> p) 

skipMany1 :: Mode m => Parser Recognizing t a -> Parser m t ()
skipMany1 p = skip (many1 p)

skipMany :: Mode m => Parser Recognizing t a -> Parser m t ()
skipMany p = skip (many p)

between :: Mode m => Parser Recognizing t open -> Parser Recognizing t close -> Parser m t a -> Parser m t a
between open close p = open *>> p <<* close 

sepEndBy :: Mode m => Parser m t a -> Parser Recognizing t sep -> Parser m t [a]
sepEndBy p sep = go0 where
    go0 = option [] go1
    go1 = (:) <$> p <*> option [] (sep *>> go0)

sepEndBy1 :: Mode m => Parser m t a -> Parser Recognizing t sep -> Parser m t [a]
sepEndBy1 p sep = go1 where
    go0 = option [] go1
    go1 = (:) <$> p <*> option [] (sep *>> go0)
