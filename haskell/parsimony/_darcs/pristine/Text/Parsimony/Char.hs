{-# LANGUAGE TypeFamilies, FlexibleInstances  #-}
module Text.Parsimony.Char where

import Control.Applicative

import Data.Char
import Data.Traversable

import Text.Parsimony.Prim
import Text.Parsimony.Combinators

char :: Mode m => Char -> Parser m Char Char
char x = satisfy (==x) <?> show [x]

tab :: Mode m => Parser m Char Char
tab = char '\t' <?> "tab"

space :: Mode m => Parser m Char Char
space = satisfy isSpace <?> "space"

spaces :: Mode m => Parser m Char ()
spaces = skip (many space) <?> "white space"

lower :: Mode m => Parser m Char Char
lower = satisfy isLower <?> "lowercase letter"

upper :: Mode m => Parser m Char Char
upper = satisfy isUpper <?> "uppercase letter"

alphaNum :: Mode m => Parser m Char Char
alphaNum = satisfy isAlphaNum <?> "letter or digit"

letter :: Mode m => Parser m Char Char
letter = satisfy isAlpha <?> "letter"

hexDigit :: Mode m => Parser m Char Char
hexDigit = satisfy isHexDigit <?> "hexadecimal digit"

octDigit :: Mode m => Parser m Char Char
octDigit = satisfy isOctDigit <?> "octal digit"

anyChar :: Mode m => Parser m Char Char
anyChar = anyToken

string :: Mode m => String -> Parser m Char String
string = traverse char
