{-# LANGUAGE  RankNTypes, 
              GADTs,
              MultiParamTypeClasses,
              FunctionalDependencies, 
              FlexibleInstances, 
              FlexibleContexts, 
              UndecidableInstances,
              NoMonomorphismRestriction #-}

-------------------------------------------------------------------------------------------
-- |
-- Module      : Text.ParserCombinators.UU.BasicInstances
-- Copyright   : 2001-2009 Doaitse Swierstra, 2009 Edward Kmett
-- License     : LGPL 
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GADTs, MPTCs, Fundeps)
--
-------------------------------------------------------------------------------------------

module Text.ParserCombinators.UU.Examples where

import Data.Char
import Data.Traversable
import Text.ParserCombinators.UU.Parsing
import Text.ParserCombinators.UU.BasicInstances

test :: P (Str Char) b -> String -> (b, [Error Char Char Int]) 
test p inp = parse ( (,) <$> p <*> pEnd) (listToStr inp)

pa, pb, paz :: P (Str Char) [Char] 
pa = return <$> pSym 'a'
pb = return <$> pSym 'b'
p <++> q = (++) <$> p <*> q
pa2 = pa <++> pa
pa3 = pa <++> pa2

count p = (\ a b -> b+1) <$> p <*> count p <<|> pure 0
exact 0 p = pure []
exact n p = (:) <$> p <*> exact (n-1) p

paz = pMany (pSym ('a','z'))

paz' = pSym (\t -> 'a' <= t && t <= 'z', "a .. z", 'k')

printTest :: Show b => P (Str Char) b -> String -> IO ()
printTest p inp = let (b,e) = test p inp in putStrLn ("Result: " ++ show b ++ concatMap showError e)

main :: IO ()
main = do 
    printTest pa "a"
    printTest pa "b"
    printTest pa2 "bbab"
    printTest pa "ba"
    printTest pa "aa"
    printTest (do l <- count pa; exact l pb) "aaacabbb"
    printTest (amb ( (++) <$> pa2 <*> pa3 <|> (++) <$> pa3 <*> pa2)) "aaabaa"
    printTest paz "ab1z7"
    printTest paz' "m"
    printTest paz' ""

infixl 3 `pOpt`
p `pOpt` v = p <|> pure v 

-- parsing pMany things
pMany p  = ((:) <$> p <*> pMany p) <|> pure []
pMany1 p = (:) <$> p <*> pMany p
pChoice = foldr (<|>) empty
listSep p s = listSep1 p s `pOpt` []
listSep1 p s = (:) <$> p <*> sepTail p s
   where sepTail p s = pMany (s *> p)

-- bracketing expressions
parens   p = id <$ pSym '(' <*> p <* pSym ')'
brackets p = id <$ pSym '[' <*> p <* pSym ']'
curlies  p = id <$ pSym '{' <*> p <* pSym '}'

-- parsing numbers
digit = pSym ('0', '9')
digitAsInt = digit2Int <$> digit where
    digit2Int a = ord a - ord '0'
natural = foldl (\a b -> a * 10 + b) 0 <$> pMany1 digitAsInt

-- parsing letters and identifiers
lower  = pSym ('a','z')
upper  = pSym ('A','Z')
letter = upper <|> lower
varId  = (:) <$> lower <*> pMany idChar
conId  = (:) <$> upper <*> pMany idChar
idChar = lower <|> upper <|> digit <|> pSymIn "='"
pSymIn s = pChoice $ map pSym s
pKey str = sequenceA (map pSym str)

-- running the parser; if complete input accepted return the result else fail with reporting unconsumed tokens
run :: P (Str Char) t -> String -> t
run p i = let (a,b) = exec p i in
          if null b then a else error (show b)

exec :: P (Str Char) b -> String -> (b, [Error Char Char Int])
exec p inp = parse ( (,) <$> p <*> pEnd) (listToStr inp)

-- Testing
testMS :: P (Str Char) Char
testMS = id <$ pSym 'u' <*> pSym '2'

pOp (c, o) = o <$ pSym c

pChainl t pOp = applyAll <$> t <*> pMany (flip <$> pOp <*> t)
applyAll e [] = e
applyAll e (f:fs) = applyAll (f e) fs

expr = term `pChainl` (pOp ('+', (+)) <|> pOp ('-', (-)))
term = factor `pChainl` pOp ('*' , (*))
factor = getal <|> pSym '(' *> expr <* pSym ')'
getal = natural

rune ::  String -> IO ()
rune i = let (a,b) = exec expr i in
         if null b then print ("Result: " ++ show a)
                   else do print b
                           print ("Result: " ++ show a)
