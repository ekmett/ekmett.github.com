{-# LANGUAGE KindSignatures, TypeFamilies, GADTs, EmptyDataDecls, FlexibleContexts, FlexibleInstances, Rank2Types #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parsimony.Prim
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (type families, GADTs)
--
-- Parallel parser combinators
--
-----------------------------------------------------------------------------

module Text.Parsimony.Prim 
    ( Parsing
    , Recognizing
    , Parser(..)
    , Mode, Pure, Fun
    -- , fmapParser, pureParser, appParser
    , satisfy, skip 
    , (<<$), (*>>), (<<*)
    , named
    , recognizing
    , labels
    , (<?>)
    , choice 
    , (<<|>)
    , greedyChoice
    , epsilon
    ) where

import Control.Applicative
import Control.Arrow (Arrow, arr)
import Control.Category
import Prelude hiding ((.),id)
import Text.Parsimony.Util
import Unsafe.Coerce (unsafeCoerce)
import Data.Traversable
#ifdef X_OverloadedStrings
import Data.String
#endif

-- | This parser is capable of parsing a token stream and obtaining an answer
data Parsing

-- | This parser may only be used to recognize a token stream as belonging to a context-free language
data Recognizing

-- | 'Parser' invariants provided by smart constructors:
--
-- (1) a Name never contains a Name directly
--
-- (2) Labels never contain Labels directly
--
-- (3) an Alt never contains an Alt directly
--
-- (4) a Greedy never contains a Greedy directly
--
-- (5) a Skip never contains a Skip transitively
--
-- Unenforceable invariants required for the parallel parser: 
--
-- (1) The set of non-terminals used by the parser is finite

data Parser m t a where
    App     :: Parser m t (a -> b) -> Parser m t a -> Parser m t b
    Pure    :: Pure m a -> Parser m t a
    Alt     :: [Parser m t a] -> Parser m t a
    Greedy  :: [Parser m t a] -> Parser m t a
    Satisfy :: Fun m t a -> (t -> Bool) -> Parser m t a
    Skip    :: Parser Recognizing t a -> Parser m t ()
    Name    :: Parser m t a -> String -> Parser m t a 
    Labels  :: Parser m t a -> [String] -> Parser m t a

-- | A parser mode defines what optimizations are possible, and how much information must be retained
class (Arrow (Fun m), Applicative (Pure m)) => Mode m where
    type Pure m :: * -> *
    type Fun  m :: * -> * -> *

    fmapParser :: (a -> b) -> Parser m t a -> Parser m t b
    pureParser :: a -> Parser m t a
    appParser  :: Parser m t (a -> b) -> Parser m t a -> Parser m t b
    skip       :: Parser Recognizing t a -> Parser m t ()

    fmapParser g (App f x)     = App (fmap g <$> f) x
    fmapParser g (Pure a)      = Pure (g <$> a)
    fmapParser g (Alt as)      = Alt (fmap g <$> as)
    fmapParser g (Greedy as)   = Greedy (fmap g <$> as)
    fmapParser g (Satisfy s p) = Satisfy (arr g . s) p
    fmapParser _ s@(Skip _)    = unsafeCoerce s
    fmapParser g (Name p s)    = Name (g <$> p) s
    fmapParser g (Labels p ss) = Labels (g <$> p) ss

    pureParser = Pure . pure

    skip       = Skip

-- | Generate a parser that recognizes a single token using a predicate
satisfy :: Mode m => (t -> Bool) -> Parser m t t
satisfy = Satisfy id

-- | treat a non-recognizing parser as a recognizing one to reduce redundant rules
recognizing :: Mode m => Parser m t a -> Parser Recognizing t a
recognizing = unsafeCoerce

-- | a parser mode that actually parses and retains intermediate results
instance Mode Parsing where
    type Pure Parsing = Id
    type Fun  Parsing = (->)

    appParser (Pure f) (Pure x)      = Pure (f <*> x)
    appParser (Pure f) (Satisfy s p) = Satisfy (runId f <$> s) p
    appParser f x                    = App f x

-- | unsafeCoerces parsers to gain additional sharing that cannot be obtained in a general parser
instance Mode Recognizing where
    type Pure Recognizing = TrivialApplicative
    type Fun  Recognizing = TrivialArrow

    skip         = unsafeCoerce
    fmapParser _ = unsafeCoerce

    appParser Pure{} x      = unsafeCoerce x
    appParser f      Pure{} = unsafeCoerce f
    appParser f      x      = App f x 

    pureParser _ = unsafeCoerce epsilon

-- | parser that accepts the empty string used by Recognizing parsers where necessary
epsilon :: Parser Recognizing Magic ()
epsilon = Pure undefined `named` "epsilon"

instance Mode m => Functor (Parser m t) where
    fmap = fmapParser

instance Mode m => Applicative (Parser m t) where
    pure  = pureParser
    (<*>) = appParser

instance Mode m => Alternative (Parser m t) where
    empty             = Alt []
    Alt as <|> Alt bs = Alt (as ++ bs)
    Alt as <|> b      = Alt (as ++ [b])
    a      <|> Alt bs = Alt (a:bs)
    a      <|> b      = Alt [a,b]

-- | optimized version of '(<$)'
(<<$) :: Mode m => a -> Parser Recognizing t b -> Parser m t a 
a <<$ p = const a <$> skip p

-- | optimized version of '(*>)'
(*>>) :: Mode m => Parser Recognizing t a -> Parser m t b -> Parser m t b
a *>> b = const id <$> skip a <*> b

-- | optimized version of '(<*)'
(<<*) :: Mode m => Parser m t a -> Parser Recognizing t b -> Parser m t a
a <<* b = const <$> a <*> skip b

-- | smart constructor for naming parsers for purposes of displaying the grammar
named :: Parser m t a -> String -> Parser m t a
named (Name p _) = Name p
named p          = Name p

-- | smart constructor for labeling the grammar ala Parsec
labels :: Parser m t a -> [String] -> Parser m t a 
labels (Labels p _) = Labels p
labels p            = Labels p 

-- | Annotate a parser with what it should say was expected if the first character in it is unconsumed
(<?>) :: Parser m t a -> String -> Parser m t a 
p <?> s = labels p [s]

-- | Multiple parsers returning multiple answers
choice :: [Parser m t a] -> Parser m t a 
choice = Alt . foldr flatten [] where
    flatten (Alt bs) as = bs ++ as
    flatten a as        = a : as

-- | Prefers the result from the left when present. Only accepts parses from the right hand parser
--   when the left hand parser fails.
(<<|>) :: Parser m t a -> Parser m t a -> Parser m t a 
Greedy as <<|> Greedy bs = Greedy (as ++ bs)
Greedy as <<|> a         = Greedy (as ++ [a])
a         <<|> Greedy as = Greedy (a : as)
a         <<|> b         = Greedy [a,b]

-- | Multiple parsers returning the results from the left-most parser that matches anything
greedyChoice :: [Parser m t a] -> Parser m t a
greedyChoice = Greedy . foldr flatten [] where
    flatten (Greedy bs) as = bs ++ as
    flatten a as = a : as

-- | Here rather than in Text.Parsimony.Char to avoid an orphan instance
#ifdef X_OverloadedStrings
instance Mode m => IsString (Parser m Char String) where
    fromString = traverse (\x -> satisfy (==x) <?> show [x])
#endif
