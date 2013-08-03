module Test where

import Prelude hiding (lex)
import Data.Monoid.Lexical.SourcePosition
import Data.Monoid.Lexical.Words
import Data.Monoid.Lexical
import Data.Monoid
import Data.Maybe (fromJust)
import Control.Functor.Pointed
import Control.Arrow (first,second)

test :: [(SourcePosition String,String,())]
test = wordsFrom (point "-") "Hello World\nThis is a test"
