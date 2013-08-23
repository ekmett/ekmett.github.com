module Data.Ring.Semi.Kleene 
    ( module Data.Ring
    , KleeneAlgebra
    , star
    ) where

import Data.Ring

class SemiRing r => KleeneAlgebra r where
    star :: r -> r
