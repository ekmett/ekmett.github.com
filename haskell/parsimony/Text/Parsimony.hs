-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parsimony
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (type families, GADTs)
--
-- Parallel parser combinators
--
-----------------------------------------------------------------------------

module Text.Parsimony
    ( module Text.Parsimony.Char
    , module Text.Parsimony.Error
    , module Text.Parsimony.Mode
    , module Text.Parsimony.Prim
    ) where

import Text.Parsimony.Char
import Text.Parsimony.Error
import Text.Parsimony.Mode
import Text.Parsimony.Prim
