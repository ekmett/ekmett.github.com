
-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Prelude
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (class-associated types)
--
-------------------------------------------------------------------------------------------
module Control.Category.Prelude where

import Prelude hiding (Functor, map, (.), id, fst, snd, curry, uncurry)
import qualified Prelude

import Control.Category.Classes
import Control.Category.Functor.Instances
import Control.Category.Dual
import Control.Category.Hask


