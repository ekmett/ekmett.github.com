{-# LANGUAGE UndecidableInstances, TypeOperators, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Reducer.With
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (MPTCs)
--
-----------------------------------------------------------------------------

module Data.Monoid.Reducer.With
    ( module Data.Monoid.Reducer
    , WithReducer(WithReducer,withoutReducer)
    ) where

import Data.Monoid.Reducer
import Data.FingerTree

-- | If @m@ is a @c@-"Reducer", then m is @(c `WithReducer` m)@-"Reducer"
--   This can be used to quickly select a "Reducer" for use as a 'FingerTree'
--   'measure'.

newtype WithReducer c m = WithReducer { withoutReducer :: c } 

instance (c `Reducer` m) => Reducer (c `WithReducer` m) m where
    unit = unit . withoutReducer 

instance (c `Reducer` m) => Measured m (c `WithReducer` m) where
    measure = unit . withoutReducer
