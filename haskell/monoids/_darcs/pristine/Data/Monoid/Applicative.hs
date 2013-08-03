{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Applicative
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (MPTCs)
--
-- Monoids for working with an 'Applicative' 'Functor'.
--
-----------------------------------------------------------------------------

module Data.Monoid.Applicative 
    ( module Data.Monoid.Reducer
    , module Data.Ring.Module
    , Traversal(Traversal,getTraversal)
    , Alt(Alt,getAlt)
    , App(App,getApp)
    , snocTraversal
    ) where

import Control.Applicative
import Data.Monoid.Reducer
import Data.Ring.Module
import Control.Functor.Pointed

-- | A 'Traversal' uses an glues together 'Applicative' actions with (*>)
--   in the manner of 'traverse_' from "Data.Foldable". Any values returned by 
--   reduced actions are discarded.
newtype Traversal f = Traversal { getTraversal :: f () } 

instance Applicative f => Monoid (Traversal f) where
    mempty = Traversal (pure ())
    Traversal a `mappend` Traversal b = Traversal (a *> b)

instance Applicative f => Reducer (f a) (Traversal f) where
    unit a = Traversal (a *> pure ())
    a `cons` Traversal b = Traversal (a *> b)
    Traversal a `snoc` b = Traversal (a *> b *> pure ())


-- | Efficiently avoid needlessly rebinding when using 'snoc' on an action that already returns ()
--   A rewrite rule automatically applies this when possible
snocTraversal :: Reducer (f ()) (Traversal f) => Traversal f -> f () -> Traversal f
snocTraversal a = mappend a . Traversal
{-# RULES "unitTraversal" unit = Traversal #-}
{-# RULES "snocTraversal" snoc = snocTraversal #-}

-- | A 'Alt' turns any 'Alternative' instance into a 'Monoid'.
--   It also provides a 'Multiplicative' instance for an 'Applicative' functor wrapped around a 'Monoid'
--   and asserts that any 'Alternative' applied to a 'Monoid' forms a 'RightSemiNearRing' 
--   under these operations.

newtype Alt f a = Alt { getAlt :: f a } 
    deriving (Eq,Ord,Show,Read,Functor,Applicative,Alternative,Copointed)

instance Alternative f => Monoid (Alt f a) where
    mempty = empty 
    Alt a `mappend` Alt b = Alt (a <|> b) 

instance (Applicative f, Monoid a) => Multiplicative (Alt f a) where
    one = pure mempty
    times = liftA2 mappend

instance Applicative f => Pointed (Alt f) where
    point = pure

instance Alternative f => Reducer (f a) (Alt f a) where
    unit = Alt 

instance (Alternative f, Monoid a) => Ringoid (Alt f a)

instance (Alternative f, Monoid a) => RightSemiNearRing (Alt f a)

-- | if @m@ is a 'Module' over @r@ and @f@ is a 'Applicative' then @f `App` m@ is a 'Module' over @r@ as well

newtype App f m = App { getApp :: f m } 
    deriving (Eq,Ord,Show,Read,Functor,Applicative,Alternative,Pointed,Copointed)

instance (Monoid m, Applicative f) => Monoid (f `App` m) where
    mempty = pure mempty
    mappend = liftA2 mappend

instance (Group m, Applicative f) => Group (f `App` m) where
    gnegate = fmap gnegate
    minus = liftA2 minus
    gsubtract = liftA2 gsubtract

instance (c `Reducer` m, Applicative f) => Reducer c (f `App` m) where
    unit = pure . unit

instance (LeftModule r m, Applicative f) => LeftModule r (f `App` m) where x *. m = (x *.) <$> m
instance (RightModule r m, Applicative f) => RightModule r (f `App` m) where m .* y = (.* y) <$> m
instance (Module r m, Applicative f) => Module r (f `App` m)
