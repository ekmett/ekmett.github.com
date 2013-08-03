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
-- 'Monoid' instances for working with a 'Monad'
--
-----------------------------------------------------------------------------

module Data.Monoid.Monad 
    ( module Data.Monoid.Reducer
    , module Data.Ring.Module
    -- * Actions
    , Action(Action,getAction)
    , snocAction
    -- * MonadPlus Monoid
    , MonadSum(MonadSum, getMonadSum)
    -- * Lifting Modules
    , Mon(Mon,getMon)
    ) where

import Control.Applicative
import Control.Functor.Pointed
import Data.Monoid.Reducer
import Data.Ring.Module
import Control.Monad

-- | An 'Action' uses glues together 'Monad' actions with (>>)
--   in the manner of 'mapM_' from "Data.Foldable". Any values returned by 
--   reduced actions are discarded.
newtype Action m = Action { getAction :: m () } 

instance Monad m => Monoid (Action m) where
    mempty = Action (return ())
    Action a `mappend` Action b = Action (a >> b)

instance Monad m => Reducer (m a) (Action m) where
    unit a = Action (a >> return ())
    a `cons` Action b = Action (a >> b)
    Action a `snoc` b = Action (a >> b >> return ())

{-# RULES "unitAction" unit = Action #-}
{-# RULES "snocAction" snoc = snocAction #-} 

-- | Efficiently avoid needlessly rebinding when using 'snoc' on an action that already returns ()
--   A rewrite rule automatically applies this when possible
snocAction :: Reducer (m ()) (Action m) => Action m -> m () -> Action m
snocAction a = mappend a . Action

-- | A 'MonadSum' turns any 'MonadPlus' instance into a 'Monoid'.
--   It also provides a 'Multiplicative' instance for a 'Monad' wrapped around a 'Monoid'
--   and asserts that any 'MonadPlus' applied to a 'Monoid' forms a 'RightSemiNearRing' 
--   under these operations.

newtype MonadSum m a = MonadSum { getMonadSum :: m a } 
    deriving (Eq,Ord,Show,Read,Monad,MonadPlus)

instance MonadPlus m => Monoid (MonadSum m a) where
    mempty = mzero
    mappend = mplus

instance (Monad m, Monoid a) => Multiplicative (MonadSum m a) where
    one = return mempty
    times = liftM2 mappend

instance Monad m => Functor (MonadSum m) where
    fmap = liftM

instance Monad m => Applicative (MonadSum m) where
    pure = return
    (<*>) = ap

instance Monad m => Pointed (MonadSum m) where
    point = return

instance MonadPlus m => Reducer (m a) (MonadSum m a) where
    unit = MonadSum

instance (MonadPlus m, Monoid a) => Ringoid (MonadSum m a)

instance (MonadPlus m, Monoid a) => RightSemiNearRing (MonadSum m a)

-- | if @m@ is a 'Module' over @r@ and @f@ is a 'Monad' then @f `Mon` m@ is a 'Module' as well

newtype Mon f m = Mon { getMon :: f m } 
    deriving (Eq,Ord,Show,Read,Functor,Pointed, Monad,MonadPlus)

instance (Monoid m, Monad f) => Monoid (f `Mon` m) where
    mempty = return mempty
    mappend = liftM2 mappend

instance (Group m, Monad f) => Group (f `Mon` m) where
    gnegate = liftM gnegate
    minus = liftM2 minus
    gsubtract = liftM2 gsubtract

instance (c `Reducer` m, Monad f) => Reducer c (f `Mon` m) where
    unit = return . unit

instance (LeftModule r m, Monad f) => LeftModule r (f `Mon` m) where
    x *. m = liftM (x *.) m

instance (RightModule r m, Monad f) => RightModule r (f `Mon` m) where
    m .* y = liftM (.* y) m

instance (Module r m, Monad f) => Module r (f `Mon` m)
