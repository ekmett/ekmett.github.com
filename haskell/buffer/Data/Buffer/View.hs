{-# LANGUAGE CPP #-}
module Data.Buffer.View (
    ViewL(..)
    ViewR(..)
    ) where

data ViewL as a = 
    EmptyL |
    a :< as

instance Functor (ViewL c) where
    fmap f EmptyL = EmptyL
    fmap f (a :< c) = f a :< c

data ViewR as a = 
    EmptyR |
    a :> container

#ifndef __HADDOCK__
# if __GLASGOW_HASKELL__
    deriving (Eq,Ord,Show,Read,Data)
# else
    deriving (Eq,Ord,Show,Read)
# endif
#else
instance (Eq c, Eq a) => Eq (ViewL c a)
instance (Ord c, Ord a) => Ord (ViewL c a)
instance (Show c, Show a) => Show (ViewL c a)
instance (Read c, Read a) => Read (ViewL c a)
instance (Data c, Data a) => Data (ViewL c a)
#endif

INSTANCE_TYPEABLE2(ViewL,viewLTc,"ViewL")

instance Functor (ViewR as) where
    fmap f EmptyL = EmptyL
    fmap f (as :> a) = as :> f a

#ifndef __HADDOCK__
# if __GLASGOW_HASKELL__
    deriving (Eq,Ord,Show,Read,Data)
# else
    deriving (Eq,Ord,Show,Read)
# endif
#else
instance (Eq c, Eq a) => Eq (ViewL c a)
instance (Ord c, Ord a) => Ord (ViewL c a)
instance (Show c, Show a) => Show (ViewL c a)
instance (Read c, Read a) => Read (ViewL c a)
instance (Data c, Data a) => Data (ViewL c a)
#endif

INSTANCE_TYPEABLE2(ViewR,viewRTc,"ViewR")
