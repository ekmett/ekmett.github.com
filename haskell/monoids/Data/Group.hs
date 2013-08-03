----------------------------------------------------------------------------
-- |
-- Module     : Data.Group
-- Copyright  : 2007-2009 Edward Kmett
-- License    : BSD
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Extends 'Monoid' to support 'Group' operations
-----------------------------------------------------------------------------

module Data.Group 
    ( module Data.Monoid.Multiplicative
    , Group
    , gnegate
    , gsubtract
    , minus
    , MultiplicativeGroup
    , over
    , under
    , grecip
    ) where

import Data.Monoid.Multiplicative
import Data.Monoid.Self

#ifdef X_OverloadedStrings
import Data.Monoid.FromString
#endif

infixl 6 `minus`

-- | Minimal complete definition: 'gnegate' or 'minus'
class Monoid a => Group a where
    -- additive inverse
    gnegate :: a -> a
    minus :: a -> a -> a
    gsubtract :: a -> a -> a 

    gnegate = minus zero
    a `minus` b = a `plus` gnegate b 
    a `gsubtract` b = gnegate a `plus` b

instance Num a => Group (Sum a) where
    gnegate = Sum . negate . getSum
    Sum a `minus` Sum b = Sum (a - b)
    
instance Fractional a => Group (Product a) where
    gnegate = Product . negate . getProduct
    Product a `minus` Product b = Product (a / b)
    
instance Group a => Group (Dual a) where
    gnegate = Dual . gnegate . getDual

instance Group a => Group (Self a) where
    gnegate = Self . gnegate . getSelf
    Self a `minus` Self b = Self (a `minus` b)

-- | Minimal definition over or grecip
class Multiplicative g => MultiplicativeGroup g where
    -- | @x / y@
    over :: g -> g -> g
    -- | @x \ y@
    under :: g -> g -> g
    grecip :: g -> g

    x `under` y = grecip x `times` y
    x `over` y = x `times` grecip y
    grecip x = one `over` x

instance MultiplicativeGroup g => Group (Log g) where
    Log x `minus` Log y = Log (x `over` y)
    Log x `gsubtract` Log y = Log (x `under` y)
    gnegate (Log x) = Log (grecip x)

instance Group g => MultiplicativeGroup (Exp g) where
    Exp x `over` Exp y = Exp (x `minus` y)
    Exp x `under` Exp y = Exp (x `gsubtract` y)
    grecip (Exp x) = Exp (gnegate x)

instance MultiplicativeGroup g => MultiplicativeGroup (Self g) where
    Self x `over` Self y = Self (x `over` y)
    Self x `under` Self y = Self (x `under` y)
    grecip (Self x) = Self (grecip x)

#ifdef M_REFLECTION
instance MultiplicativeGroup g => MultiplicativeGroup (ReducedBy g s) where
    Reduction x `over` Reduction y = Reduction (x `over` y)
    Reduction x `under` Reduction y = Reduction (x `under` y)
    grecip (Reduction x) = Reduction (grecip x)

instance Group a => Group (ReducedBy a s) where
    gnegate = Reduction . gnegate . getReduction
    Reduction a `minus` Reduction b = Reduction (a `minus` b)
    Reduction a `gsubtract` Reduction b = Reduction (a `gsubtract` b)
#endif

instance MultiplicativeGroup a => MultiplicativeGroup (Dual a) where
    grecip = Dual . grecip . getDual

#ifdef X_OverloadedStrings
instance MultiplicativeGroup g => MultiplicativeGroup (FromString g) where
    FromString x `over` FromString y = FromString (x `over` y)
    FromString x `under` FromString y = FromString (x `under` y)
    grecip (FromString x) = FromString (grecip x)

instance Group a => Group (FromString a) where
    gnegate = FromString . gnegate . getFromString
    FromString a `minus` FromString b = FromString (a `minus` b)
    FromString a `gsubtract` FromString b = FromString (a `gsubtract` b)
#endif

