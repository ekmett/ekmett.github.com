module Type.Sign (TSign, Negative, Positive, SignZero) where

-- TODO: order them naturally?
import Type.Boolean
import Type.Ord

data Closure
class Closed a | -> a
instance Closed Closure

data Negative
data SignZero
data Positive

class TCSign c a | a -> c
instance TCSign Closure Negative
instance TCSign Closure Positive
instance TCSign Closure SignZero

class TCSign Closure s => TSign s
instance TSign Negative
instance TSign SignZero
instance TSign Positive

instance TEq Negative Negative T
instance TEq Negative SignZero F
instance TEq Negative Positive F
instance TEq SignZero Negative F
instance TEq SignZero SignZero T
instance TEq SignZero Positive F
instance TEq Positive Negative F
instance TEq Positive SignZero F
instance TEq Positive Positive T
