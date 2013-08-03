{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts,  GeneralizedNewtypeDeriving, PatternGuards, BangPatterns, TypeSynonymInstances #-} 
module Language.Kata.Parsing.Iteratee 
    ( Iteratee
    , Enumerator, andThen
    , Buffer, Chunk(..)
    , Cursor(..)
    , Supply, supply
    , EOF(..)
    , runIteratee
    , slice
    , replacementChar
    , unconsWord8
    ) where

import Control.Applicative ((<$>), Applicative, pure, (<*>), Alternative, empty, (<|>))
import Control.Monad (MonadPlus, mzero, mplus)
import Data.Bits ((.|.),(.&.),shiftL)
import Data.Foldable (toList)
import Data.FingerTree (FingerTree, measure, ViewL(..),(><))
import qualified Data.FingerTree as F
import Data.Monoid (Monoid, mempty, mappend)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.String.UTF8 as UTF8
import Data.Word (Word8)
import Text.Parsec.Prim (uncons)
import qualified Text.Parsec.Prim as P
import Text.Show (Show, showParen, showsPrec, showString, showChar)

newtype Cursor = Cursor { getCursor :: Int } deriving (Eq,Ord,Num,Show,Read,Enum)

instance Monoid Cursor where
    mempty = 0
    mappend = (+)

newtype Chunk = Chunk { getChunk :: S.ByteString } deriving (Eq,Ord,Show,Read)

instance F.Measured Cursor Chunk where
    measure (Chunk xs) = Cursor (S.length xs)

type Buffer = FingerTree Cursor Chunk

data Iteratee a 
    = Done a !Buffer !Bool
    | Cont (Buffer -> Bool -> Iteratee a)
    | Fail String !Buffer !Bool

instance Functor Iteratee where
    fmap f (Done a h eof) = Done (f a) h eof
    fmap f (Cont k) = Cont (\h eof -> f `fmap` k h eof)
    fmap _ (Fail s h eof) = Fail s h eof

instance Monad Iteratee where
    return a = Done a F.empty False

    Done a h False >>= f | F.null h = f a
    Done a h eof >>= f = case f a of
        Done b _ _ -> Done b h eof
        Cont k -> k h eof
        Fail s _ _ -> Fail s h eof
    Cont k >>= f    = Cont (\h eof -> k h eof >>= f) 
    Fail s h eof >>= _ = Fail s h eof

    fail s = Fail s F.empty False

instance Applicative Iteratee where
    pure = return

    Done f h eof <*> c = case c of
        Done b _ _ -> Done (f b) h eof
        Cont k     -> f <$> k h eof 
        Fail s _ _ -> Fail s h eof
    Cont k <*> r = Cont (\h eof -> k h eof <*> r)
    Fail s h eof <*> _ = Fail s h eof

instance Alternative Iteratee where
    empty = Fail "empty" F.empty False

    d@(Done _ _ _) <|> _ = d
    Cont k <|> r = Cont (\h eof -> k h eof <|> r)
    f@(Fail _ h eof) <|> rhs = case rhs of
        Done a _ _ -> Done a h eof
        Cont k     -> k h eof
        Fail _ _ _ -> f

instance MonadPlus Iteratee where
    mzero = empty
    mplus = (<|>)

getWord8 :: Cursor -> Iteratee Word8
getWord8 !n = Cont getWord8' where
    getWord8' :: Buffer -> Bool -> Iteratee Word8
    getWord8' h eof 
        | n < measure h = Done (index n h) h eof
        | eof           = Fail "EOF" h eof
        | otherwise     = Cont (\h' eof' -> getWord8' (h >< h') eof')

    index :: Cursor -> Buffer -> Word8
    index !i !t = S.index a $ getCursor (i - measure l) where 
        (l,r) = F.split (> i) t
        Chunk a :< _ = F.viewl r 

mask :: Word8 -> Word8 -> Int
mask c m = fromEnum (c .&. m) 

combine :: Int -> Word8 -> Int
combine a r = shiftL a 6 .|. fromEnum (r .&. 0x3f)

replacementChar :: Char
replacementChar = '\xFFFD'

b2 :: Word8 -> Word8 -> Char
b2 c d | valid_b2 = toEnum (combine (mask c 0x1f) d)
       | otherwise = replacementChar where
    valid_b2 = c >= 0xc2 && c <= 0xdf && d >= 0x80 && d <= 0xbf

b3 :: Word8 -> Word8 -> Word8 -> Char
b3 c d e | valid_b3 = toEnum (combine (combine (mask c 0x0f) d) e)
         | otherwise = replacementChar where
    valid_b3 
        = (c == 0xe0 && d >= 0xa0 && d <= 0xbf && e >= 0x80 && e <= 0xbf) 
       || (c >= 0xe1 && c <= 0xef && d >= 0x80 && d <= 0xbf && e >= 0x80 && e <= 0xbf)

b4 :: Word8 -> Word8 -> Word8 -> Word8 -> Char
b4 c d e f | valid_b4 = toEnum (combine (combine (combine (mask c 0x07) d) e) f)
           | otherwise = replacementChar where
    valid_b4
      = (c == 0xf0 && d >= 0x90 && d <= 0xbf && e >= 0x80 && e <= 0xbf && f >= 0x80 && f <= 0xbf) 
     || (c >= 0xf1 && c <= 0xf3 && d >= 0x80 && d <= 0xbf && e >= 0x80 && e <= 0xbf && f >= 0x80 && f <= 0xbf) 
     || (c == 0xf4 && d >= 0x80 && d <= 0x8f && e >= 0x80 && e <= 0xbf && f >= 0x80 && f <= 0xbf)

instance P.Stream Cursor Iteratee Char where
    uncons !n = (getWord8 n >>= uncons') <|> return Nothing where
        uncons' c 
            | c <= 0x7f = 
                return $ Just (toEnum (fromEnum c), n + 1)
            | c >= 0xc2 && c <= 0xdf = do
                d <- getWord8 (n + 1)
                return $ Just (b2 c d, n + 2)
            | c >= 0xe0 && c <= 0xef = do
                d <- getWord8 (n + 1)
                e <- getWord8 (n + 2)
                return $ Just (b3 c d e, n + 3)
            | c >= 0xf0 && c <= 0xf4 = do
                d <- getWord8 (n + 1)
                e <- getWord8 (n + 2)
                f <- getWord8 (n + 3)
                return $ Just (b4 c d e f, n + 4)
            | otherwise = 
                return $ Just (replacementChar, n + 1)

unconsWord8 :: Cursor -> Iteratee (Maybe (Word8,Cursor))
unconsWord8 !n = (getWord8 n >>= uncons8') <|> return Nothing where
    uncons8' c = return (Just (c, n + 1))

-- requires i <= j
slice :: Cursor -> Cursor -> Iteratee S.ByteString
slice !i !j = Cont slice' where

    slice' :: Buffer -> Bool -> Iteratee S.ByteString
    slice' h eof
        | j <= measure h || eof = Done (sliceBuffer h) h eof
        | otherwise = Cont $ \h' eof' -> slice' (h >< h') eof'

    sliceBuffer :: Buffer -> S.ByteString
    sliceBuffer !t
        | req <= rmn = S.take req first
        | otherwise = 
            S.concat $ L.toChunks $ 
            L.take (fromIntegral req) $ 
            L.fromChunks $ first : map getChunk (toList r')
        where 
            (l,r) = F.split (> i) t
            Chunk a :< r' = F.viewl r
            first = S.drop (getCursor (i - measure l)) a
            req   = getCursor $ j - i
            rmn   = S.length first

type Enumerator a = Iteratee a -> Iteratee a

andThen :: Enumerator a -> Enumerator a -> Enumerator a
andThen = flip (.)

class Supply t where
    supply :: t -> Enumerator a
    supplyList :: [t] -> Enumerator a

    supplyList = foldr (andThen . supply) id

supplyBuffer :: Buffer -> Enumerator a
supplyBuffer b (Cont k) = k b False
supplyBuffer _ done = done

supplyStrictByteString :: S.ByteString -> Enumerator a
supplyStrictByteString = supplyBuffer . F.singleton . Chunk

instance Supply Buffer where
    supply = supplyBuffer

instance Supply S.ByteString where
    supply = supplyStrictByteString
    supplyList = supplyBuffer . F.fromList . map Chunk

instance Supply L.ByteString where
    supply = supplyList . L.toChunks

instance Supply Char where
    supply = supplyStrictByteString . UTF8.toRep . UTF8.fromString . return
    supplyList = supplyStrictByteString . UTF8.toRep . UTF8.fromString

instance Supply a => Supply [a] where
    supply = supplyList

data EOF = EOF

instance Supply EOF where
    supply _ (Fail s h _) = Fail s h True
    supply _ (Cont k)     = k F.empty True
    supply _ (Done a h _) = Done a h True

-- | You probably want to `runIteratee . supply EOF` to flush out Cont nodes

runIteratee :: Iteratee a -> Maybe (Either String a, L.ByteString, Bool)
runIteratee (Done a h eof) = Just (Right a, L.fromChunks $ map getChunk $ toList h, eof)
runIteratee (Fail s h eof) = Just (Left s, L.fromChunks $ map getChunk $ toList h, eof)
runIteratee (Cont _)       = Nothing

instance Show a => Show (Iteratee a) where
    showsPrec d (Done a h eof) = showParen (d > 10) $ 
        showString "Done " . 
        showsPrec 11 a . showChar ' ' . 
        showsPrec 11 h . showChar ' ' . 
        showsPrec 11 eof
    showsPrec d (Cont _) = showParen (d > 10) $ showString "Cont <function>" 
    showsPrec d (Fail s h eof) = showParen (d > 10) $ 
        showString "Fail " . 
        showsPrec 11 s . showChar ' ' . 
        showsPrec 11 h . showChar ' ' . 
        showsPrec 11 eof
