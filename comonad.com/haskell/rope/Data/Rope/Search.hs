module Data.Rope.Search where

-- Hits (delta to first location) (count of hits) (overall tree width)
data Hits = Hits {-# UNPACK #-} (Maybe !Int) {-# UNPACK #-} !Int {-# UNPACK #-} !Int
data Hit = Hit {-# Delta #-} !Int
newtype Search = Search (FingerTree Indices (Indices Word8))

count :: Hits -> Int
count (Hits _ c _) = c

width :: Hits -> Int
width (Hits _ _ w) = w

first :: H

instance Monoid Hits where 
    mempty = Hits Nothing 0 0 
    Hits f c w `mappend` Hits f' c' w' =  Hits (f `mappend` f') (c+c') (w + w')

instance Measured Hits Hit where
    measure (Hit x) = Hits (Just x) 1 x 

-- given a means to extract a search result from the rope's annotation, split the rope in the middle of the results
splitWith :: Annotation a => (Rope a -> Search) -> Rope a -> (Rope a, Rope a)
splitWith f rope
        | c < 2 = (rope, mempty)
        | otherwise = splitAt (width l) rope
    where Search s = f rope
          c = count (measure s)
          g = c `div ` 2
          (l, r) = F.split (\hs -> count hs >= g) s

split :: Rope Search -> (Rope Search, Rope Search)
split = splitWith extract

instance Monoid Search where
    mempty = Search F.empty
    Search f `mappend` Search f' = Search (f >< f')

search :: Word8 -> Rope a -> Search
search w (Rope t _) 
