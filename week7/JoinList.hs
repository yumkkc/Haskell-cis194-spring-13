{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
module JoinList where
import Sized  
import Scrabble
import Buffer

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Show)


newtype Product a = Product a
  deriving (Show, Eq, Ord, Num)
instance Num a => Semigroup (Product a) where
  (<>) = (+)
instance Num a => Monoid (Product a) where
  mempty = Product 1


-- Exercise 1
-- TODO: Change this to balanced tree usinng AVL
-- (+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
-- (+++) jl1 jl2 = Append (tag1 <> tag2) jl1 jl2
--     where tag1 = tag jl1
--          tag2 = tag jl2

(+++) :: (Monoid m, Ord m) => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty jl2 = jl2
(+++) p@(Single m _) jl2 = Append (m <> tag jl2) p jl2
(+++) p@(Append m (Single _ _) (Single _ _)) jl2 = Append (m <> tag jl2) p jl2
(+++) (Append m a b) jl2  = Append m' a' b'
    where f | tag a < tag b = (a +++ jl2 , b )
            | tag b < tag a = (a , b +++ jl2)
            | otherwise     = (a +++ a''', b''' +++ jl2)
                                where (a''', b''') = prun b
          prun (Append m'' a'' b'') = (a'', b'')
          (a', b') = f
          m' = tag a' <> tag b'          

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- testing
tl1 = Single (Size 1) 'S'
tl2 = Single (Size 1) 'A'
tl3 = Single (Size 1) 'R'
tl4 = Single (Size 1) 'I'
tl5 = Single (Size 1) 'N'
tl6 = Single (Size 1) 'A'
jl1 = Single (Product 1) 'a'
jl2 :: JoinList (Product Integer) Char
jl2 = Single (Product 1) 'b'
jl3 = Single (Product 1) 'c'

sizes :: (Monoid  b, Sized b) => JoinList b a  -> Int
sizes e = getSize (size $ tag e)

-- Exercise 2
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i jl | i >= sizes jl  || i< 0 = Nothing
            | otherwise                    = go i jl
            where go _ Empty               = Nothing
                  go i (Single _ a)        = Just a
                  go i (Append _ a b )
                    | i < sizes a          = go i a
                    | otherwise            = go (i - sizes a) b

-- testing

test = ((((tl1 +++ tl2) +++ tl3) +++ tl4) +++ tl5) +++ tl6
-- 2
dropJ :: (Sized b, Monoid b, Ord b) => Int -> JoinList b a -> JoinList b a
dropJ n Empty = Empty
dropJ 0 jl    = jl
dropJ n (Single _ _) = Empty
dropJ n (Append m a b) 
  | n >= getSize (size m) = Empty
  | n <= sizes a          = dropJ n a +++ b
  | otherwise             = dropJ (n - sizes a) b

-- 3
takeJ :: (Sized b, Monoid b, Ord b) => Int -> JoinList b a -> JoinList b a
takeJ n Empty           = Empty
takeJ 0 jl              = Empty
takeJ n jl@(Single _ _) = jl
takeJ n jl@(Append m a b)
  | n >= getSize (size m) = jl
  | n <= sizes a          = takeJ n a
  | otherwise             = a +++ takeJ (n - sizes a) b

-- Exercise 3
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

scoreSize :: String -> JoinList (Score, Size) String
scoreSize s = Single (scoreString s, Size 1) s

replaceAtLast :: String 
  -> JoinList (Score, Size) String 
  -> JoinList (Score, Size) String  
replaceAtLast l Empty = Empty
replaceAtLast l (Single (m1, m2) a) = Single (scoreString l, Size 1) l
replaceAtLast l (Append m a b) = Append (tag a <> tag r') a r'
  where r' = replaceAtLast l b

instance Eq (Score, Size) where
  (a1, b1)  == (a2,b2) = b1 == b2

instance Ord (Score, Size) where
  compare (a1, b1) (a2, b2) = compare b1 b2


instance Buffer (JoinList (Score, Size) String) where
  toString Empty = ""
  toString (Single _ a) = a
  toString (Append _ a b) = toString a ++ "\n" ++ toString b

  fromString :: String -> JoinList (Score, Size) String
  fromString s = foldl (\jl a -> jl +++ scoreSize a) Empty $ lines s

  line = indexJ

  numLines = getSize . snd . tag

  value = getScore . fst . tag
  
  replaceLine :: Int 
    -> String
    -> JoinList (Score, Size) String
    -> JoinList (Score, Size) String
  replaceLine n l jl = jl' +++ dropJ (n+1) jl
    where njl = takeJ (n+1) jl
          jl' = if n >= numLines njl then njl else replaceAtLast l njl
            
t = fromString "saiyam\nis\nbebba\nlover\nand\nhe\nis\nbebba\nnine\nten" :: JoinList (Score, Size) String