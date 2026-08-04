-- 1
-- Conversion to wholemeal programming

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x : xs)
  | even x  = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' xs = foldr (\x y -> (x - 2) * y) 1 (filter even xs)

-- 2
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' n = sum $ takeWhile (>0) (filter even (iterate f n))
  where f 1 = 0
        f n' | even n' = n' `div` 2
             | otherwise = 3 * n' + 1

-- testing for 100 numbers
test1 = foldr (\x y -> x && y) True (map (\x -> fun2 x == fun2' x) (takeWhile (<1000) [1..]))


-- Exercise 2 : Folding with trees

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

createNewNode :: a -> Tree a
createNewNode v = Node 0 Leaf v Leaf

insertToTree :: a -> Tree a -> Tree a
insertToTree v Leaf = createNewNode v
insertToTree v (Node h Leaf v' Leaf) = Node (h+1) (createNewNode v) v' Leaf
insertToTree v (Node h lst v' rst) = Node h' lst' v' rst'
  where
    getRootHeight :: Tree a -> Integer
    getRootHeight Leaf = -1
    getRootHeight (Node h _ _ _) = h

    go | getRootHeight lst > getRootHeight rst = (lst, insertToTree v rst)
           | otherwise   = (insertToTree v lst , rst)
    (lst', rst') = go
    h' = max (getRootHeight lst') (getRootHeight rst') + 1

foldTree :: [a] -> Tree a
foldTree = foldr insertToTree Leaf

-- Node 3 (Node 2 (Node 1 (Node 0 Leaf 'D' Leaf) 'G' Leaf) 'I' (Node 1 (Node 0 Leaf 'A' Leaf) 'E' Leaf))
-- 'J'
-- (Node 2 (Node 1 (Node 0 Leaf 'B' Leaf) 'F' Leaf) 'H' (Node 0 Leaf 'C' Leaf))
