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
