fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [1..]

-- Exercise 2
fib' :: Integer -> Integer -> [Integer]
fib' a b = a : fib' b (a+b)

fibs2 :: [Integer]
fibs2 = fib' 0 1
