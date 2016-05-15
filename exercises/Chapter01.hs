module Chapter01 where

-- Exercise 3
product' :: Num a => [a] -> a
product' []     = 1
product' (x:xs) = x * product' xs

-- Exercise 4
qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller
  where
    smaller = [a | a <- xs, a <= x]
    larger  = [b | b <- xs, b > x]
