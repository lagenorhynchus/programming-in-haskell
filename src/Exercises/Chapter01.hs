module Exercises.Chapter01 where

-- Introduction

-- Exercise 1
{-
  double (double 2)
= double (2 + 2)
= (2 + 2) + (2 + 2)
= 4 + (2 + 2)
= 4 + 4
= 8
-}

-- Exercise 2
{-
  sum [x]
= x + sum []
= x + 0
= x
-}

-- Exercise 3
product' :: Num a => [a] -> a
product' []     = 1
product' (x:xs) = x * product' xs

{-
  product' [2, 3, 4]
= 2 * product' [3, 4]
= 2 * (3 * product' [4])
= 2 * (3 * (4 * product' []))
= 2 * (3 * (4 * 1))
= 24
-}

-- Exercise 4
qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller
  where
    smaller = [a | a <- xs, a <= x]
    larger  = [b | b <- xs, b > x]

-- Exercise 5
qsort' :: Ord a => [a] -> [a]
qsort' []     = []
qsort' (x:xs) = qsort' smaller ++ [x] ++ qsort' larger
  where
    smaller = [a | a <- xs, a < x]
    larger  = [b | b <- xs, b > x]

{-
  qsort' [2, 2, 3, 1, 1]
= qsort' [1, 1] ++ [2] ++ qsort' [3]
= (qsort' [] ++ [1] ++ qsort' []) ++ [2] ++ (qsort' [] ++ [3] ++ qsort' [])
= ([] ++ [1] ++ []) ++ [2] ++ ([] ++ [3] ++ [])
= [1, 2, 3]
-}
