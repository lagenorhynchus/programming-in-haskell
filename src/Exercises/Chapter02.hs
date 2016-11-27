module Exercises.Chapter02 where

-- First steps

-- Exercise 1
{-
(2 ^ 3) * 4

(2 * 3) + (4 * 5)

2 + (3 * (4 ^ 5))
-}

-- Exercise 3
n :: Int
n = a `div` length xs
  where
    a  = 10
    xs = [1, 2, 3, 4, 5]

-- Exercise 4
last' :: [a] -> a
last' = head . reverse

last'' :: [a] -> a
last'' xs = xs !! (length xs - 1)

-- Exercise 5
init' :: [a] -> [a]
init' = reverse . tail . reverse

init'' :: [a] -> [a]
init'' xs = take (length xs - 1) xs
