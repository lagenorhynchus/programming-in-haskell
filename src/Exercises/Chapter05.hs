module Exercises.Chapter05 where

-- List comprehensions

import Data.Char

-- Exercise 1
sumSquares :: Int
sumSquares = sum [n ^ 2 | n <- [1..100]]

-- Exercise 2
replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]

-- Exercise 3
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n]
                     , y <- [1..n]
                     , z <- [1..n]
                     , x ^ 2 + y ^ 2 == z ^ 2]

pyths' :: Int -> [(Int, Int, Int)]
pyths' n = [(x, y, z) | x <- [1..n]
                      , y <- [x..n]
                      , z <- [y..n]
                      , x ^ 2 + y ^ 2 == z ^ 2]

-- Exercise 4
perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (init $ factors x) == x]
  where
    factors m = [x | x <- [1..m], m `mod` x == 0]

-- Exercise 5
ns :: [(Int, Int)]
ns = concat [[(x, y) | y <- [4, 5, 6]] | x <- [1, 2, 3]]

-- Exercise 6
positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x $ zip xs [0..]
  where
    find :: Eq a => a -> [(a, b)] -> [b]
    find k t = [v | (k', v) <- t, k == k']

-- Exercise 7
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]

-- Exercise 8
shift :: Int -> Char -> Char
shift n c
    | isLower c = int2lower $ (lower2int c + n) `mod` 26
    | isUpper c = int2upper $ (upper2int c + n) `mod` 26
    | otherwise = c
  where
    lower2int c' = ord c' - ord 'a'
    int2lower n' = chr $ ord 'a' + n'
    upper2int c' = ord c' - ord 'A'
    int2upper n' = chr $ ord 'A' + n'

encode :: Int -> String -> String
encode n cs = [shift n c | c <- cs]

decode :: Int -> String -> String
decode n = encode $ negate n

freqs :: String -> [Float]
freqs xs = [percent (count x xs') total | x <- ['a'..'z']]
  where
    percent n m = (fromIntegral n / fromIntegral m) * 100
    count c cs = sum [1 | c' <- cs, c == c']
    xs' = map toLower xs
    total = sum [1 | x <- xs, isAlpha x]

table :: [Float]
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4,
         6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [(o - e) ^ 2 / e| (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack cs = decode factor cs
  where
    factor = head $ positions (minimum chitab) chitab
    chitab = [chisqr (rotate n table') table | n <- [0..25]]
    table' = freqs cs
