module Chapter04 where

-- Exercise 1
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

halve' :: [a] -> ([a], [a])
halve' xs = (take n xs, drop n xs)
  where
    n = length xs `div` 2

-- Exercise 2
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

safetail' :: [a] -> [a]
safetail' xs
  | null xs   = []
  | otherwise = tail xs

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' xs = tail xs

-- Exercise 3
(||.) :: Bool -> Bool -> Bool
True  ||. True  = True
True  ||. False = True
False ||. True  = True
False ||. False = False

(||..) :: Bool -> Bool -> Bool
False ||.. False = False
_     ||.. _     = True

(||...) :: Bool -> Bool -> Bool
False ||... b = b
_     ||... _ = True

(||....) :: Bool -> Bool -> Bool
b ||.... c
  | b == c    = b
  | otherwise = True

-- Exercise 4
(&&.) :: Bool -> Bool -> Bool
a &&. b  =
  if a then if b then True else False
  else False

-- Exercise 5
(&&..) :: Bool -> Bool -> Bool
a &&.. b  = if a then b else False

-- Exercise 6
mult :: Num a => a -> a -> a -> a
mult = \x -> \y -> \z -> x * y * z
