module Chapter07 where

-- Exercise 1
g :: (a -> Bool) -> (a -> b) -> [a] -> [b]
g p f xs = [f x | x <- xs, p x]

g' :: (a -> Bool) -> (a -> b) -> [a] -> [b]
g' p f = map f . filter p

-- Exercise 2
all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p

any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
  | p x       = x : takeWhile' p xs
  | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs)
  | p x       = dropWhile' p xs
  | otherwise = x : xs
