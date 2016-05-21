module Chapter06 where

-- Exercise 1
(^.) :: (Num a, Integral b) => a -> b -> a
_ ^. 0 = 1
n ^. m = n * (n ^. (m - 1))

-- Exercise 3
and' :: [Bool] -> Bool
and' []     = True
and' (x:xs) = x && and' xs

concat' :: [[a]] -> [a]
concat' []       = []
concat' (xs:xss) = xs ++ concat' xss

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n - 1) x

(!!.) :: [a] -> Int -> a
(x:_)  !!. 0 = x
(_:xs) !!. n = xs !!. (n - 1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys)
  | x == y = True
  | otherwise = elem' x ys

-- Exercise 4
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

-- Exercise 5
msort :: Ord a => [a] -> [a]
msort []     = []
msort [x]    = [x]
msort xs     = merge (msort ys) (msort zs)
  where
    (ys, zs) = halve xs
    halve ls = splitAt (length ls `div` 2) ls

-- Exercise 6
sum' :: Num a => [a] -> a
sum' = foldr (+) 0

take' :: Int -> [a] -> [a]
take' _ []     = []
take' 0 _      = []
take' n (x:xs) = x : take' (n - 1) xs

last' :: [a] -> a
last' [x]    = x
last' (_:xs) = last' xs
