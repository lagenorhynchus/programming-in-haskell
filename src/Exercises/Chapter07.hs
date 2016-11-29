module Exercises.Chapter07 where

-- Higher-order functions

import Data.Char

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

-- Exercise 3
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

-- Exercise 4
dec2int :: [Int] -> Int
dec2int = foldl (\acc x -> acc * 10 + x) 0

-- Exercise 6
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x, y)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (x, y) = f x y

-- Exercise 7
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x
    | p x       = []
    | otherwise = h x : unfold p h t (t x)

type Bit = Int

int2bin :: Int -> [Bit]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8)

map'' :: (a -> b) -> [a] -> [b]
map'' f = unfold null (f . head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' = unfold (const False) id

-- Exercise 8
bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

chop9 :: [Bit] -> [[Bit]]
chop9 = unfold null (take 9) (drop 9)

parity :: [Bit] -> Bit
parity bits
    | odd $ sum bits = 1
    | otherwise      = 0

addParity :: [Bit] -> [Bit]
addParity bits = parity bits : bits

checkParity :: [Bit] -> [Bit]
checkParity (bit:bits)
    | bit == parity bits = bits
    | otherwise          = error "parity mismatch"

encodeWithParity :: String -> [Bit]
encodeWithParity = concatMap $ addParity . make8 . int2bin . ord

decodeWithParity :: [Bit] -> String
decodeWithParity = map (chr . bin2int . checkParity) . chop9


-- Exercise 9
errorChannel :: [Bit] -> [Bit]
errorChannel = tail

transmit :: String -> String
transmit = decodeWithParity . errorChannel . encodeWithParity
