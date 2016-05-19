module Chapter03 where

-- Exercise 1
chars :: [Char]
chars = ['a', 'b', 'c']

charTuple :: (Char, Char, Char)
charTuple = ('a', 'b', 'c')

boolCharTuples :: [(Bool, Char)]
boolCharTuples = [(False, '0'), (True, '1')]

boolsCharsTuple :: ([Bool], [Char])
boolsCharsTuple = ([False, True], ['0', '1'])

listFuncs :: [[a] -> [a]]
listFuncs = [tail, init, reverse]

-- Exercise 2
second :: [a] -> a
second xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

pair :: a -> b -> (a, b)
pair x y = (x, y)

double :: Num a => a -> a
double x = x * 2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)
