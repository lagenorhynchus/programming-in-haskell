module Exercises.Chapter08 where

-- Functional parsers

import Control.Applicative ( Alternative(..) )
import Data.Char ( isAlpha
                 , isAlphaNum
                 , isDigit
                 , isLower
                 , isSpace
                 , isUpper
                 )

newtype Parser a = P (String -> [(a, String)])

failure :: Parser a
failure = P $ const []

item :: Parser Char
item = P $ \inp -> case inp of
    []     -> []
    (x:xs) -> [(x, xs)]

parse :: Parser a -> String -> [(a, String)]
parse (P p) = p

instance Functor Parser where
    fmap f p = P $ \inp -> case parse p inp of
        []         -> []
        [(v, out)] -> [(f v, out)]

instance Applicative Parser where
    pure v = P $ \inp -> [(v, inp)]

    pf <*> px = P $ \inp -> case parse pf inp of
        []         -> []
        [(f, out)] -> parse (fmap f px) out

instance Monad Parser where
    p >>= f = P $ \inp -> case parse p inp of
        []         -> []
        [(v, out)] -> parse (f v) out

instance Alternative Parser where
    empty = P $ const []

    p <|> q = P $ \inp -> case parse p inp of
        []         -> parse q inp
        [(v, out)] -> [(v, out)]

sat :: (Char -> Bool) -> Parser Char
sat p = do
    x <- item
    if p x
        then return x
        else failure

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char c = sat (== c)

string :: String -> Parser String
string []     = return []
string (c:cs) = do
    c' <- char c
    cs' <- string cs
    return $ c' : cs'

many' :: Parser a -> Parser [a]
many' p = many1 p <|> return []

many1 :: Parser a -> Parser [a]
many1 p = do
    v <- p
    vs <- many' p
    return $ v : vs

ident :: Parser String
ident = do
    x <- lower
    xs <- many' alphanum
    return $ x : xs

nat :: Parser Int
nat = do
    xs <- many1 digit
    return $ read xs

space :: Parser ()
space = do
    _ <- many' $ sat isSpace
    return ()

token :: Parser a -> Parser a
token p = do
    space
    v <- p
    space
    return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol = token . string

expr :: Parser Int
expr = do
    t <- term
    do
        _ <- symbol "+"
        e <- expr
        return $ t + e
        <|> return t

term :: Parser Int
term = do
    f <- factor
    do
        _ <- symbol "*"
        t <- term
        return $ f * t
        <|> return f

factor :: Parser Int
factor = do
    _ <- symbol "("
    e <- expr
    _ <- symbol ")"
    return e
    <|> natural

eval :: String -> Int
eval xs = case parse expr xs of
    [(n, [])]  -> n
    [(_, out)] -> error $ "unused input " ++ out
    []         -> error "invalid input"
