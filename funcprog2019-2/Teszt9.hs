{-# LANGUAGE KindSignatures,DeriveFunctor,InstanceSigs #-}
module Teszt9 where

import Control.Monad.Writer
import Control.Applicative

newtype Parser a = P { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (P g) = P $ \str -> case g str of
        Just (x, str') -> Just (f x, str')
        Nothing -> Nothing

instance Applicative Parser where
    pure :: a -> Parser a
    pure a = P $ \str -> Just (a,str)
  
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (<*>) (P pF) (P g) = P $ \str -> do
        (f, str') <- pF str
        (x, str'') <- g str'
        pure (f x, str'')

eof :: Parser ()
eof = P $ \str -> case str of
    "" -> Just ((), str)
    _ -> Nothing

char :: Char -> Parser Char
char c = P $ \str -> case str of
    (x:xs) | x == c -> Just (c, xs)
    _               -> Nothing

instance Alternative Parser where
    empty :: Parser a
    empty = P $ \_ -> Nothing

    (<|>) :: Parser a -> Parser a -> Parser a
    (<|>) p g = P $ \str -> case runParser p str of
        Just res -> Just res
        Nothing -> runParser g str

charXY :: Parser Char
charXY = char 'x' <|> char 'y'

anyChar :: Parser Char
anyChar = foldr (<|>) empty $ map char ['a'..'z']

string :: String -> Parser [Char]
string = traverse char

as :: Parser [Char]
as = many (char 'a')

data Bit = T | F
    deriving (Eq, Ord, Show)

data ShortByte = SB Bit Bit Bit Bit
    deriving (Eq, Ord, Show)

bitF :: Parser Bit
bitF = char '0' *> pure F

bitT :: Parser Bit
bitT = char '1' *> pure T

bit :: Parser Bit
bit = bitF <|> bitT

shortByte :: Parser ShortByte
shortByte = SB <$> bit <*> bit <*> bit <*> bit

instance Monad Parser where
    return :: a -> Parser a
    return = pure

    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (>>=) p k = P $ \str -> case runParser p str of
        Nothing         -> Nothing
        Just (x, str')  -> runParser (k x) str'

times :: Int -> Parser a -> Parser [a]
times n p = traverse (\_ -> p) [1..n]

digit :: Parser Int
digit = fmap (\n -> n - 48)
        . fmap fromEnum
        . foldr (<|>) empty
        $ map char ['0'..'9']

intTuple :: Parser (Int, Int)
intTuple = (,) <$> digit <*> digit

bits :: Parser [Int]
bits = many (digit)