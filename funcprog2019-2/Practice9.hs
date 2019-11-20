{-# LANGUAGE KindSignatures,DeriveFunctor,InstanceSigs #-}
module Practice9 where

import Control.Monad.Writer
import Control.Applicative

newtype Parser a = P { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (P g) = P $ \str -> case g str of
        Just (x, str') -> Just (f x, str')
        Nothing -> Nothing
    -- fmap f (P g) = P $ \str -> do
    --     (x, str') <- g str
    --     pure (f x, str')

instance Applicative Parser where
    pure :: a -> Parser a
    pure a = P $ \str -> Just (a,str)
  
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (<*>) (P pF) (P g) = P $ \str -> do
        (f, str') <- pF str
        (x, str'') <- g str'
        pure (f x, str'')
--

eof :: Parser ()
eof = P $ \str -> case str of
    "" -> Just ((), str)
    _ -> Nothing

char :: Char -> Parser Char
char c = P $ \str -> case str of
    (x:xs) | x == c -> Just (c, xs)
    _               -> Nothing

--
instance Alternative Parser where
    empty :: Parser a
    empty = P $ \_ -> Nothing

    (<|>) :: Parser a -> Parser a -> Parser a
    -- (<|>) (P pF) (P g) = P $ \str -> case pF str of
    --     Nothing -> g str
    --     _       -> pF str
    (<|>) p g = P $ \str -> case runParser p str of
        Just res -> Just res
        Nothing -> runParser g str
--
charXY :: Parser Char
charXY = char 'x' <|> char 'y'

anyChar :: Parser Char
anyChar = foldr (<|>) empty $ map char ['a'..'z']
-- char 'a' <|> char 'b' char <|> 'c'

-- foo = traverse char ['a'..'z']
-- runParser foo ['a'..'z']
-- Just ("abcdefghijklmnopqrstuvwxyz","")

string :: String -> Parser [Char]
string = traverse char

as :: Parser [Char]
as = many (char 'a')

some' :: Alternative f => f a -> f [a]
some' f = (:) <$> f <*> many' f
-- (:) - const

many' :: Alternative f => f a -> f [a]
many' f = some' f <|> pure []

data Bit = T | F
    deriving (Eq, Ord, Show)

data ShortByte = SB Bit Bit Bit Bit
    deriving (Eq, Ord, Show)

bitF :: Parser Bit
bitF = char '0' *> pure F
-- eldobja és odarakja a bitnek az eredményét

bitT :: Parser Bit
bitT = char '1' *> pure T

bit :: Parser Bit
bit = bitF <|> bitT

byte :: Parser ShortByte
byte = SB <$> bit <*> bit <*> bit <*> bit

-- runParser shortByte "0101"

instance Monad Parser where
    return :: a -> Parser a
    return = pure

    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (>>=) p k = P $ \str -> case runParser p str of
        Nothing         -> Nothing
        Just (x, str')  -> runParser (k x) str'
--

times :: Int -> Parser a -> Parser [a]
times n p = traverse (\_ -> p) [1..n]
-- runParser (times 3 (char 'x')) "xxx"
-- Just ("xxx","")

digit :: Parser Int
digit = fmap (\n -> n - 48)
        . fmap fromEnum
        . foldr (<|>) empty
        $ map char ['0'..'9'] -- kell egy fv ami 0-9 karaktereket átalakítja számokká

int :: Parser Int
int = undefined

foo :: Parser a -> Parser [a]
foo p = do
    n <- int
    times n p