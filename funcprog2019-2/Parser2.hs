{-# LANGUAGE KindSignatures,DeriveFunctor,InstanceSigs #-}
module Parser2 where

import Control.Monad.Writer
import Control.Applicative
import Control.Monad.Trans.State
import Control.Monad.Trans

--type Parser a = StateT String (MaybeT Identity) a
type Parser a = StateT String Maybe a

runParser :: Parser a -> String -> Maybe (a, String)
runParser = runStateT
-- runParser p str = runStateT

-- eof :: Parser ()
-- eof = P $ \str -> case str of
--     "" -> Just ((), str)
--     _ -> Nothing
--
eof :: Parser ()
eof = do
    str <- get
    case str of
        "" -> do
            put ""
            pure () --lift $ Just (), lift $ pure ()
        _ -> lift $ Nothing


char :: Char -> Parser Char
char c = do
    str <- get
    case str of
        (x:xs) | x == c -> do
            put xs
            pure c
        _ -> lift $ Nothing
--

-- char :: Char -> Parser Char
-- char c = P $ \str -> case str of
--     (x:xs) | x == c -> Just (c, xs)
--     _               -> Nothing

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

shortByte :: Parser ShortByte
shortByte = SB <$> bit <*> bit <*> bit <*> bit

-- runParser shortByte "0101"


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

intTuple :: Parser (Int, Int)
intTuple = (,) <$> digit <*> digit

bits :: Parser [Int]
bits = many (digit)

natural :: Parser Int
natural = foldl (\acc cur -> acc*10 + cur) 0 <$> some digit

foo :: Parser a -> Parser [a]
foo p = do
    n <- int
    times n p
