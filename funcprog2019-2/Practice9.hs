{-# LANGUAGE KindSignatures,DeriveFunctor,InstanceSigs #-}
module Practice9 where

import Data.Set
import Control.Monad.Writer

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