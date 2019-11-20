{-# LANGUAGE KindSignatures,DeriveFunctor,InstanceSigs #-}
module Practice8 where

import Data.Set
import Control.Monad.Writer
{- import Prelude hiding(Applicative(..))
-- http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Applicative.html
class Functor f -> Applicative (f :: * -> *) where
    pure :: a -> f Applicative
    (<*>) :: f (a -> b) -> f a -> f b -}
--

{- Prelude> pure (+) <*> (pure 2) <*> (pure 5)
7
Prelude> pure (+) <*> (Just 2) <*> (Just 5)
Just 7
fmap (+1) (Just 5) == (+1) <$> (Just 5) -}

data Either2 a b = L a | R b
    deriving (Show,Ord, Eq, Functor)
    --
--
-- Left "fail" >>= \_ -> return 5
-- Left "fail" >> return 5
-- Left "fail" *> Left "fail2"

instance Monoid a => Applicative (Either2 a) where
    pure :: b -> Either2 a b
    pure x = R x
    (<*>) :: Either2 a (b -> c) -> Either2 a b -> Either2 a c
    (<*>) (R f) (R x) = R (f x) 
    (<*>) (L e) (R x) = L e
    (<*>) (R f) (L e) = L e
    (<*>) (L e1) (L e2) = L (e1 <> e2)

f1 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
f1 f m1 m2 = do
    x <- m1
    y <- m2
    pure (f x y)

f1' :: Applicative m => (a -> b -> c) -> m a -> m b -> m c
f1' f m1 m2 = f <$> m1 <*> m2

--ZipList [(+1), (*2), ()] <*> 

--sequenceMaybes [Just 5, Just 1, Just 2] == Just [5,1,2]
--sequenceMaybes [Just 5, Nothing, Just 2] == Nothing
sequenceMaybes :: [Maybe a] -> Maybe [a]
sequenceMaybes [] = Just []
sequenceMaybes (Nothing:_) = Nothing
sequenceMaybes ((Just x):xs) = case sequenceMaybes xs of
    Nothing -> Nothing
    Just ys -> Just (x:ys)

--twiceM :: Monad m => m a
fv = [sequenceMaybes [Just 5, Nothing] == Nothing,
     sequenceMaybes [Just 5, Just 1] == Just [5,1]]

twiceM :: Monad m => m a -> m (a, a)
twiceM m = do
    x <- m 
    y <- m
    return (x,y)

gatherEffect :: Monad m => [m a] -> m [a]
gatherEffect [] = pure []
gatherEffect (m:ms) = do
    x <- m
    xs <- gatherEffect ms
    pure (x:xs)
  
compM :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
compM f g x = do
    y <- g x
    f y

--
h :: Double -> Maybe Double
h 0 = Nothing
h n = Just (10 / n)

{- type SolverM = Writer (DList Move)

moveM :: RodID -> RodID -> Problem -> SolverM Problem
moveM a b p = do
    tell (toDList [(a,b)])
    return (move a b p) -}

--
uniquesM :: Ord a => [a] -> Writer (Set a) Int
uniquesM [] = do
    tell (fromList [])
    return 0
uniquesM (x:xs) = do
    tell (fromList [x])
    return (1 + (uniquesM xs))