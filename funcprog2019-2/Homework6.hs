module Homework6 where

import Control.Monad.Writer
import Data.Set

uniquesM :: Ord a => [a] -> Writer (Set a) Int
uniquesM [] = do
    tell (fromList [])
    return 0
uniquesM (x:xs) = do
    result <- (uniquesM xs)
    tell (fromList [x])
    return (1 + result)

reduce :: Monad m => m (m a) -> m a
reduce m = m >>= id

bind :: Monad m => m a -> (a -> m b) -> m b
x `bind` f = reduce (fmap f x)

gatherEffects :: Monad m => [m a] -> m [a]
gatherEffects [] = pure []
gatherEffects (m:ms) = do
    x <- m
    xs <- gatherEffects ms
    pure (x:xs)

monadicMap :: Monad m => (a -> m b) -> [a] -> m [b]
monadicMap f [] = return []
monadicMap f (x:xs) = do 
    y <- f x
    ys <- monadicMap f xs
    return (y:ys)

compM :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
compM f g x = do
    y <- g x
    f y

twiceM :: Monad m => m a -> m (a, a)
twiceM m = do
    x <- m 
    y <- m
    return (x,y)

times :: Monad m => Int -> m a -> m [a]
times 0 m = 
    return []
times n m = do
    result <- (times (n-1) m)
    x <- m 
    return (x : result)

voidF :: Functor f => f a -> f ()
voidF x = () <$ x
