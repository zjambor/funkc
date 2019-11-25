{-# LANGUAGE InstanceSigs,FlexibleContexts #-}
module Homework6 where

import Control.Monad.Writer
import Data.Set

import Control.Monad

-- ************************************************************
newtype State s a = State (s -> (a,s))

instance Functor (State s) where 
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (State g) = State $ \s -> let (x, s') = g s in (f x, s')
    -- State $ \s -> (f (fst $ g s), snd $ g s)

instance Applicative (State s) where 
  pure  = return
  (<*>) = ap

instance Monad (State s) where 
  return :: a -> State s a 
  return x = State $ \s -> (x,s) 
  
  (>>=) :: State s a -> (a -> State s b) -> State s b 
  (>>=) (State f) k = State $ \s -> let (x, s')   = f s  in 
                                    let (State g) = k x  in
                                        g s'

runState :: s -> State s a -> (a,s)
runState s (State f) = f s 

evalState :: s -> State s a -> a 
evalState s = fst . runState s 

execState :: s -> State s a -> s 
execState s = snd . runState s

get :: State s s
get = State $ \s -> (s,s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)
-- *****************************************************

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
-- times n x = sequence (replicate n x)
times 0 m = 
    return []
times n m = do
    result <- (times (n-1) m)
    x <- m 
    return (x : result)

voidF :: Functor f => f a -> f ()
voidF x = () <$ x
