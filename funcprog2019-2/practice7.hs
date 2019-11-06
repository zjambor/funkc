module Practice7 where

import Control.Monad.State

type CounterM a = State [(a,Int)]
type CMap a = [(a,Int)]

inc :: Eq a => a -> CMap a -> CMap a
inc x [] = [(x,1)]
inc x ((y,n):ys)
   | x == y = [(y,n+1)] ++ (inc x ys)
   | otherwise = inc x ys

incM :: Eq a => a -> CounterM a ()
incM x = do
    cmap <- get 
    let cmap' = inc x cmap
    put cmap'
--    pure cmap

-- pushM x = do 
--   s <- get 
--   let s' = push x s
--   put s'

CountM :: Eq a => [a] -> CounterM a ()
CountM [] pure ()
CountM (x:xs) = do
    incM x
    countM xs

-- count :: Eq a => a -> [a] -> Maybe Int
-- count x xs = lookup x occurences where
--     occurences = execState (CountM xs) []

lengthM :: [a] -> State Int ()
lengthM x = do
    ll <- get 
    let ll' = length(x)
    put ll'

-- incM :: Eq a => a -> CounterM a ()
-- incM x = do
--     cmap <- get 
--     let cmap' = inc x cmap
--     put cmap'
