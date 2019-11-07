module Practice7 where

import Control.Monad.State

type CounterM a = State [(a,Int)]
--type CounterM a = State (CMap a)
type CMap a = [(a,Int)]

-- inc :: Eq a => a -> CMap a -> CMap a
-- inc x [] = [(x,1)]
-- inc x ((y,n):ys)
--    | x == y = [(y,n+1)] ++ (inc x ys)
--    | otherwise = inc x ys

inc :: Eq a => a -> CMap a -> CMap a 
inc x [] = [(x,1)]
inc x ((y,n):ys)
  | x == y    = (y, n+1) : ys 
  | otherwise = (y, n)   : inc x ys

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

countM :: Eq a => [a] -> CounterM a ()
countM [] = pure ()
countM (x:xs) = do
    incM x
    countM xs

-- Use countM, lookup :: Eq a => a -> [(a,b)] -> Maybe b
count :: Eq a => a -> [a] -> Maybe Int 
count x xs = lookup x occurences where 
  occurences = execState (countM xs) []

lengthM :: [a] -> State Int ()
lengthM [] = pure ()
lengthM (x:xs) = do
    modify (+1)
    lengthM xs

incTests :: [Bool]
incTests = 
  [ inc 'a' [] == [('a',1)]
  , inc 0 [(2,0), (3,2)] == [(2,0), (3,2), (0,1)]
  , inc True [(True,4), (False,3)] == [(True,5), (False,3)]
  ]