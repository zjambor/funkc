module Gyak2 where

import Prelude hiding (sum)

data Tree a = Leaf | Node a (Tree a) (Tree a)
   deriving Show
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Leaf = Leaf
mapTree f (Node a l r) = Node (f a) (mapTree f l) (mapTree f r)
--n=Node 4 (Node 5 Leaf Leaf) (Node 5 Leaf Leaf)
--mapTree times2 n

t1 = Leaf
t2 = Node 2 Leaf Leaf
t3 = Node 3 t2 t2
t4 = Node 4 t3 t3

instance Eq a => Eq (Tree a) where
    (==) Leaf Leaf = True
    (==) Leaf _ = False
    (==) (Node a xl xr) (Node b yl yr) = (a == b) && xl == yl && xr == yr
    (==) _ _ = False
--(Node x xl xr) == (Node y yl yr) = (x == y) && xl == yl && xr == yr

doubleSmallNumber x = if x > 100  
    then x  
    else x*2   

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1  

d1 = doubleSmallNumber 105
d2 = doubleSmallNumber 95
d3 = doubleSmallNumber' 95

--removeNonUppercase :: [Char] -> [Char]  
--removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']] 
removeNonUppercase st = [ c | c <- st, c == 'm']

