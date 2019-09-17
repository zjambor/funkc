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

eqBool :: Bool -> Bool -> Bool
eqBool True  True  = True
eqBool False False = True
eqBool _     _     = False

eqList :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList eqa []     []     = True
eqList eqa (x:xs) (y:ys) = eqa x y && eqList eqa xs ys
eqList _   _      _      = False

class Eq' a where        -- osztály deklaráció
    eq :: a -> a -> Bool   -- osztály metódus  
  
instance Eq' Bool where  -- instance
    eq True  True  = True
    eq False False = True
    eq _     _     = False
    
--class EqT a where        -- osztály deklaráció
--    eqTree :: a -> a -> Bool   -- osztály metódus

instance Eq' Int where  -- instance
    eq x y  = x == y
    --eq False False = True
    --eq _     _     = False

instance Eq' a => Eq' [a] where
        eq [] [] = True
        eq (x:xs) (y:ys) = eq x y           &&    eq xs ys
        --    ^ eq "a" típusra       ^ rekurzív hívás
        eq _ _ = False

--instance Eq' (Tree a) where
instance Eq' a => Eq' (Tree a) where  -- instance   
    eq Leaf Leaf = True
    eq Leaf _ = False
    eq _ Leaf = False
    --eq _ _ = False
    eq (Node a xl xr) (Node b yl yr) = eq a b && eq xl yl && eq xr yr