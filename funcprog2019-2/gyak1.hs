module Gyak1 where

import Prelude hiding (sum)

data BinTree = Leaf | Nodex BinTree BinTree
   deriving Show
numLeaves :: BinTree -> Int
numLeaves Leaf       = 1
numLeaves (Nodex l r) = numLeaves l + numLeaves r

-- 1. Definiáld újra a lista típust ADT-ként, "List" néven. Írj egy
--    "mapList :: (a -> b) -> List a -> List b", ami a lista minden elemére
--    egy függvényt alkalmaz.

data Listx a = Nil | Cons a (Listx a)
   deriving Show
mapList :: (a -> b) -> Listx a -> Listx b
mapList f Nil = Nil
mapList f (Cons a xs) = Cons (f a) (mapList f xs)
--ll=Cons 99 (Cons 5 (Cons 10 Nil))
--mapList times2 ll

l1 :: Listx Int
l1 = Cons 0 Nil

sum' :: Listx Int -> Int
sum' Nil = 0
sum' (Cons a xs) = a + (sum' xs)

sum :: Listx Int -> Int
sum Nil = 0
sum (Cons a xs) = 1 + (sum xs)

sum1 :: [Int] -> Int
sum1 [] = 0
sum1 (x:xs) = x + sum1 xs

times2 :: Int -> Int
times2 x = x * 2

-- 3. Definiálj egy "Tree a" típust, aminek annotáció nélküli levelei és "a"-val annotált bináris
--    elágazásai vannak. Írj egy "mapTree :: (a -> b) -> Tree a -> Tree b" függvényt, ami az elágazásokban
--    levő "a" értékekre egy függvényt alkalmaz.

data Tree a = TLeaf | Node a (Tree a) (Tree a)
   deriving Show
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f TLeaf = TLeaf
mapTree f (Node a l r) = Node (f a) (mapTree f l) (mapTree f r)
--n=Node 4 (Node 5 TLeaf TLeaf) (Node 5 TLeaf TLeaf)
--mapTree times2 n
