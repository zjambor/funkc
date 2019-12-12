{-# LANGUAGE FlexibleContexts #-}
module Feladatok02 where

-- 1. Definiáld a következő függvényeket.

f1 :: (a -> (b, c)) -> (a -> b, a -> c)
f1 = undefined

f2 :: (a -> b, a -> c) -> (a -> (b, c))
f2 = undefined

f3 :: (Either a b -> c) -> (a -> c, b -> c)
f3 = undefined

f4 :: (a -> c, b -> c) -> (Either a b -> c)
f4 = undefined

-- bónusz feladat
f5 :: (a -> a -> b) -> ((a -> b) -> a) -> b
f5 = undefined

-- 2. Definiáld a "partition :: (a -> Bool) -> [a] -> ([a], [a])" függvényt, ami
--    az első output listában visszaadja azokat az elemeket, amelyekre az "f :: a -> Bool"
--    igaz, a második output-ban pedig azokat, amire "f" hamis.

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition f xs = (filter f xs, filter (not . f) xs)

-- 3. Definiáld a "composeAll :: [a -> a] -> a -> a" függvényt. Az eredmény legyen
--    az összes bemenő függvény kompozíciója,
--    pl. "composeAll [f, g, h] x == f (g (h x))"
composeAll :: [a -> a] -> a -> a
composeAll = foldr (.) id

-- 4. Definiáld a "merge :: Ord a => [a] -> [a] -> [a]" függvényt, ami két nemcsökkenő
--    rendezett listát összefésül úgy, hogy az eredmény is rendezett maradjon.
merge :: Ord a => [a] -> [a] -> [a]
merge a [] = a
merge [] a = a
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

-- 5. (bónusz) Definiáld a "mergeSort :: Ord a => [a] -> [a]" függvényt, ami a "merge"
--     iterált felhasználásával rendez egy listát.
mergeSort :: Ord a => [a] -> [a]
mergeSort = undefined

-- 6. (bónusz) Definiáld a "sublists :: [a] -> [[a]]" függvényt, ami a bemenő lista
--    minden lehetséges részlistáját visszaadja. Pl. "sublists [1, 2] == [[],
--    [1], [2], [1, 2]]".  A részlisták sorrendje az eredményben tetszőleges, a
--    fontos, hogy az össze részlista szerepeljen.
sublists :: [a] -> [[a]]
sublists = undefined

-- 7. Vegyük a következő ADT-t:

data Tree a = Node a [Tree a]

--    Írj "Eq a => Eq (Tree a)" instance-t
--    Írj "mapTree :: (a -> b) -> Tree a -> Tree b" függvényt
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree = undefined

-- 8. Vegyük a következő ADT-t:

data Tree2 a = Leaf a | Branch (Int -> Tree2 a)

--    Írj legalább 5 darab (Tree2 a) típusú definíciót.
--    Írj "mapTree2 :: (a -> b) -> Tree2 a -> Tree2 b" függvényt.
t1 = Leaf 0
t2 = Branch $ \i -> Leaf (i + 1)
t3 = Branch $ \i -> Branch $ \j -> Leaf (i + j)
t4 = Branch $ \i -> if i < 0 then Leaf "foo" else Leaf "bar"
t5 = Branch $ \_ -> t2

mapTree2 :: (a -> b) -> Tree2 a -> Tree2 b
mapTree2 = undefined

-- 9. Definiáld a következő függvényt:
--    Működés: alkalmazzuk a kapott függvényt a lista minden elemére,
--    ha minden függvényalkalmazás eredménye Just, akkor a végeredmény
--    legyen (Just <az összes b-típusú eredmény listája>), egyébként Nothing.

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe = undefined

mapMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMaybe = undefined
