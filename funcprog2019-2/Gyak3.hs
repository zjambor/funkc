module Gyak3 where

import Prelude hiding (sum,and)

-- 2. Definiáld a "partition :: (a -> Bool) -> [a] -> ([a], [a])" függvényt, ami
--    az első output listában visszaadja azokat az elemeket, amelyekre az "f :: a -> Bool"
--    igaz, a második output-ban pedig azokat, amire "f" hamis.

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition f xs = (filter f xs, filter (not . f) xs)
--partition even [1,2,3,4,5,6]

f1 :: (a -> (b, c)) -> (a -> b, a -> c)
f1 f = (fst . f, snd . f)

f3 :: (Either a b -> c) -> (a -> c, b -> c)
f3 f = (f . Left, f . Right)

f4 :: (a -> c, b -> c) -> (Either a b -> c)
f4 (f, g) (Left a) = f a
f4 (f, g) (Right b) = g b

ff=f4 (even, odd) (Left 2)
ff2=f4 (even, odd) (Left 3)
ff3=f4 (even, odd) (Left 4)
ff4=[ff,ff2,ff3]

f5 :: (Int -> Bool) -> [Int] -> [Bool]
f5 f [] = []
f5 f (x : xs) = (f x) : (f5 f xs)

fff = f5 even [1,2,3,4,5,6,7,8,9,10]

f6 :: (a -> c, b -> c) -> (Either [a] [b] -> ([c]))
f6 (f, g) (Left []) = []
f6 (f, g) (Right []) = []
f6 (f, g) (Left (x : xs)) = (f x) : (f6 (f, g) (Left xs))
f6 (f, g) (Right (x : xs)) = (g x) : (f6 (f, g) (Right xs))

ffg = f6 (even, odd) (Left [1,2,3,4,5,6,7,8,9,10])
ffg2 = f6 (even, odd) (Right [1,2,3,4,5,6,7,8,9,10])

-- foldr
sum xs = foldr (+) 0 xs
szumma = sum [1,5,8]
and xs = foldr (&&) True xs
--main = and ffg

-- 3. Definiáld a "composeAll :: [a -> a] -> a -> a" függvényt. Az eredmény legyen
--    az összes bemenő függvény kompozíciója,
--    pl. "composeAll [f, g, h] x == f (g (h x))"
composeAll :: [a -> a] -> a -> a
composeAll = foldr (.) id
--composeAll [f, g, h] x = f (g (h x))

paratlan = composeAll [(`mod` 2),(`mod` 3)] 6 == 1
--composeAll [(`mod` 2),(`mod` 3)] 6
-- composeAll [(`mod` 5),(`mod` 6),(+ 4),(* 2)] 11
fy x = foldr (.) id [(+2), (*7)] x

-- 4. Definiáld a "merge :: Ord a => [a] -> [a] -> [a]" függvényt, ami két nemcsökkenő
--    rendezett listát összefésül úgy, hogy az eredmény is rendezett maradjon.
merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys) 
    | x < y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys
merge xs ys = xs ++ ys

li = [1,2,4,5,8,9] 
li2 = [3,4,6,7,8,9]
rend = merge li li2

-- 5. (bónusz) Definiáld a "mergeSort :: Ord a => [a] -> [a]" függvényt, ami a "merge"
--     iterált felhasználásával rendez egy listát.
mergeSort :: Ord a => [a] -> [a]
mergeSort = mergeAll . map (\x -> [x]) where
    mergePairs (xs:ys:yss) = merge xs ys : mergePairs yss
    mergePairs yss         = yss
  
    mergeAll [as] = as
    mergeAll as   = mergeAll (mergePairs as)
----
mergePairs (xs:ys:yss) = merge xs ys : mergePairs yss
mergePairs yss         = yss
mli = mergePairs [li, li2, rend, li]

mergeAll [as] = as
mergeAll as   = mergeAll (mergePairs as)

mli2 = mergeAll [li, li2, rend, li]

-- 7. Vegyük a következő ADT-t:

data Tree a = Node a [Tree a]
    deriving Show
--   Írj "Eq a => Eq (Tree a)" instance-t
--   Írj "mapTree :: (a -> b) -> Tree a -> Tree b" függvényt
instance Eq a => Eq (Tree a) where
    (==) (Node a []) (Node b []) = a == b
    (==) (Node a _) (Node b []) = False
    (==) (Node a []) (Node b _) = False
    (==) (Node a as) (Node b bs) = a == b && as == bs

--
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Node n []) = Node (f n) []
mapTree f (Node n as) = Node (f n) (map (mapTree f) as)

tr1 = Node 1 [Node 5 []]
tr2 = Node 5 [Node 1 []]
tr3 = mapTree (* 2) tr1
tr4 = mapTree (/ 2) tr3

main = [ mli2 == mergeAll mli,
         tr1 == tr2,
         tr2 == tr3,
         tr4 == tr1 ]

-- 8. Vegyük a következő ADT-t:

data Tree2 a = Leaf a | Branch (Int -> Tree2 a)

--    Írj legalább 5 darab (Tree2 a) típusú definíciót.
--    Írj "mapTree2 :: (a -> b) -> Tree2 a -> Tree2 b" függvényt.

t1 = Leaf 2
t2 = Branch (\x -> Leaf (x + 1))
t3 = Branch (\i -> Branch (\j -> Leaf (i + j)))
t4 = Branch (\i -> if i > 0 then Leaf "a" else Leaf "b")
t5 = Branch $ \_ -> t2

mapTree2 :: (a -> b) -> Tree2 a -> Tree2 b
mapTree2 f (Leaf n) = Leaf (f n)
mapTree2 f (Branch g) = Branch (\i -> mapTree2 f (g i))

-- 9. Definiáld a következő függvényt:
--    Működés: alkalmazzuk a kapott függvényt a lista minden elemére,
--    ha minden függvényalkalmazás eredménye Just, akkor a végeredmény
--    legyen (Just <az összes b-típusú eredmény listája>), egyébként Nothing.

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing  _ = Nothing
bindMaybe (Just a) f = f a

mapMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMaybe f []     = Just []
mapMaybe f (a:as) =
  bindMaybe (f a) $ \a ->
  bindMaybe (mapMaybe f as) $ \as ->
  Just (a : as)
