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
fy = foldr (.) id [(+2), (*7)]