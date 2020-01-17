{-# LANGUAGE FlexibleInstances, InstanceSigs #-}
module Vizsga_Gyak3 where

import Data.Foldable (Foldable(foldMap), toList)
import Data.Traversable (Traversable(traverse))
import Control.Monad.State

data Either' a b = Left' a | Right' b | Both a b
  deriving (Eq, Show)

--Írjunk Functor példányt az Either' típushoz! (1 pont)
instance Functor (Either' a) where
    fmap f (Left' a) = Left' a
    fmap f (Right' b) = Right' (f b)
    fmap f (Both a b) = Both a (f b)

--Írjunk Foldable példányt az Either' típushoz! (1 pont)
instance Foldable (Either' a) where
    foldMap _ (Left' x) = mempty
    foldMap f (Right' b) = f b
    foldMap f (Both a b) = f b

----Írjunk Traversable példányt az Either' típushoz! (1 pont)
instance Traversable (Either' a) where
    traverse _ (Left' a) = pure (Left' a)
    traverse f (Right' b) = Right' <$> f b
    traverse f (Both a b) = Both a <$> f b

--Írjunk egy függvényt, ami az Either' konstruktorai szerint három listára bont egy listát. (1 pont)
partition :: [Either' a b] -> ([a], [b], [(a, b)])
partition [] = ([],[],[])
partition (x:xs) = partition' (x:xs) ([],[],[]) where 
    partition' :: [Either' a b] -> ([a], [b], [(a, b)]) -> ([a], [b], [(a, b)])
    partition' [] (as,bs,cs) = (reverse as, reverse bs, reverse cs)
    partition' (x:xs) (as,bs,cs) = case x of
        Left' a -> partition' xs ((a:as),bs,cs)
        Right' b -> partition' xs (as,(b:bs),cs)
        Both a b -> partition' xs (as,bs,((a, b):cs))

-- Példa a működésre:
-- partition [Left' True, Right' (0::Int), Both False 10,Both False 1] == ([True],[0],[(False,10),(False,1)])
-- partition [] == ([], [], [])

-- Írjunk egy függvényt, ami két listát kombinál elemenként egy függvénnyel. Az eredmény lista hossza az hosszabb input lista hossza legyen. 
-- Ha elfogyik valamelyik input lista, akkor a Left' vagy Right' konstruktorral kell feldolgozni a fennmaradó listát. (2 pont)
zipWith' :: (Either' a b -> c) -> [a] -> [b] -> [c]
zipWith' f []       [] =     []
zipWith' f (a:as)   [] =     f (Left' a)  : zipWith' f as []
zipWith' f []       (b:bs) = f (Right' b) : zipWith' f [] bs
zipWith' f (a:as)   (b:bs) = f (Both a b) : zipWith' f as bs

-- Példák:
-- zipWith' (\x -> case x of Left' x -> x; Right' y -> 0; Both x y -> x + y) [1, 2, 3] [] == [1,2,3]
-- zipWith' (\x -> case x of Left' x -> x; Right' y -> 0; Both x y -> x + y) [1, 2, 3] [10, 20, 30] == [11,22,33]
-- zipWith' (\x -> case x of Left' x -> x; Right' y -> 0; Both x y -> x + y) [1] [10, 20, 30] == [11,0,0]

-- Alkalmazzunk egy a -> Maybe b függvényt minden a típusú értéken az inputban. 
-- Ha bármelyik függvényhívás eredménye Nothing, legyen a végeredmény Nothing, egyébként a végeredmény Just-ban az output lista, ahol a függvényt minden a-ra alkalmaztuk. 
-- Tipp: használjunk Maybe monádot. (2 pont)

ff :: Int -> Maybe Int
ff x = if x == 0 then Nothing else Just x

mapMaybeLeft :: (a -> Maybe b) -> [Either' a c] -> Maybe [Either' b c]
mapMaybeLeft f ts = traverse go ts where
    go (Left' a) = Left' <$> f a
    go (Right' b) = pure (Right' b)
    go (Both a b) = flip Both b <$> f a

-- Példák:
-- mapMaybeLeft (\_ -> Just True) [Left' ()] == Just [Left' True]
-- mapMaybeLeft (\x -> if x == 0 then Nothing else Just x) [Right' 10, Both 0 10] == Nothing
-- mapMaybeLeft (\x -> if x == 0 then Nothing else Just x) [Right' 10, Both 1 10] == Just [Right' 10,Both 1 10]

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Eq, Show)

-- Írjunk Functor, Foldable és Traversable instance-ot Tree-hez. (2 pont)
instance Functor Tree where
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node l r) = Node (fmap f l) (fmap f r)

instance Foldable Tree where
    foldMap f (Leaf a) = f a
    foldMap f (Node l r) = foldMap f l <> foldMap f r

instance Traversable Tree where
    traverse f (Leaf a) = Leaf <$> f a
    traverse f (Node l r) = Node <$> traverse f l <*> traverse f r

-- Irjunk egy függvényt, ami minden levélben az attól balra levő levelekben levő Int-ek összegét adja vissza. Tipp: használjunk State monádot. (2 pont)
treeSums :: Tree Int -> Tree Int
treeSums t = evalState (traverse (\n ->
    do
        m <- get
        put(n + m)
        pure(m))
    t) 0 

-- Példák:
-- treeSums (Leaf 10) == Leaf 0
-- treeSums (Node (Leaf 10) (Leaf 10)) == Node (Leaf 0) (Leaf 10)
-- treeSums (Node (Leaf 10) (Node (Leaf 10) (Leaf 10))) == Node (Leaf 0) (Node (Leaf 10) (Leaf 20))
-- treeSums (Node (Node (Leaf 1) (Leaf 100)) (Leaf 0)) == Node (Node (Leaf 0) (Leaf 1)) (Leaf 101)

-- utánanézni: traverse, <>, <$>

