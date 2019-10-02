{-# LANGUAGE FlexibleInstances, KindSignatures, InstanceSigs #-}
module Gyak5 where

import Prelude hiding(Functor(..),Either(..))

data List a = Nil | Cons a (List a)
    deriving (Eq,Ord,Show)

class Functor f where               --(f :: * -> *)
    fmap :: (a -> b) -> f a -> f b    

instance Functor List where
    fmap :: (a -> b) -> List a -> List b
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)
--

-- map :: (a -> b) -> [a] -> [b]  

instance Functor [] where
    fmap :: (a -> b) -> [a] -> [b]
    fmap f [] = []
    fmap f (x:xs) = f x : fmap f xs

fxy = fmap (*2) [1..3]  

instance Functor Maybe where  
    fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap f (Just x) = Just (f x)  
    fmap f Nothing = Nothing  

data Tree1 a = Leaf1 a | Node1 (Tree1 a) (Tree1 a)
data Tree2 a = Node2 a [Tree2 a]
data Tree3 i a = Leaf3 a | Node3 (i -> Tree3 i a)
data Pair a b = Pair a b
data Either a b = Left a | Right b
newtype Id a = Id a
newtype Const a b = Const a
newtype Fun a b = Fun (a -> b)

instance Functor Tree1 where
    fmap :: (a -> b) -> Tree1 a -> Tree1 b
    fmap f (Leaf1 a) = Leaf1 (f a)
    fmap f (Node1 l r) = Node1 (fmap f l) (fmap f r)
--
instance Functor Tree2 where
    fmap :: (a -> b) -> Tree2 a -> Tree2 b
    fmap f (Node2 a []) = Node2 (f a) []
    fmap f (Node2 a xs) = Node2 (f a) (map (fmap f) xs)
--
instance Functor (Tree3 i) where
    fmap :: (a -> b) -> Tree3 i a -> Tree3 i b
    fmap f (Leaf3 a) = Leaf3 (f a)
    fmap f (Node3 g) = Node3 (\i -> fmap f (g i))
--

instance Functor (Pair a) where
    fmap f (Pair a b) = Pair a (f b)
  
instance Functor (Either a) where
    fmap f (Left a) = Left a
    fmap f (Right b) = Right (f b)

instance Functor Id where
    fmap f (Id a) = Id (f a)

instance Functor (Const a) where
    fmap f (Const a) = Const a

instance Functor (Fun x) where 
    fmap :: (a -> b) -> (Fun x a) -> (Fun x b)
    fmap f (Fun g) = Fun (f . g)

--
newtype Cont r a = Cont ((a -> r) -> r)

instance Functor (Cont r) where
    fmap f (Cont g) = Cont (\k -> g (k . f))
  