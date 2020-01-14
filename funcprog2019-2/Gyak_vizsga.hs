{-# LANGUAGE FlexibleInstances, InstanceSigs #-}
module Vizsga_Gyak where

import Data.Foldable (Foldable(foldMap), toList)
import Data.Traversable (Traversable(traverse))
import Control.Monad.State

data RoseTree a = Branch a [RoseTree a]
  deriving (Eq, Ord, Show)

instance Functor RoseTree where
    --fmap :: (a -> b) -> (RoseTree a) -> (RoseTree b)
    fmap f (Branch a []) = Branch (f a) []
    --fmap f (Branch a (x:xs)) = Branch (f a) (map (fmap f) xs)
    fmap f (Branch a ts) = Branch (f a) (map (fmap f) ts)
    --vagy: fmap f (Branch x ts) = Branch (f x) (fmap f <$> ts)

instance Foldable RoseTree where
    --foldMap f (Branch a []) = f a
    foldMap f (Branch a as) = f a <> foldMap (foldMap f) as     --`mappend`
    toList = foldMap (\x -> [x])

instance Traversable RoseTree where
    --traverse f (Branch x []) = f x 
    traverse f (Branch x ts) = Branch <$> f x <*> traverse (traverse f) ts

data Tree a b = Leaf b | Node a (Tree a b) (Tree a b)
  deriving (Eq, Show)

ex1 :: Tree Int Int
ex1 = Node 1 (Leaf 10) (Node 2 (Leaf 20) (Leaf 30))

instance Functor (Tree a) where
    fmap f (Leaf b) = Leaf (f b)
    fmap f (Node a t1 t2) = Node a (fmap f t1) (fmap f t2)

instance Foldable (Tree a) where
    foldMap f (Leaf b) = f b
    foldMap f (Node a t1 t2) = foldMap f t1 <> foldMap f t2 

instance Traversable (Tree a) where
    traverse f (Leaf a) = Leaf <$> f a
    traverse f (Node a t1 t2) = Node <$> pure a <*> traverse f t1 <*> traverse f t2 
--
ex2 :: RoseTree Int
ex2 = Branch 2 $
      [ Branch 3 $
          [ Branch 11 []
          ]
      , Branch 5 $ []
      , Branch 7 $
          [ Branch 13 []
          ]
      ]
--
--toList ex2
--fmap (*2) ex2
