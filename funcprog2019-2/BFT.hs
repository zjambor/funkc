{-# LANGUAGE DeriveFunctor,
             GeneralizedNewtypeDeriving,
             LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}

module BFT where

import Control.Applicative
import Data.Foldable
import Data.Traversable
import Prelude hiding (foldr)
import Control.Monad.Trans.State.Lazy

data Tree a = Tree (Tree a) a (Tree a)
            | Empty
  deriving (Show, Functor)

newtype Forest a = Forest {getForest :: [Tree a]}
   deriving (Functor)

instance Foldable Forest where
  foldMap = foldMapDefault

-- Given a forest, produce the forest consisting
-- of the children of the root nodes of non-empty
-- trees.
children :: Forest a -> Forest a
children (Forest xs) = Forest $ foldr go [] xs
  where
    go Empty c = c
    go (Tree l _a r) c = l : r : c

-- Given a forest, produce a list of the root nodes
-- of the elements, with `Nothing` values in place of
-- empty trees.
parents :: Forest a -> [Maybe a]
parents (Forest xs) = foldr go [] xs
  where
    go Empty c = Nothing : c
    go (Tree _l a _r) c = Just a : c

-- Given a list of values (mixed with blanks) and
-- a list of trees, attach the values to pairs of
-- trees to build trees; turn the blanks into `Empty`
-- trees.
zipForest :: [Maybe a] -> Forest a -> [Tree a]
zipForest [] _ts = []
zipForest (Nothing : ps) ts = Empty : zipForest ps ts
zipForest (Just p : ps) (Forest ~(t1 : ~(t2 : ts'))) =
   Tree t1 p t2 : zipForest ps (Forest ts')

instance Traversable Forest where
  -- Traversing an empty container always gets you
  -- an empty one.
  traverse _f (Forest []) = pure (Forest [])

  -- First, traverse the parents. The `traverse.traverse`
  -- gets us into the `Maybe`s. Then traverse the
  -- children. Finally, zip them together, and turn the
  -- result into a `Forest`. If the `Applicative` in play
  -- is lazy enough, like lazy `State`, I believe 
  -- we avoid the double traversal Okasaki mentions as
  -- a problem for strict implementations.
  traverse f xs = (Forest .) . zipForest <$>
          (traverse.traverse) f (parents xs) <*>
          traverse f (children xs)

instance Foldable Tree where
  foldMap = foldMapDefault

instance Traversable Tree where
  traverse f t =
       (\case {(Forest [r]) -> r;
               _ -> error "Whoops!"}) <$>
       traverse f (Forest [t])

--Now we can write code to pair up each element of the tree with its breadth-first number like this:
example :: Tree Char
example = Tree (Tree Empty 'b' (Tree Empty 'c' Empty)) 'a' (Tree Empty 'd' Empty)

numberTree :: Tree a -> Tree (Int, a)
numberTree tr = flip evalState 1 $ for tr $ \x ->
      do
        v <- get
        put $! (v+1)
        return (v,x)