{-# LANGUAGE FlexibleInstances, InstanceSigs, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Vizsga_Gyak4 where

--import Data.Foldable (Foldable(foldMap), toList)
import Data.Traversable (Traversable(traverse))
--import Control.Monad.State
import Data.Foldable
import Data.Traversable
import Prelude hiding (foldr)
import Control.Monad.Trans.State.Lazy

data RoseTree a = Branch a [RoseTree a]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

postIncrement :: Enum s => State s s
postIncrement = do
    result <- get
    modify succ
    return result

mapTreeM :: Monad m => (a -> m b) -> RoseTree a -> m (RoseTree b)
mapTreeM f (Branch a subtrees) = do
    a' <- f a
    subtrees' <- mapM (mapTreeM f) subtrees
    return $ Branch a' subtrees'

numberElems :: RoseTree a -> State Int (RoseTree (a, Int))
numberElems = mapTreeM go where 
        go :: a -> State Int (a, Int)
        go a = do 
            counter <- postIncrement
            return (a, counter)

numberTree :: RoseTree a -> RoseTree (a, Int)
numberTree t = evalState (numberElems t) 0

{- instance Traversable RoseTree where
      traverse f (Branch a subtrees) = Branch <$> f a <*> traverse f subtrees -}

tag :: Traversable t => t a -> t (a, Int)
tag t = evalState (traverse go t) 0 where 
        go :: a -> State Int (a, Int)
        go a = do 
            tag <- postIncrement
            return (a, tag)

ex3 :: RoseTree Int
ex3 = Branch 2 $
      [ Branch 4 $
          [ Branch 12 []
          ]
      , Branch 6 $ []
      , Branch 8 $
          [ Branch 14 []
          ]
      ]

data BinTree a = Nil | Node a (BinTree a) (BinTree a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

ex5 :: BinTree Int
ex5 = Node 3 (Node 5 (Node 1 Nil Nil) (Node 1 Nil Nil)) (Node 6 (Node 7 Nil Nil) (Node 4 Nil Nil))

main = [
    numberTree ex3 == Branch (2,0) [Branch (4,1) [Branch (12,2) []],Branch (6,3) [],Branch (8,4) [Branch (14,5) []]],
    tag ex3 == Branch (2,0) [Branch (4,1) [Branch (12,2) []],Branch (6,3) [],Branch (8,4) [Branch (14,5) []]],
    tag ex5 == Node (3,0) (Node (5,1) (Node (1,2) Nil Nil) (Node (1,3) Nil Nil)) (Node (6,4) (Node (7,5) Nil Nil) (Node (4,6) Nil Nil))
    ]