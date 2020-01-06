{-# LANGUAGE FlexibleInstances, InstanceSigs #-}
module Vizsga where

import Data.Foldable (Foldable(foldMap), toList)
import Data.Traversable (Traversable(traverse))
import Control.Monad.State

data RoseTree a
  = Branch a [RoseTree a]
  deriving (Eq, Ord, Show)

instance Functor RoseTree where
    fmap :: (a -> b) -> (RoseTree a) -> (RoseTree b)
    fmap f (Branch a []) = Branch (f a) []
    fmap f (Branch a (x:xs)) = Branch (f a) (map (fmap f) xs)

instance Foldable RoseTree where
    foldMap f (Branch a as) = f a `mappend` foldMap (foldMap f) as
    toList = foldr (:) []

instance Traversable RoseTree where
    traverse f (Branch x ts) = Branch <$> f x <*> traverse (traverse f) ts
--

countElems :: RoseTree a -> Int
countElems (Branch _ []) = 1
countElems (Branch x (a:as)) = (countElems a) + (countElems (Branch x as))

maxElem :: Ord a => RoseTree a -> a
maxElem (Branch a []) = a     -- For a tree without subtrees the maximum is it's value
maxElem (Branch a subtrees) =
    maximum (a : map maxElem subtrees)   

--Definiáljuk azt a függvényt, amely megszámozza egy RoseTree elemeit! A bejárás sorrendje legyen preorder, azaz először az elemet látogassuk meg, majd balról jobbra a részfákat. (2 pont)

numberElems :: RoseTree a -> RoseTree (a, Int)
numberElems = undefined

--Segítség: Használjuk State monádot és a forM vagy mapM függvényeket!

instance Applicative RoseTree where
    pure :: a -> RoseTree a
    pure x = Branch x []

    (<*>) :: RoseTree (a -> b) -> RoseTree a -> RoseTree b
    (<*>) (Branch f tree) (Branch x subtrees) = Branch (f x) (zipWith (<*>) tree subtrees)

{- mapMRoseTree :: Monad m => (a -> m b) -> RoseTree a -> m (RoseTree b)
mapMRoseTree f (Branch a ts) = do
  b  <- f a
  ts <- mapMList (mapMRoseTree f) ts
  pure $ Branch b ts -}

ex1 :: RoseTree Int
ex1 = Branch 2 $
      [ Branch 3 $
          [ Branch 11 []
          ]
      , Branch 5 $ []
      , Branch 7 $
          [ Branch 13 []
          ]
      ]
