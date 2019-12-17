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

-- maxElem :: Ord a => RoseTree a -> a
-- maxElem (Branch n []) = n
-- maxElem (Branch x (a:as)) = max' as 0

-- max' :: RoseTree Int -> Int -> Int
-- max' (Branch x (a:as)) y
--     | max' (Branch y as)


-- numberElems :: RoseTree a -> RoseTree (a, Int)
-- numberElems (Branch x (a:as)) = evalState (mapMRoseTree go (Branch x (a:as))) 0
--   where go (Branch x (a:as)) = do {n <- get; put (n + 1); pure (a,n)}
--

mapMList :: Monad m => (a -> m b) -> [a] -> m [b]
mapMList f []     = pure []
mapMList f (a:as) = do
  b  <- f a
  bs <- mapMList f as
  pure $ b:bs

mapMRoseTree :: Monad m => (a -> m b) -> RoseTree a -> m (RoseTree b)
mapMRoseTree f (Branch a ts) = do
  b  <- f a
  ts <- mapMList (mapMRoseTree f) ts
  pure $ Branch b ts

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
