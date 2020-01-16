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
    -- traverse f (Branch x []) = f x 
    traverse f (Branch x ts) = Branch <$> f x <*> traverse (traverse f) ts
    -- traverse f (Branch x ts) = Branch <$> traverse (traverse f) ts <*> f x

data Tree a b = Leaf b | Node a (Tree a b) (Tree a b)
  deriving (Eq, Show)

ex1 :: Tree Int Int
ex1 = Node 1 (Leaf 10) (Node 2 (Leaf 20) (Leaf 30))

tree =
  Node 1
    (Node 2 (Node 4 (Node 7 (Leaf 20) (Leaf 20) ) (Leaf 20)) (Node 5 (Leaf 20) (Leaf 20)))
    (Node 3 (Node 6 (Node 8 (Leaf 20) (Leaf 20)) (Node 9 (Leaf 20) (Leaf 20))) (Leaf 20))

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
--
ex4 = Branch 2 []
--toList ex2
--fmap (*2) ex2
-- párok készítése: zip [1,2,3] [9,8,7] 
f x = if even x then Just (x `div` 2) else Nothing
-- fmap f ex3
--Branch (Just 1) [Branch (Just 2) [Branch (Just 6) []],Branch (Just 3) [],Branch (Just 4) [Branch (Just 7) []]]
-- traverse f ex3
-- Just (Branch 1 [Branch 2 [Branch 6 []],Branch 3 [],Branch 4 [Branch 7 []]])
--main = traverse f ex3

countElems :: RoseTree a -> Int
countElems (Branch _ []) = 1
countElems (Branch a (x:xs)) = (countElems x) + (countElems (Branch a xs))
--countElems ex2

maxElem :: Ord a => RoseTree a -> a
maxElem (Branch a []) = a
maxElem (Branch a ts) = 
    maximum (a : map maxElem ts)
--maxElem ex2

--Definiáljuk azt a függvényt, amely megszámozza egy RoseTree elemeit! 
--A bejárás sorrendje legyen preorder, azaz először az elemet látogassuk meg, majd balról jobbra a részfákat. (2 pont)

numberElems :: RoseTree a -> RoseTree (a, Int)
numberElems t = evalState (traverse go t) 0 where
  go :: a -> State Int (a, Int)
  go a = do 
      n <- get
      put (n + 1) 
      pure (a, n)

--Segítség: Használjuk State monádot és a forM vagy mapM függvényeket!

{- helper' :: Tree a b -> State Int (Tree (a, Int) (b, Int))
helper' (Leaf b) = do
    n <- get
    put (n + 1)
    pure $ Leaf (b, n)
helper' (Node a t1 t2) = do    
    n <- get
    put (n + 1)
    t1' <- helper' t1
    t2' <- helper' t2
    pure (Node (a, n) t1' t2') -}

-- bitraverse :: Applicative f => (a -> f a') -> (b -> f b') -> Tree a b -> f (Tree a' b')
-- bitraverse fa fb (Leaf b) = Leaf <$> fb b
-- bitraverse fa fb (Node a t1 t2) = Node <$> (fa a) <*> bitraverse fa fb t1 <*> bitraverse fa fb t2 

{- bitraverse :: Applicative f => (a -> f a') -> (b -> f b') -> Tree a b -> f (Tree a' b')
bitraverse f g = go where
  go (Leaf b) = Leaf <$> g b
  go (Node a l r) = Node <$> f a <*> go l <*> go r -}

numberElems' :: Tree a b -> Tree (a, Int) (b, Int)
--numberElems' t = evalState (helper' t) 0
numberElems' t = evalState (bitraverse go go t) 0 where
  go :: a -> State Int (a, Int)
  go a = do 
      n <- get
      put (n + 1) 
      pure (a, n)

-- Definiáljuk azt a függvényt, amely biztonságosan beleindexel egy listába! (1 pont)
safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _ = Nothing
safeIndex xs a
    | a < 0 = Nothing
    | a >= length xs = Nothing
    | otherwise = Just (xs !! a)

ll :: [Int]
ll = [1,2,3,4,5,6]
main = [safeIndex ll 0 == Just 1,
        safeIndex ll 7 == Nothing,
        safeIndex ll (-1) == Nothing,
        transformWithList ll (Branch 2 []) == Just (Branch 3 [])
         ]

roseList = [ex2, ex3]
-- Definiáljuk azt a függvényt, amely egy RoseTree-ben lévő indexet lecserél egy adott lista annyiadik elemére! Az indexelés nem feltétlenül helyes, ezért Maybe-vel térjünk vissza! Ha akár egyszer is invalid lenne az index, akkor Nothing-gal térjünk vissza! (2 pont)
transformWithList :: [Int] -> RoseTree Int -> Maybe (RoseTree Int)
transformWithList xs (Branch a ts)
    | safeIndex xs a == Nothing = Nothing
    | otherwise = Just (Branch (xs !! a) ts)

-- Segítség: Használjuk az előzőleg definiált safeIndex függvényt!

----------------------------------------------------------------------
-- Írjunk egy függvényt, ami az fmap-hoz hasonlóan működik, viszont mindkét típusparaméter felett map-el. (1 pont)
bimap :: (a -> a') -> (b -> b') -> Tree a b -> Tree a' b'
bimap f g = go where
    go (Leaf b) = Leaf (g b)
    go (Node a l r) = Node (f a) (go l) (go r)

-- Írjunk egy függvényt, ami a traverse-hez hasonlóan működik, viszont mindkét típusparamétert bejárja. (1 pont)
bitraverse :: Applicative f => (a -> f a') -> (b -> f b') -> Tree a b -> f (Tree a' b')
bitraverse f g = go where
    go (Leaf a) = Leaf <$> g a
    go (Node a t1 t2) = Node <$> f a <*> go t1 <*> go t2 

-- Írjunk egy függvényt, ami minden levélbe a levél fölötti Node-okban tárolt Int-ek összegét teszi. Ha nincs egy levél fölött Node, az összeg 0. (2 pont)
-- annotateSums :: Tree Int a -> Tree Int Int

{- helper :: Tree Int a -> State Int (Tree Int Int)
helper (Leaf _) = do
    n <- get
    pure $ Leaf n
helper (Node n t1 t2) = do
    m <- get
    let m' = n + m
    put m'
    t1' <- helper t1
    t2' <- helper t2
    put m
    pure $ Node n t1' t2' -}

annotateSums :: Tree Int a -> Tree Int Int
annotateSums = go 0 where
    go m (Leaf a) = Leaf m
    go m (Node a l r) = Node a (go (a + m) l) (go (a + m) r)

-- Példák a működésre:
-- annotateSums ex1 == Node 1 (Leaf 1) (Node 2 (Leaf 3) (Leaf 3))
-- annotateSums (Leaf 10) == Leaf 0

-- Írjunk egy függvényt, ami minden Node-ba a Node alatti levelekben levő Int-ek összegét teszi. (2 pont)

annotateSums' :: Tree a Int -> Tree Int Int
annotateSums' t = evalState (go t) 0 where
    go :: Tree a Int -> State Int (Tree Int Int)
    go (Leaf n) = do
        put n
        pure (Leaf n)
    go (Node n l r) = do
        l' <- go l
        n <- get
        r' <- go r
        m <- get
        put (n + m)
        pure (Node (n + m) l' r')

-- Példák a működésre:
-- annotateSums' ex1 == Node 60 (Leaf 10) (Node 50 (Leaf 20) (Leaf 30))
-- annotateSums' (Leaf 0) == Leaf 0

data BinTree a
  = Nil 
  | Branchh a (BinTree a) (BinTree a)
  deriving (Eq, Ord, Show)

ex5 :: BinTree Int
ex5 = Branchh 1 Nil (Branchh 2 Nil Nil)

instance Functor BinTree where
    fmap f Nil = Nil
    fmap f (Branchh a l r) = Branchh (f a) (fmap f l) (fmap f r)

instance Foldable BinTree where
    foldMap f Nil = mempty
    foldMap f (Branchh a t1 t2) = f a <> foldMap f t1 <> foldMap f t2 
    toList = foldMap (\x -> [x])

instance Traversable BinTree where
    traverse f Nil = pure Nil
    traverse f (Branchh a t1 t2) = Branchh <$> f a <*> traverse f t1 <*> traverse f t2 

--http://hackage.haskell.org/package/folds-0.2/src/src/Data/Fold/Internal.hs
numberNodes :: BinTree a -> BinTree (Int, a)
numberNodes t = evalState (traverse go t) 0 where
  go :: a -> State Int (Int, a)
  go a = do 
      n <- get
      put (n + 1) 
      pure (n, a)

--numberNodes ex5
