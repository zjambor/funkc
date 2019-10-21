{-# LANGUAGE FlexibleInstances #-}
module Homework2 where

import Data.Foldable (Foldable(foldMap), toList)
--import Data.Traversable (Traversable(traverse))

data RoseTree a = Node { value    :: a
                       , children :: [RoseTree a]
                       }
  deriving (Eq, Show)

instance Functor RoseTree where
  --fmap :: (a -> b) -> (RoseTree a -> RoseTree b)
  fmap f (Node x xs) = Node (f x) (map (fmap f) xs) 

--A fenti két függvényt a Foldable típusosztáy toList műveletének segítségével definiáljuk! Ehhez arra van szükség, hogy a Sum és Product típusokhoz hasonló wrapper-eket hozzunk létre a RoseTree-hez is. 
--Ezek a wrapper-ek a következők legyenek:
newtype PreOrder  t a = PreOrder  { getPreOrder  :: t a } deriving (Eq, Show)
newtype PostOrder t a = PostOrder { getPostOrder :: t a } deriving (Eq, Show)

--Adjuk meg a következő Foldable példányokat!
instance Foldable (PreOrder RoseTree) where
    toList (PreOrder (Node a children)) = a : concatMap toList children

instance Foldable (PostOrder RoseTree)

preOrder :: RoseTree a -> [a]
preOrder n = toList (PreOrder n)
--(Node a children)

postOrder :: RoseTree a -> [a]
postOrder (Node a children) = concatMap postOrder children ++ [a]

--A toList függvény segítségével definiáljuk a fentebb említett preOrder és postOrder függvényeket!
{-
instance Foldable (PreOrder  RoseTree) where
    foldMap f = fold . map' f
  
    fold Nil = mempty
    fold (Cons x xs) = x <> fold xs
    
  
instance Foldable (PostOrder RoseTree)
  foldMap f = fold . map' f

  fold Nil = mempty
  fold (Cons x xs) = x <> fold xs

preOrder :: RoseTree a -> [a]
preOrder (Node v []) = [v]
preOrder (Node v c) = [v] ++ (foldl (++) (map preOrder c) [])

-- postOrder :: RoseTree a -> [a]
-}

treeFold :: (b -> [b] -> b) -> (a -> b) -> RoseTree a -> b
treeFold f g tree = f (g (value tree)) (map (treeFold f g) (children tree))

instance Foldable RoseTree where
    -- foldMap :: (Monoid m) => (a -> m) -> Tree a -> m
    foldMap f = treeFold (\x xs -> x <> mconcat xs) f


haskellTree :: RoseTree String
haskellTree = Node "Haskell"
              [ Node "is"
                [ Node "a" []
                , Node "purely" []
                ]
              , Node "functional"
                [ Node "programming" []
                , Node "language" []
                ]
              ]

main = [ "Haskell is a purely functional programming language" == unwords (preOrder haskellTree),
       "a purely is programming language functional Haskell" == unwords (postOrder haskellTree) ]