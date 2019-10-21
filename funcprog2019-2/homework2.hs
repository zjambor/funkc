{-# LANGUAGE FlexibleInstances #-}
module Homework2 where

import Data.Foldable (Foldable(foldMap), toList)

data RoseTree a = Node { value    :: a
                       , children :: [RoseTree a]
                       }
  deriving (Eq, Show)

--A fenti két függvényt a Foldable típusosztáy toList műveletének segítségével definiáljuk! Ehhez arra van szükség, hogy a Sum és Product típusokhoz hasonló wrapper-eket hozzunk létre a RoseTree-hez is. 
--Ezek a wrapper-ek a következők legyenek:
newtype PreOrder  t a = PreOrder  { getPreOrder  :: t a } deriving (Eq, Show)
newtype PostOrder t a = PostOrder { getPostOrder :: t a } deriving (Eq, Show)

instance Foldable RoseTree where
    foldMap f (Node x ts) = f x `mappend` foldMap (foldMap f) ts

instance Foldable (PostOrder RoseTree) where
    toList (PostOrder (Node a children)) = concat (map toList children ++ [a])
    foldMap f (PostOrder (Node x ts)) = f x `mappend` foldMap (foldMap f) ts
    --foldMap f (PostOrder (Node x ts)) = f x `mappend` foldMap (foldMap f) ts
--concat (map postorder ts)
--Adjuk meg a következő Foldable példányokat!
instance Foldable (PreOrder RoseTree) where
    toList (PreOrder (Node a children)) = a : concatMap toList children
    foldMap f (PreOrder (Node x ts)) = f x `mappend` foldMap (foldMap f) ts



preOrder :: RoseTree a -> [a]
preOrder n = toList (PreOrder n)

postOrder :: RoseTree a -> [a]
postOrder n = toList (PostOrder n)

--A toList függvény segítségével definiáljuk a fentebb említett preOrder és postOrder függvényeket!

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