{-# LANGUAGE FlexibleInstances #-}
module RoseTree where

import Data.Foldable (Foldable(foldMap), toList)

data RoseTree a = Node { value    :: a
                       , children :: [RoseTree a]
                       }
  deriving (Eq, Show)

--A feladat, hogy megadjunk a RoseTree adatszerkezethez egy pre-order és egy post-order bejárási algoritmust.

-- Megjegyzés: A listákban a megfelelő sorrend szerint szerepeljenek az elemek.
-- A fenti két függvényt a Foldable típusosztáy toList műveletének segítségével definiáljuk! Ehhez arra van szükség, hogy a Sum és Product típusokhoz hasonló wrapper-eket hozzunk létre a RoseTree-hez is. Ezek a wrapper-ek a következők legyenek:

newtype PreOrder  t a = PreOrder  { getPreOrder  :: t a } deriving (Eq, Show)
newtype PostOrder t a = PostOrder { getPostOrder :: t a } deriving (Eq, Show)

-- Adjuk meg a következő Foldable példányokat!
instance Foldable RoseTree where
    toList = foldr (:) []
    foldMap = undefined

instance Foldable (PreOrder RoseTree) where
    toList = foldMap (\x -> [])
    foldMap = undefined

instance Foldable (PostOrder RoseTree) where
    toList = foldMap (\x -> [])
    foldMap = undefined

preOrder  :: RoseTree a -> [a]
preOrder (Node a []) = []

postOrder :: RoseTree a -> [a]
postOrder (Node a []) = []

--A toList függvény segítségével definiáljuk a fentebb említett preOrder és postOrder függvényeket!

