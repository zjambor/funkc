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
    foldMap f (Node a as) = f a `mappend` foldMap (foldMap f) as

instance Foldable (PreOrder RoseTree) where
    toList = foldMap (\x -> [x])
    foldMap f (PreOrder (Node a as)) = f a <> foldMap (foldMap f . PreOrder) as

instance Foldable (PostOrder RoseTree) where
    toList = foldMap (\x -> [x])
    foldMap f (PostOrder (Node a as)) = foldMap (foldMap f . PostOrder) as <> f a

preOrder  :: RoseTree a -> [a]
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
