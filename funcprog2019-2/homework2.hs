{-# LANGUAGE FlexibleInstances #-}
module Homework4 where

import Data.Foldable (Foldable(foldMap), toList)

data RoseTree a = Node { value    :: a
                       , children :: [RoseTree a]
                       }
  deriving (Eq, Show)

newtype PreOrder  t a = PreOrder  { getPreOrder  :: t a } deriving (Eq, Show)
newtype PostOrder t a = PostOrder { getPostOrder :: t a } deriving (Eq, Show)

instance Foldable RoseTree where
    foldMap f (Node x ts) = f x `mappend` foldMap (foldMap f) ts
    toList = foldr (:) []

instance Foldable (PreOrder RoseTree) where
    toList = foldMap (\x -> [x])
    foldMap f (PreOrder (Node a children)) = f a <> foldMap (foldMap f . PreOrder) children

instance Foldable (PostOrder RoseTree) where
    toList = foldMap (\x -> [x])
    foldMap f (PostOrder (Node a children)) = foldMap (foldMap f . PostOrder) children <> f a

preOrder :: RoseTree a -> [a]
preOrder n = toList (PreOrder n)

postOrder :: RoseTree a -> [a]
postOrder n = toList (PostOrder n)

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