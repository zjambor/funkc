{-# LANGUAGE FlexibleInstances #-}
module Gyak7 where

import Control.Applicative (Applicative(..), (<$>))
import Control.Monad
import Data.Monoid (Monoid(..))
import Data.Sequence (Seq, empty, singleton, (<|), (|>), fromList,
            ViewL(..), ViewR(..), viewl, viewr)
import Data.Foldable (Foldable(foldMap), toList)
import Data.Traversable (Traversable(traverse))
import Data.Typeable
import Control.DeepSeq (NFData(rnf))

-- | Multi-way trees, also known as /rose trees/.
data Tree a = Node {
        rootLabel :: a,         -- ^ label value
        subForest :: Forest a   -- ^ zero or more child trees
    }
  deriving (Eq, Read, Show)

type Forest a = [Tree a]

instance Functor Tree where
    fmap f (Node x ts) = Node (f x) (map (fmap f) ts)

instance Applicative Tree where
    pure x = Node x []
    Node f tfs <*> tx@(Node x txs) =
        Node (f x) (map (f <$>) txs ++ map (<*> tx) tfs)

instance Monad Tree where
    return x = Node x []
    Node x ts >>= f = Node x' (ts' ++ map (>>= f) ts)
      where Node x' ts' = f x

instance Traversable Tree where
    traverse f (Node x ts) = Node <$> f x <*> traverse (traverse f) ts

instance Foldable Tree where
    foldMap f (Node x ts) = f x `mappend` foldMap (foldMap f) ts

-- | The elements of a tree in pre-order.
flatten :: Tree a -> [a]
flatten t = squish t []
  where squish (Node x ts) xs = x:Prelude.foldr squish xs ts

-- | Lists of nodes at each level of the tree.
levels :: Tree a -> [[a]]
levels t =
    map (map rootLabel) $
        takeWhile (not . null) $
        iterate (concatMap subForest) [t]

----
preorder :: a -> [[a]] -> [a]
preorder x xs = x : concat xs
 
inorder :: a -> [[a]] -> [a]
inorder x [] = [x]
inorder x (y:xs) = y ++ [x] ++ concat xs
 
postorder :: a -> [[a]] -> [a]
postorder x xs = concat xs ++ [x]

foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f = go
  where
    go (Node x ts) = f x (go <$> ts)
 
--- iteration of bind (>>=) over sub-trees: 
levelOrder :: Tree a -> [a]
levelOrder x =
  takeWhile (not . null) (iterate (concatMap subForest) [x]) >>= fmap rootLabel

----
haskellTree :: Tree String
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
        
--main = [ [flatten haskellTree], levels haskellTree ]
main :: IO ()
main = do
  mapM_ print ([foldTree] <*> [preorder, inorder, postorder] <*> [haskellTree])
  print $ levelOrder haskellTree