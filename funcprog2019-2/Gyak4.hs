module Gyak4 where

import Prelude hiding (Either(..))

data List a = Nil
            | Cons a (List a)
  deriving (Show, Eq)

-- generic representation
data Union a b   = Left a | Right b
data Product a b = Product a b
data Unit        = Unit

instance (Eq a, Eq b) => Eq (Union a b) where
  (==) (Left x) (Left y) = x == y
  (==) (Right x) (Right y) = x == y
  (==) _ _ = False

instance (Eq a, Eq b) => Eq (Product a b) where
  (==) (Product a1 b1) (Product a2 b2) = a1 == a2 && b1 == b2

instance Eq Unit where
  (==) x y = True

newtype GList a = GL (Union Unit (Product a (GList a)))
  deriving Eq  -- this will use the Eq instance for the underlying generic representation