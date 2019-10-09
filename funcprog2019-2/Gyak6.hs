{-# LANGUAGE FlexibleInstances, FlexibleContexts, KindSignatures, InstanceSigs #-}
module Gyak6 where

import Prelude hiding(Functor(..),Either(..),Semigroup(..),Monoid(..), Maybe(..))

newtype Swap a = Swap { getSwap :: a } deriving (Eq, Show)

newtype E a = E { appE :: a -> a }
--   deriving (Eq,Ord,Show)

newtype Sum a = Sum { getSum :: a }
  deriving (Show, Eq, Ord)

newtype Prod a = Prod { getProd :: a }
  deriving (Show, Eq, Ord)

class Semigroup m where
    (<>) :: m -> m -> m
--
class Semigroup a => Monoid a where
    mempty  :: a
--
instance Semigroup a => Semigroup (E a) where
    (<>) (E f) (E g) = E (f . g)

instance Monoid a => Monoid (E a) where
    mempty = E id

instance Semigroup b => Semigroup (a -> b) where
   f <> g = \x -> f x <> g x

instance Monoid b => Monoid (a -> b) where
   mempty = \x -> mempty

newtype Fun a b = Fun (a -> b)

instance Semigroup m where
    f <> g = \x -> f x <> g x
    
instance Semigroup (Sum Int) where
(<>) (Sum n) (Sum k) = Sum (n + k)

instance Semigroup (Prod Int) where
(<>) (Prod n) (Prod k) = Prod (n * k)

instance Semigroup m => Semigroup (Swap m) where
   (<>) (Swap f) = \x -> (f y) (f x)

-- instance Semigroup m => Semigroup (Swap m) where
--     (<>) (Swap x) (Swap y) = y ++ x

instance Monoid m where
    mempty = id

instance Monoid a => Monoid (Swap a) where
    mempty = Swap id
--
