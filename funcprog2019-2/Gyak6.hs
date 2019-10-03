{-# LANGUAGE FlexibleInstances, KindSignatures, InstanceSigs #-}
module Gyak6 where

import Prelude hiding(Functor(..),Either(..),Semigroup(..),Monoid(..), Maybe(..))

newtype Swap a = Swap { getSwap :: a } deriving (Eq, Show)

newtype E a = E { appE :: a -> a }
--   deriving (Eq,Ord,Show)

class Semigroup m where
    (<>) :: m -> m -> m
--

newtype Fun a b = Fun (a -> b)

instance Semigroup m => Semigroup (Swap m) where
    (<>) (Swap f) (Swap g) = Swap $ \x -> g x <> f x
--
