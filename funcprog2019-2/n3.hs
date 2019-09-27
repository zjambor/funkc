{-# LANGUAGE FlexibleInstances, KindSignatures, InstanceSigs #-}
module N3 where

import Prelude hiding (Semigroup(..),Monoid(..), Maybe(..))

--import Data.Monoid
import Data.List
import Data.Function

newtype E a = E { appE :: a -> a }
--   deriving (Eq,Ord,Show)

class Semigroup m where
    (<>) :: m -> m -> m

--class Semigroup m => Monoid m where    -- van egy egységeleme - mempty
--    mempty :: m
--
newtype Endo' a = Endo' {runEndo :: a -> a}

{- instance Monoid (E a) where
    E f `mappend` E g = E (f . g)
    mempty = E id -}
--instance Monoid (E a) where
--   mempty = E id
   --mappend (E f) (E g) = E (f . g)


class Semigroup a => Monoid a where
    mempty  :: a

instance Semigroup b => Semigroup (a -> b) where
    f <> g = \x -> f x <> g x

instance Monoid b => Monoid (a -> b) where
    mempty = \x -> mempty

instance Semigroup a => Semigroup (E a) where
    (<>) (E f) (E g) = E (f . g)
    --(<>) = E . ((.) `on` appE)
    --(<>) (E f) (E g) = E (f . g)
    --(<>) = E . ((.) `on` appE)

instance Monoid a => Monoid (E a) where
    mempty = E id

--instance Semigroup (E a) where
--    (<>) = E id                 --E . ((.) `on` appE)

{- a1=appE (E (*2) <> E (+1)) 5 == 12
a2=appE (E tail <> E tail) [1,2,3] == [3]
a3=appE (E (drop 4) <> E (take 5)) [1..] == [5]  -}

{- a1=appE mempty 5 == 5
a2=appE mempty [1,2,3] == [1,2,3]
a3=appE (E (*2) <> mempty) 5 == appE (E (*2)) 5
a4=appE (mempty <> E (*2)) 5 == appE (E (*2)) 5 -}

newtype Fun a b = Fun (a -> b)

instance Semigroup b => Semigroup (Fun a b) where
    (<>) (Fun f) (Fun g) = Fun $ \x -> f x <> g x

--instance Monoid b => Monoid (Fun a b) where
--    mempty = Fun $ \x -> mempty

data Maybe a = Nothing | Just a 
    deriving (Show,Eq,Ord)

instance Functor Maybe where
    fmap f Nothing = Nothing
    fmap f (Just a) = Just (f a)

--instance Functor (Fun a) where
--    fmap f (Fun g) = \x -> Fun ((f . g) x)

--
-- data RoseTree a = Nil | Branch [RoseTree a]

data InfiniTree k v = Nil | Branch v (k -> InfiniTree k v)

--type MaybeTree a = InfiniTree Nothing | Just (a)
-- type ListTree a = InfiniTree _ _
-- type BinTree a = InfiniTree _ _
