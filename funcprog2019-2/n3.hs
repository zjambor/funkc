{-# LANGUAGE FlexibleInstances, KindSignatures, InstanceSigs #-}
module N3 where

import Prelude hiding (Semigroup(..), Monoid(..), Maybe(..))
import Data.Function

newtype E a = E { appE :: a -> a }

class Semigroup m where
    (<>) :: m -> m -> m

class Semigroup m => Monoid m where    -- van egy egységeleme - mempty
    mempty :: m

--instance Monoid (E a) where
--    mempty = E a

{- instance Semigroup (E a) where
    (<>) = E . ((.) `on` appE)

a1=appE (E (*2) <> E (+1)) 5 == 12
a2=appE (E tail <> E tail) [1,2,3] == [3]
a3=appE (E (drop 4) <> E (take 5)) [1..] == [5] -}

newtype Fun a b = Fun (a -> b)

instance Semigroup b => Semigroup (Fun a b) where
    (<>) (Fun f) (Fun g) = Fun $ \x -> f x <> g x

instance Monoid b => Monoid (Fun a b) where
    mempty = Fun $ \x -> mempty

data Maybe a = Nothing | Just a 
    deriving (Show,Eq,Ord)

instance Functor Maybe where
    fmap = undefined

instance Functor (Fun a) where
    fmap = undefined

--
-- data RoseTree a = Nil | Branch [RoseTree a]

data InfiniTree k v = Nil | Branch v (k -> InfiniTree k v)

-- type MaybeTree a = InfiniTree _ _
-- type ListTree a = InfiniTree _ _
-- type BinTree a = InfiniTree _ _
