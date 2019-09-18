{-# LANGUAGE FlexibleInstances, KindSignatures, InstanceSigs #-}
module N2 where

import Prelude hiding (Semigroup(..), Monoid(..), Functor(..))

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)

data Union a b = Left a                 -- Either a b   +
            | Right b       

data Product a b = Product a b      --(a,b) *

data Unit = Unit                    -- egységtípus ()   1

-- Union Int Unit =
--     Right Unit :: _:_
--     Left 3 :: _:_

data Void

-- type Bool = Union Unit Unit
--             Left Unit
--             Right Unit

data Exp a b = Fun (a -> b)

data List a = Nil | Cons a (List a)
    deriving (Eq,Ord,Show)

--
len :: List a -> Int
len Nil = 0
len (Cons _ xs) = 1 + len xs
--len (Cons 1 (Cons 2 (Cons 3 Nil)))

class Semigroup m where
    (<>) :: m -> m -> m
-- mappend
-- asszociatív

-- Sum és Product newtype definíciók
-- data - 1 konstruktor / newtype - sok konstruktor

{- newtype Sum a = Sum a
    deriving (Eq,Ord,Show)

getSum :: Sum a -> a
getSum (Sum x) = x -}

-- ehelyett:

newtype Sum a = Sum { getSum :: a}
    deriving (Eq,Ord,Show)


-- Sum {getSum = True}

newtype Prod a = Prod { getProd :: a}
    deriving (Eq,Ord,Show)

instance Semigroup (Sum Int) where
    (<>) (Sum n) (Sum k) = Sum (n + k)

instance Semigroup (Prod Int) where
    (<>) (Prod n) (Prod k) = Prod (n * k)
--
-- Sum 2 <> Sum 3 :: Sum Int

-- 2 :: Int
-- 2 :: Double

-- superclass -> class
class Semigroup m => Monoid m where    -- van egy egységeleme - mempty
    mempty :: m 

instance Monoid (Sum Int) where
    mempty = Sum 0

instance Monoid (Prod Int) where
    mempty = Prod 1

-- List kifejezése algebrailag
-- listákhoz megírni semigroup monoidokat 
-- tetszőleges típus, aminek nincs egységeleme

-- Functor f :: * -> *           Kind / type of types
-- típusok típusa
-- egy műveletet vár el:
-- fmap :: (a->b) -> f a -> f b

-- Cons 1 Nil :: List Int
-- List Int :: *        -- kind
-- List :: * -> *       -- kind

class Functor (f :: * -> *) where
    fmap :: (a -> b) -> f a -> f b

instance Functor List where
    fmap :: (a -> b) -> List a -> List b
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)
    --
--
