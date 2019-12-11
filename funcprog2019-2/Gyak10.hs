{-# LANGUAGE KindSignatures,DeriveFunctor,InstanceSigs #-}
module Gyak10 where

import Control.Monad.Writer
--import Control.Applicative

htiz = fmap (*3) (+100) 1

data CMaybe a = CNothing | CJust Int a
    deriving(Show)

--
instance Functor CMaybe where
    fmap f CNothing = CNothing
    fmap f (CJust counter x) = CJust (counter + 1) (f x)
--

newtype F1 x a   = F1 (x -> a)
newtype F2 x a   = F2 ((a -> x) -> a)

instance Functor (F1 a) where
    fmap f (F1 g) = F1 (\a -> f (g a))

newtype DList a = DL { unDL :: [a] -> [a] }

fromList :: [a] -> DList a
fromList xs = DL (xs++)

toList :: DList a -> [a]
toList (DL f) = f []

instance Semigroup (DList a) where
    (<>) = mappend

instance Monoid (DList a) where
    mempty = DL (\xs -> [] ++ xs)
    (DL f) `mappend` (DL g) = DL (\xs -> f (g xs))

instance Show (DList a) where  
    show :: (DList a) -> String
    show d = show (toList d)


main = [
        unDL (fromList []) [1,2,3] == [1,2,3],
        unDL (fromList [1,2,3]) [4,5,6] == [1..6],
        toList (fromList []) == [],
        toList (fromList [1,2,3]) == [1,2,3]
        ]

{-
típusok
- nagyobb:
  - parser + eval + mtl, nagyobb feladat
- kisebb:
  - Functor, Foldable instance
  - lista/fa algoritmus (első gyak/hf feladatokhoz hasonló)
  - általános egyszerű State/Maybe/Reader monád feladat
-}