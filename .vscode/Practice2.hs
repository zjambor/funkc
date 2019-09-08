module Practice2 where

import Prelude hiding (Maybe(..))

-- synonym
-- OverloadedString
type Name = String

-- compile time difference
-- no runtime overhead 
newtype TaggedName = N Name 

-- algebraic data type
-- Nat       ~ type ctor
-- Zero, Suc ~ data ctor
data Nat = Zero | Suc Nat
  
data Maybe a = Nothing | Just a 
  deriving Show

data Email = Email { _address :: String
                   , _domain  :: String 
                   }
  deriving Show

data Person = Person { _name  :: Name
                     , _age   :: Int
                     , _email :: Maybe Email
                     } 
  deriving Show

{-
_name :: Person -> Name 
_name (Person n _ _) = n 
_age :: Person -> Int 
_age (Person _ age _) = age
_email :: Person -> String
_email (Person _ _ email) = email
-}

zero :: Nat 
zero = Zero 

three :: Nat 
three = Suc (Suc (Suc Zero))

-- parameterized
-- unit ~ Nil
-- union ~ |
-- product ~ Cons _ _
data List a = Nil | Cons a (List a) 
  deriving Show

length' :: List a -> Nat
length' Nil = Zero
length' (Cons x xs) = Suc $ length' xs

addNat :: Nat -> Nat -> Nat
addNat Zero n = n 
addNat n Zero = n 
addNat (Suc n) m = Suc (n `addNat` m)

sum' :: List Nat -> Nat
sum' Nil = Zero 
sum' (Cons n xs) = n `addNat` (sum' xs)

-- (n + 1) * m = n * m + m
mulNat :: Nat -> Nat -> Nat 
mulNat Zero n = Zero
mulNat n Zero = Zero
mulNat (Suc n) m = m `addNat` (n `mulNat` m)
-- mulNat (Suc (Suc Zero)) (Suc (Suc (Suc Zero)))

instance Show Nat where
  --show :: Nat -> String
  show Zero    = "Zero"
  show (Suc n) = "Suc (" ++ show n ++ ")" 
