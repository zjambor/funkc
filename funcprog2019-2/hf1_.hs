module HF1 where
data Nat = Zero | Suc Nat
    deriving (Eq,Show)

addNat :: Nat -> Nat -> Nat 
addNat Zero n = n
addNat n Zero = n
addNat (Suc n) m = Suc (addNat n m)

mulNat :: Nat -> Nat -> Nat 
mulNat Zero n = Zero
mulNat n Zero = Zero
mulNat (Suc n) m = addNat m (mulNat n m)

data List a = Nil | Cons a (List a) 
    deriving Show

product' :: List Nat -> Nat
product' Nil = Suc Zero
product' (Cons n xs) = mulNat n (product' xs)

mapList :: (a -> b) -> (List a) -> (List b)
mapList f Nil = Nil
mapList f (Cons n xs) = Cons (f n) (mapList f xs)

(+++) :: List a -> List a -> List a
(+++) Nil xs = xs
(+++) xs Nil = xs
(+++) (Cons n xs) ys = Cons n (xs +++ ys)

instance Eq a => Eq (List a) where
    Nil == Nil = True
    (Cons a as) == (Cons b bs) = a == b && as == bs
    _ == _ = False

instance Ord a => Ord (List a) where 
    (<=) Nil _           = True 
    (<=) (Cons a as) (Cons b bs) = (a <= b) || (a == b && as <= bs)
    (<=) (Cons _ _) Nil  = False 

data Tree a b = Leaf a | Bin b (Tree a b) (Tree a b)
    deriving (Show, Eq)

mapTree :: (a -> c) -> (b -> d) -> Tree a b -> Tree c d
mapTree f g (Leaf n) = Leaf (f n)
mapTree f g (Bin n l r) = Bin (g n) (mapTree f g l) (mapTree f g r)
