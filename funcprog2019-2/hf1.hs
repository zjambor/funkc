module HF1 where

--import Prelude hiding (Eq,Ord)

----
data Nat = Zero | Suc Nat
    deriving (Eq,Show)

addNat :: Nat -> Nat -> Nat 
addNat Zero n = n
addNat n Zero = n
--addNat (Suc n) m = Suc (n `addNat` m)
addNat (Suc n) m = Suc (addNat n m)

--addNat (Suc Zero) (Suc Zero)

mulNat :: Nat -> Nat -> Nat 
mulNat Zero n = Zero
mulNat n Zero = Zero
mulNat (Suc n) m = addNat m (mulNat n m)

-- mulNat (Suc Zero) (Suc (Suc Zero))
-- addNat (Suc (Suc Zero)) (mulNat Zero (Suc (Suc Zero)))

-- mulNat (Suc (Suc Zero)) (Suc (Suc (Suc Zero)))
-- addNat (Suc (Suc (Suc Zero))) (mulNat (Suc Zero) (Suc (Suc (Suc Zero))))
-- addNat (Suc (Suc (Suc Zero))) (mulNat Zero (Suc (Suc (Suc Zero))))

----
data List a = Nil | Cons a (List a) 
    deriving Show

product' :: List Nat -> Nat
product' Nil = Suc Zero
product' (Cons n xs) = mulNat n (product' xs)

x=product' (Cons (Suc (Suc Zero)) (Cons (Suc (Suc Zero)) Nil))
y=mulNat (Suc (Suc Zero)) (product' (Cons (Suc (Suc Zero)) Nil))
--mulNat (Suc (Suc Zero)) (product' Nil)

mapList :: (a -> b) -> (List a) -> (List b)
mapList f Nil = Nil
mapList f (Cons n xs) = Cons (f n) (mapList f xs)

plus1 :: Nat -> Nat
plus1 Zero = Suc Zero
plus1 m = (Suc m)

ll = (Cons (Suc (Suc Zero)) (Cons (Suc (Suc Zero)) Nil))
v = mapList plus1 ll

ll1 = Cons (Suc (Suc Zero)) Nil
--eredm = Cons (Suc (Suc Zero)) (Cons (Suc (Suc Zero)) Nil)

(+++) :: List a -> List a -> List a
(+++) Nil xs = xs
(+++) xs Nil = xs
-- (+++) (Cons n xs) ys = Cons n ((+++) xs ys)
(+++) (Cons n xs) ys = Cons n (xs +++ ys)

l2 = ll1 +++ ll1

instance Eq a => Eq (List a) where
    Nil == Nil = True
    (Cons a as) == (Cons b bs) = a == b && as == bs
    _ == _ = False

instance Ord Nat where 
    (<=) Zero _          = True 
    (<=) (Suc n) (Suc m) = n <= m
    (<=) (Suc _) Zero    = False 

instance Ord a => Ord (List a) where 
    (<=) Nil _           = True 
    (<=) (Cons a as) (Cons b bs) = (a <= b) || (a == b && as <= bs)
    (<=) (Cons _ _) Nil  = False 

----
integerToNat :: Integer -> Nat
integerToNat 0 = Zero
integerToNat n | n > 0 = Suc (integerToNat $ n-1)
               | otherwise = error "Cannot convert negative Integer to Nat"

instance Num Nat where
    (+) = addNat
    (*) = mulNat
    abs = id
    
    signum Zero = Zero
    signum _    = Suc Zero
    
    -- not total
    (-) n Zero = n
    (-) (Suc n) (Suc m) = n - m
    fromInteger = integerToNat
----

data Tree a b = Leaf a | Bin b (Tree a b) (Tree a b)
   deriving (Show,Eq)

{- mapTree :: (a -> c) -> (b -> d) -> Tree a b -> Tree c d
mapTree f _ (Leaf n) = Leaf (f n)
mapTree f g (Bin m l r) = Bin (g m) (mapTree f g l) (mapTree f g r) -}

mapTree :: (a -> c) -> (b -> d) -> Tree a b -> Tree c d
mapTree f g (Leaf n) = Leaf (f n)
mapTree f g (Bin n l r) = Bin (g n) (mapTree f g l) (mapTree f g r)

tr = Bin 2 (Leaf "Haskell") (Bin 1 (Leaf "is") (Leaf "great!"))
maptr = mapTree (++ " a") (* 2) tr
tr2 = Bin (Suc Zero) (Leaf (Suc Zero)) (Bin (Suc Zero) (Leaf (Suc Zero)) (Leaf (Suc Zero)))
maptr2 = mapTree plus1 plus1 tr2
-- = Bin (Suc (Suc Zero)) (Leaf (Suc (Suc Zero))) (Bin (Suc (Suc Zero)) (Leaf (Suc (Suc Zero))) (Leaf (Suc (Suc Zero))))

maptr3 = mapTree (* (Suc (Suc Zero))) (+ (Suc (Suc Zero))) tr2
maptr4 = mapTree (`mulNat` (Suc (Suc Zero))) (`addNat` (Suc (Suc Zero))) tr2
maptr5 = mapTree (length) (* 2) (Bin 2 (Leaf "Haskell") (Bin 1 (Leaf "is") (Leaf "great!")))
   == (Bin 4 (Leaf 7) (Bin 2 (Leaf 2) (Leaf 6)))


mapTree' :: (a -> c) -> (b -> d) -> Tree a b -> Tree c d
mapTree' f g (Leaf n) = Leaf (f n)
mapTree' f g (Bin n l r) = Bin (g n) (mapTree' f g l) (mapTree' f g r)

maptr6 = mapTree' (length) (* 2) (Bin 2 (Leaf "Haskell") (Bin 1 (Leaf "is") (Leaf "great!")))
   == (Bin 4 (Leaf 7) (Bin 2 (Leaf 2) (Leaf 6)))

main = [   
    maptr2 == Bin (Suc (Suc Zero)) (Leaf (Suc (Suc Zero))) (Bin (Suc (Suc Zero)) (Leaf (Suc (Suc Zero))) (Leaf (Suc (Suc Zero)))),    
    maptr3 == Bin (Suc (Suc (Suc Zero))) (Leaf (Suc (Suc Zero))) (Bin (Suc (Suc (Suc Zero))) (Leaf (Suc (Suc Zero))) (Leaf (Suc (Suc Zero)))),    
    maptr4 == maptr3,
    maptr6,
    (Cons 1 (Cons 4 Nil)) <= (Cons 2 (Cons 0 Nil)) ]
