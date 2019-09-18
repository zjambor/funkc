module Gyak2 where

import Prelude hiding (sum)

data Tree a = Leaf | Node a (Tree a) (Tree a)
   deriving Show
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Leaf = Leaf
mapTree f (Node a l r) = Node (f a) (mapTree f l) (mapTree f r)
--n=Node 4 (Node 5 Leaf Leaf) (Node 5 Leaf Leaf)
--mapTree times2 n

t1 = Leaf
t2 = Node 2 Leaf Leaf
t3 = Node 3 t2 t2
t4 = Node 4 t3 t3

{- instance Eq a => Eq (Tree a) where
    (==) Leaf Leaf = True
    (==) Leaf _ = False
    (==) (Node a xl xr) (Node b yl yr) = (a == b) && xl == yl && xr == yr
    (==) _ _ = False -}
--(Node x xl xr) == (Node y yl yr) = (x == y) && xl == yl && xr == yr

doubleSmallNumber x = if x > 100  
    then x  
    else x*2   

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1  

d1 = doubleSmallNumber 105
d2 = doubleSmallNumber 95
d3 = doubleSmallNumber' 95

--removeNonUppercase :: [Char] -> [Char]  
--removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']] 
removeNonUppercase st = [ c | c <- st, c == 'm']

eqBool :: Bool -> Bool -> Bool
eqBool True  True  = True
eqBool False False = True
eqBool _     _     = False

eqList :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList eqa []     []     = True
eqList eqa (x:xs) (y:ys) = eqa x y && eqList eqa xs ys
eqList _   _      _      = False

class Eq' a where        -- osztály deklaráció
    eq :: a -> a -> Bool   -- osztály metódus  
  
instance Eq' Bool where  -- instance
    eq True  True  = True
    eq False False = True
    eq _     _     = False
    
--class EqT a where        -- osztály deklaráció
--    eqTree :: a -> a -> Bool   -- osztály metódus

--instance Eq' Int where  -- instance
  --  eq x y  = x == y
    --eq False False = True
    --eq _     _     = False

instance Eq' a => Eq' [a] where
        eq [] [] = True
        eq (x:xs) (y:ys) = eq x y           &&    eq xs ys
        --    ^ eq "a" típusra       ^ rekurzív hívás
        eq _ _ = False
--eq [True, False] [True, False]

--instance Eq' (Tree a) where
instance Eq' a => Eq' (Tree a) where  -- instance   
    eq Leaf Leaf = True
    eq Leaf _ = False
    eq _ Leaf = False
    --eq _ _ = False
    eq (Node a xl xr) (Node b yl yr) = eq a b && eq xl yl && eq xr yr

--n1 = Node False (Node True Leaf Leaf) (Node True Leaf Leaf)
--n2 = Node True (Node True Leaf Leaf) (Node True Leaf Leaf)
--eq n1 n2
--n1 = Node 5 (Node 4 Leaf Leaf) (Node 4 Leaf Leaf)
--n2 = Node 5 (Node 4 Leaf Leaf) (Node 4 Leaf Leaf)

data Tree_ = Leaf_ | Node_ Tree_ Tree_
   deriving Show

instance Eq' Tree_ where
    eq Leaf_ Leaf_ = True
    eq (Node_ xl xr) (Node_ yl yr) = eq xl xr && eq yl yr

--n1 = Node_ (Node_ Leaf_ Leaf_) (Node_ Leaf_ Leaf_)
--n2 = Node_ (Node_ Leaf_ Leaf_) (Node_ Leaf_ Leaf_)
--eq n1 n2

eqPair :: (a -> a -> Bool) -> (b -> b -> Bool) -> (a, b) -> (a, b) -> Bool
eqPair eqa eqb (a, b) (a', b') = eqa a a' && eqb b b'
-- p1 = (True, False)

instance (Eq' a, Eq' b) => Eq' (a, b) where
   eq (a, b) (a', b') = eq a a' && eq b b'
--

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = (f x) : (map' f xs)
--
-- példa végtelen bináris fára:
tree :: Tree Int
tree = go 0 where
    go n = Node n (go (n+1)) (go (n+1))

-- vesszük egy fa első n rétegét:
takeTree :: Int -> Tree a -> Tree a
takeTree 0 _ = Leaf
takeTree n (Node a t1 t2) = Node a (takeTree (n-1) t1) (takeTree (n-1) t2)

-- feladat: írjunk Eq instance-ot Tree-hez
instance Eq a => Eq (Tree a) where
    Leaf == Leaf = True
    Node a xl xr == Node b yl yr = a == b && xl == yl && xr == yr
    _ == _ = False

--
-- 5. Definiálj egy "BinTree" típust, aminek csak annotáció nélküli
--    levelei és bináris elágazásai vannak.  Írj egy "numLeaves ::
--    BinTree -> Int" függvényt, ami megszámolja a leveleket.

data BinTree = Leafx | Nodex BinTree BinTree
   deriving (Show, Eq)

numLeaves :: BinTree -> Int
numLeaves Leafx = 1
numLeaves (Nodex l r) = numLeaves l + numLeaves r

-- 6. Írj egy "mapTree :: (a -> b) -> Tree a -> Tree b" függvényt, ami
--    az elágazásokban levő "a" értékekre egy függvényt alkalmaz.

data Tree__ a = TLeaf | TNode a (Tree__ a) (Tree__ a)
    deriving (Show, Eq)

mapTree' :: (a -> b) -> Tree__ a -> Tree__ b
mapTree' f TLeaf = TLeaf
mapTree' f (TNode a l r) = TNode (f a) (mapTree' f l) (mapTree' f r)

-- 7. Írj egy "applyMany :: [a -> b] -> a -> [b]" függvényt, ami egy
--    listában található minden függvényt alkalmaz egy
--    értékre. Pl. "applyMany [(+10), (*10)] 10 == [20, 100]".

applyMany :: [a -> b] -> a -> [b]
--applyMany [] _ = []
applyMany fs a = map (\f -> f a) fs

-- 8. Definiálj egy "NonEmptyList a" típust, akár ADT-ként, akár
--    típusszinonímaként, aminek az értékei nemüres listák.
--
--    Írj egy "fromList :: [a] -> Maybe (NonEmptyList a)" függvényt, ami
--    nemüres listát ad vissza egy standard listából, ha az input nem
--    üres.

type NonEmptyList a = (a, [a])

--data NonEmptyList a = (a : (NonEmptyList a))
    --deriving Show
fromList :: [a] -> Maybe (NonEmptyList a)
fromList (x : xs) = Just (x, xs)
fromList [] = Nothing

--    Írj egy "toList :: NonEmptyList a -> [a]" függvényt, ami értelemszerűen
--    működik

toList :: NonEmptyList a -> [a]
toList (x, xs) = x : xs

