-- Magasabbrend� f�ggv�nyek, ADT-k (ism�tl�s)
--------------------------------------------------------------------------------

-- aj�nlott online jegyzetek:
--   http://lambda.inf.elte.hu/
--     - Kezd� Haskell szekci�: magasabbrend� f�ggv�nyek
--     - Halad� Haskell szekci�: t�pusdefin�ci�k

--------------------------------------------------------------------------------

-- 1. Defini�ld a "xor" m�veletet Bool t�puson.
xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False


-- 2. �rj egy f�ggv�nyt, ami megadja az els� n darab n�gyzetsz�m �sszeg�t.
--    P�lda: sqrsum 10 == 285. Tipp: listakifejez�st �rdemes haszn�lni,
--    l�sd: http://lambda.inf.elte.hu/Comprehensions.xml
sqrSum :: Int -> Int   -- sum of first n square numbers
sqrSum n | n == 0 = 0
         | otherwise = sumSquares [0..n-1] --(toList n)

sqrSum' :: Int -> Int
sqrSum' n = sum [x*x | x <- [0..n-1]]

sumSquares :: [Int] -> Int
sumSquares []       = 0
sumSquares (x:xs)   = x * x + sumSquares xs

toList :: Int -> [Int]
toList a = [0..a-1]

-- 3. Defini�ld a k�vetkez� f�ggv�nyeket tetsz�legesen, de
--    t�pushelyesen �s tot�lis f�ggv�nyk�nt (nem lehet v�gtelen loop
--    vagy kiv�tel).
f1 :: (a, (b, (c, d))) -> (b, c)
f1 = undefined

f2 :: (a -> b) -> a -> b
f2 = undefined

f3 :: (b -> c) -> (a -> b) -> a -> c
f3 = undefined

f4 :: (a -> b -> c) -> (b -> a -> c)
f4 = undefined

f5 :: ((a, b) -> c) -> (a -> b -> c)
f5 = undefined


-- 4. Defini�ld �jra a lista t�pust ADT-k�nt, "List a" n�ven.  Legyen
-- k�t konstruktor, egy az �res list�knak, egy pedig a kieg�sz�tett
-- list�knak

-- �rj egy
--    "mapList :: (a -> b) -> List a -> List b", ami a lista minden
--    elem�re egy f�ggv�nyt alkalmaz.
data List a  = Nil | Cons a (List a)-- eg�sz�tsd ki konstruktorokkal
  deriving Show

mapList :: (a -> b) -> List a -> List b
mapList f Nil = Nil
mapList f (Cons a xs) = Cons (f a) (mapList f xs)
-- add meg a defin�ci�t
l1 = Cons 1 Nil 
l2 = Cons 2 l1
l3 :: List Int
l3 = Cons 3 l2

times2 :: Int -> Int
times2 x = x * 2

-- 5. Defini�lj egy "BinTree" t�pust, aminek csak annot�ci� n�lk�li
--    levelei �s bin�ris el�gaz�sai vannak.  �rj egy "numLeaves ::
--    BinTree -> Int" f�ggv�nyt, ami megsz�molja a leveleket.
data BinTree

numLeaves :: BinTree -> Int
numLeaves = undefined


-- 6. �rj egy "mapTree :: (a -> b) -> Tree a -> Tree b" f�ggv�nyt, ami
--    az el�gaz�sokban lev� "a" �rt�kekre egy f�ggv�nyt alkalmaz.
data Tree a = TLeaf | TNode a (Tree a) (Tree a)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree = undefined


-- 7. �rj egy "applyMany :: [a -> b] -> a -> [b]" f�ggv�nyt, ami egy
--    list�ban tal�lhat� minden f�ggv�nyt alkalmaz egy
--    �rt�kre. Pl. "applyMany [(+10), (*10)] 10 == [20, 100]".
applyMany :: [a -> b] -> a -> [b]
applyMany = undefined


-- 8. Defini�lj egy "NonEmptyList a" t�pust, ak�r ADT-k�nt, ak�r
--    t�pusszinon�mak�nt, aminek az �rt�kei nem�res list�k.
--
--    �rj egy "fromList :: [a] -> Maybe (NonEmptyList a)" f�ggv�nyt, ami
--    nem�res list�t ad vissza egy standard list�b�l, ha az input nem
--    �res.
data NonEmptyList a -- lehet "type NonEmptyList a = ..." is

fromList :: [a] -> Maybe (NonEmptyList a)
fromList = undefined

--    �rj egy "toList :: NonEmptyList a -> [a]" f�ggv�nyt, ami �rtelemszer�en
--    m�k�dik
--toList :: NonEmptyList a -> [a]
--toList = undefined