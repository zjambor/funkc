{-# LANGUAGE FlexibleContexts #-}
module Feladatok04 where

-- 1. Írj egy függvényt, ami beszámozza balról jobbra 0-tól kezdve
-- egy fa leveleit.

-- Pl: labelTree1 (Node1 (Leaf1 ()) (Node1 (Leaf1 ()) (Leaf1 ())))
--             == (Node1 (Leaf1 0) (Node1 (Leaf1 1) (Leaf1 2)))

data Tree1 a = Leaf1 a | Node1 (Tree1 a) (Tree1 a) deriving Show

labelTree1 :: Tree1 a -> Tree1 Int
labelTree1 tr = fst(lbl 0 tr)

lbl :: Int -> Tree1 a -> (Tree1 Int, Int)
lbl n (Leaf1 _) = (Leaf1 n, n + 1)
lbl n (Node1 ls rs) = case lbl n ls of
    (ls, n) -> case lbl n rs of
        (rs, n) -> (Node1 ls rs, n)
--
--labelTree2 (Node2 () [Node2 () [], Node2 () [Node2 () []]])

data Tree2 a = Node2 a [Tree2 a] deriving Show

-- labelTree2 :: Tree2 a -> Tree2 Int
-- labelTree2 tr = fst(lbl2 0 tr)

-- lbl2 :: Int -> Tree2 a -> (Tree2 Int, Int)
-- lbl2 n (Node2 _ []) = (Node2 n, n + 1)
-- lbl2 n (Node2 )

-- 2. Írd meg a következő függvényt. A függvény úgy működik,
--    mint a "filter", viszont ha a kapott (a -> Maybe Bool)
--    függvény valamely elemre Nothing-ot ad, akkor Nothing legyen
--    a végeredmény, egyébként Just <szűrt lista>

filterMaybe :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe f [] = Just []
filterMaybe f (a:as) = case f a of
    Nothing -> Nothing
    Just True -> (a:) <$> (filterMaybe f as)
    Just False -> filterMaybe f as
--

fg :: Int -> Maybe Bool
fg a 
    | a>1 = Just True
    | a == 1 = Nothing
    | otherwise = Just False
--

-- 3. Alkalmazz egy (a -> Maybe b) függvény egy Tree1 minden levelére,
--    ha bármelyik alkalmazás Nothing-ot ad, legyen az eredmény Nothing!
mapMaybeTree :: (a -> Maybe b) -> Tree1 a -> Maybe (Tree1 b)
mapMaybeTree f (Leaf1 a) = Leaf1 <$> f a
mapMaybeTree f (Node1 l r) =
    case mapMaybeTree f l of
    Nothing -> Nothing
    Just l -> case mapMaybeTree f r of
        Nothing -> Nothing
        Just r -> Just (Node1 l r)
--
-- mapMaybeTree fg (Node1 (Leaf1 1) (Leaf1 3))
