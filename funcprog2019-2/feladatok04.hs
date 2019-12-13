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
lbl n (Node1 ls rs)
    | lbl n ls == (ls, n)
        | lbl n rs == (r, n) = (Node1 ls rs, n)
