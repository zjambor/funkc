{-# LANGUAGE FlexibleInstances, FlexibleContexts, InstanceSigs #-}
module Gyak9_hanoi where

import Control.Monad.Writer
--import Data.Semigroup
--import Data.Monoid ((<>), mempty)

-- Disks of different sizes
type Disk = Int
-- A rod can have several disks on it
type Rod  = [Disk]
-- A Hanoi problem consists of three rods
type Problem = (Rod, Rod, Rod)
-- Identifier for the rods
data RodID = A | B | C
  deriving (Eq, Ord, Show)
-- Move the topmost disk from one rod to another
type Move = (RodID, RodID)

initial :: Int -> Problem
initial n = ([1..n],[],[])

validateRod :: Rod -> Bool
validateRod [] = True
validateRod [x] = True
validateRod (x:y:xs) = x < y && validateRod (y:xs)

validateProblem :: Problem -> Bool
validateProblem (x, y, z) = (validateRod x) && (validateRod y) && (validateRod z)

moveab :: Problem -> Problem
moveab ([], y, z) = ([], y, z)
moveab ((x:xs), y, z) = (xs, (x:y), z)

moveac :: Problem -> Problem
moveac ([], y, z) = ([], y, z)
moveac ((x:xs), y, z) = (xs, y, (x:z))

movebc :: Problem -> Problem
movebc (x, [], z) = (x, [], z)
movebc (x, (y:ys), z) = (x, ys, (y:z))

moveba :: Problem -> Problem
moveba (x, [], z) = (x, [], z)
moveba (x, (y:ys), z) = ((y:x), ys, z)

moveca :: Problem -> Problem
moveca (x, y, []) = (x, y, [])
moveca (x, y, (z:zs)) = ((z:x), y, zs)

movecb :: Problem -> Problem
movecb (x, y, []) = (x, y, [])
movecb (x, y, (z:zs)) = (x, (z:y), zs)

move :: RodID -> RodID -> Problem -> Problem
move a b (x,y,z) 
    | a == b = (x,y,z)
    | otherwise =
        case a of
            A -> case b of
                B -> moveab (x,y,z)
                C -> moveac (x,y,z)
            B -> case b of
                A -> moveba (x,y,z)
                C -> movebc (x,y,z)
            C -> case b of
                A -> moveca (x,y,z)
                _ -> movecb (x,y,z)

executeMove :: Move -> Problem -> Problem
executeMove (a,b) (x,y,z) = move a b (x,y,z)

executeMoves :: [Move] -> Problem -> Problem
executeMoves [] p = p
executeMoves (x:xs) p = do
    let c = executeMove x p
    executeMoves xs c

igaz_e = [(((+) <$> Just 3 <*> Just 3) == (Just (+3) <*> Just 3)), (((+) <$> Just 3 <*> Just 3) == (\x -> Just (x+3)) 3 )]

-- Maybe as Monad:
yep = ( Just 3 >>= \x -> return (x+3) ) == ( (\x -> Just (x+3)) 3 )

freeRod :: RodID -> RodID -> RodID
freeRod a b 
    | (a `elem` [A,B]) && (b `elem` [A,B]) = C
    | (a `elem` [A,C]) && (b `elem` [A,C]) = B
    | otherwise = A

type SolverM = Writer [Move]

moveM :: RodID -> RodID -> Problem -> SolverM Problem
moveM a b p = do
    tell [(a,b)]
    return (move a b p)

newtype DList a = DList { getDList :: [a] -> [a] }  

-- | This lets you use a difference list of a 'Semigroup' as a 'Monoid'.
diff :: Semigroup m => [m] -> DList m
diff = DList . (<>)

toDList :: [Move] -> DList Move
toDList xs = DList (xs++)

fromDList :: DList Move -> [Move]
fromDList (DList f) = f []

instance Semigroup (DList Move) where
    (<>) = mappend       --(DList n) (DList k) = DList (n)

instance Monoid (DList Move) where
    mempty = DList (\xs -> [] ++ xs)
    (DList f) `mappend` (DList g) = DList (\xs -> f (g xs))

-- fromDList (toDList [(A,B)] `mappend` toDList [(C,A)])

--The difference list equivalent of a list like [1,2,3] would be the function \xs -> [1,2,3] ++ xs. 
--A normal empty list is [], whereas an empty difference list is the function \xs -> [] ++ xs. 

--instance Show Move => Show (DList Move) where  
instance Show (DList Move) where  
    show :: (DList Move) -> String
    show d = show (fromDList d)
    --show (DList a) = show (fromDList (DList a))

type SolverM_ = Writer (DList Move)

moveM' :: RodID -> RodID -> Problem -> SolverM_ Problem
moveM' a b p = do
    tell (toDList [(a,b)])
    return (move a b p)

{- moveManyM :: Int -> RodID -> RodID -> Problem -> SolverM Problem
moveManyM n a b p = moveManyM' n a b c p where
    c = freeRod a b
    moveManyM' 1 a b _ p = moveM a b p
    moveManyM' n a b c p = do
        p' <- moveManyM' (n-1) a c b p
        p'' <- moveM a b p'
        moveManyM' (n-1) c b a p'' -}

moveManyM :: Int -> RodID -> RodID -> Problem -> SolverM_ Problem
moveManyM n a b p = moveManyM' n a b c p where
    c = freeRod a b
    moveManyM' 0 a b _ p = do
        tell []
        return p
    moveManyM' n a b c p = do
        p' <- moveManyM' (n-1) a c b p
        p'' <- moveM' a b p'
        moveManyM' (n-1) c b a p''

hanoi :: Int -> (RodID, RodID, RodID) -> [Move]
hanoi n (a, b, c) = hanoiToList n a b c []
  where
    hanoiToList 0 _ _ _ l = l
    hanoiToList n a b c l = hanoiToList (n-1) a c b ((a, b) : hanoiToList (n-1) c b a l)

solve :: Problem -> [Move]
solve (a, b, c) = execWriter (moveManyM (length a) A C (a, b, c))
--solve (a, b, c) = hanoi (length a) (A, B, C)

main = [ execWriter (moveManyM 0 A C ([1, 2, 3], [], [])) == [],
        (fst . runWriter) (moveManyM 0 A C ([1, 2, 3], [], [])) == ([1, 2, 3], [], []),
        solve ([1], [], []) == [(A, C)],
        solve ([1, 2, 3], [], []) == [(A, C), (A, B), (C, B), (A, C), (B, A), (B, C), (A, C)]
        ]
    --moveM' A B (initial 5)
        --moveManyM 5 A B (initial 5)
        --, executeMoves (solve (initial 5)) (initial 5) == ([],[1,2,3,4,5],[])
        --moveM A B (initial 5)
        --executeMoves (hanoi 5 (A,B,C)) (initial 5) 
        --solve (initial 5)
        --]
        --executeMove (A,C) (initial 5) --
        --[ executeMove <$> [(A,C),(C,B)] <*> [(initial 5)],
        --[ executeMoves [(A,B),(A,C),(B,C),(A,B),(A,C),(B,C)] (initial 5) ]]
