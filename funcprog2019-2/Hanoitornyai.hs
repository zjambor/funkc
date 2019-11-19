{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module Hanoitornyai where

import Control.Monad.Writer

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

freeRod :: RodID -> RodID -> RodID
freeRod a b 
    | (a `elem` [A,B]) && (b `elem` [A,B]) = C
    | (a `elem` [A,C]) && (b `elem` [A,C]) = B
    | otherwise = A

type SolverM = Writer [Move]

moveM :: RodID -> RodID -> Problem -> SolverM Problem
moveM a b p = do
    tell [(a, b)]
    return (move a b p)

moveManyM :: Int -> RodID -> RodID -> Problem -> SolverM Problem
moveManyM n a b p = moveManyM' n a b c p where
    c = freeRod a b
    moveManyM' 0 _ _ _ p = do
        tell []
        return p
    moveManyM' n a b c p = do
        p' <- moveManyM' (n-1) a c b p
        p'' <- moveM a b p'
        moveManyM' (n-1) c b a p''

solve :: Problem -> [Move]
solve (a, b, c) = execWriter (moveManyM (length a) A C (a, b, c))
