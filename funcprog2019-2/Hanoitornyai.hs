{-# LANGUAGE FlexibleInstances #-}
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

instance Enum RodID where
   toEnum 0 = A
   toEnum 1 = B
   toEnum 2 = C

   fromEnum A = 0
   fromEnum B = 1
   fromEnum C = 2 

initial :: Int -> Problem
initial n = ([1..n],[],[])

validateRod :: Rod -> Bool
validateRod [] = True
validateRod [x] = True
validateRod (x:y:xs) = x < y && validateRod (y:xs)

validateProblem :: Problem -> Bool
validateProblem (x, y, z) = (validateRod x) && (validateRod y) && (validateRod z)

{- move :: [Int] -> Column -> Column -> Column -> Writer String ()
move [] _ _ _ = return ()  -- nothing to move.
 
move (largest:rest) src dst tmp = do
  move rest src tmp dst    -- move the top blocks from src to tmp using dest as temp
  tell $ "move " ++ show largest ++ " from " ++ show src ++ " to " ++ show dst ++ "\n"  -- move the largest one from src to dst
  move rest tmp dst src     -- move the tmp ones back onto the largest one on dst using src as temp -}

moveab :: Problem -> Problem
moveab ((x:xs), y, z) = (xs, (x:y), z)

moveac :: Problem -> Problem
moveac ((x:xs), y, z) = (xs, y, (x:z))

movebc :: Problem -> Problem
movebc (x, (y:ys), z) = (x, ys, (y:z))

moveba :: Problem -> Problem
moveba (x, (y:ys), z) = ((y:x), ys, z)

moveca :: Problem -> Problem
moveca (x, y, (z:zs)) = ((z:x), y, zs)

movecb :: Problem -> Problem
movecb (x, y, (z:zs)) = (x, (z:y), zs)

move :: RodID -> RodID -> Problem -> Problem
move a b (x,y,z) =
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
executeMoves (x:xs) (x,y,z) = executeMove x (x,y,z)