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

-- executeMove (A,C) (initial 5)
executeMoves :: [Move] -> Problem -> Problem
--executeMoves [] p = p
executeMoves (x:xs) p = do
    let c = executeMove x p
    return c  -- (executeMoves xs c)    -- let c' =
    --return c'

--executeMoves [(A,C),(C,B)] (initial 5)

applyMany :: [a -> b] -> a -> [b]
applyMany fs a = map (\f -> f a) fs

composeAll :: [a -> a] -> a -> a
composeAll = foldr (.) id
--composeAll [f, g, h] x = f (g (h x))

paratlan = composeAll [(`mod` 2),(`mod` 3)] 6 == 1
--composeAll [(`mod` 2),(`mod` 3)] 6
-- composeAll [(`mod` 5),(`mod` 6),(+ 4),(* 2)] 11
fy x = foldr (.) id [(+2), (*7)] x

igaz_e = [(((+) <$> Just 3 <*> Just 3) == (Just (+3) <*> Just 3)), (((+) <$> Just 3 <*> Just 3) == (\x -> Just (x+3)) 3 )]

-- Maybe as Monad:
yep = ( Just 3 >>= \x -> return (x+3) ) == ( (\x -> Just (x+3)) 3 )

main = --executeMove (A,C) (initial 5) --
    executeMove <$> [(A,C),(C,B)] <*> [(initial 5)]
