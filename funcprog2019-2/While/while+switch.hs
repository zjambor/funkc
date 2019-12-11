import Data.List

-- * State
type Var   = String
type State = Var -> Integer

aState "x" = 1
aState "y" = 2
aState "z" = 3

showState :: [Var] -> State -> String
showState vars s =
  intercalate ", "
  [ v ++ " = " ++ show (s v) | v <- vars ]

see = showState ["x","y","z"]

-- * Arithematic expressions
-- syntax
data AExp
  = Literal Integer | Variable Var
  | Plus AExp AExp | Minus AExp AExp | Negate AExp

instance Show AExp where
  show (Literal n)   = show n
  show (Variable v)  = v
  show (Plus x1 x2)  = "(" ++ show x1 ++ " + " ++ show x2 ++ ")"
  show (Minus x1 x2) = "(" ++ show x1 ++ " - " ++ show x2 ++ ")"
  show (Negate x)    = "-(" ++ show x ++ ")"

lit :: Integer -> AExp
lit = Literal

var :: Var -> AExp
var = Variable

infixl 6 %+%

(%+%) :: AExp -> AExp -> AExp
(%+%) = Plus

infixl 6 %-%

(%-%) :: AExp -> AExp -> AExp
(%-%) = Minus

neg :: AExp -> AExp
neg = Negate

-- semantics
sAExp :: AExp -> State -> Integer
sAExp (Literal n)   _ = n
sAExp (Variable v)  s = s v
sAExp (Plus a1 a2)  s = (sAExp a1 s) + (sAExp a2 s)
sAExp (Minus a1 a2) s = (sAExp a1 s) - (sAExp a2 s)
sAExp (Negate a1)   s = -(sAExp a1 s)

-- * Boolean expressions
-- syntax
data BExp
  = TT | FF
  | Not BExp | And BExp BExp
  | AEQ AExp AExp | ALT AExp AExp

instance Show BExp where
  show TT = "true"
  show FF = "false"
  show (Not x) = "not (" ++ show x ++ ")"
  show (And x1 x2) = "(" ++ show x1 ++ " ^ " ++ show x2 ++ ")"
  show (AEQ x1 x2) = "(" ++ show x1 ++ " == " ++ show x2 ++ ")"
  show (ALT x1 x2) = "(" ++ show x1 ++ " < " ++ show x2 ++ ")"

true :: BExp
true = TT

false :: BExp
false = FF

lnot :: BExp -> BExp
lnot = Not

land :: BExp -> BExp -> BExp
land = And

infix 4 %==%

(%==%) :: AExp -> AExp -> BExp
(%==%) = AEQ

infix 4 %<%

(%<%) :: AExp -> AExp -> BExp
(%<%) = ALT

-- semantics
sBExp :: BExp -> State -> Bool
sBExp TT          _ = True
sBExp FF          _ = False
sBExp (Not b)     s = not (sBExp b s)
sBExp (And b1 b2) s = (sBExp b1 s) && (sBExp b2 s)
sBExp (AEQ a1 a2) s = (sAExp a1 s) == (sAExp a2 s)
sBExp (ALT a1 a2) s = (sAExp a1 s) <  (sAExp a2 s)

-- * While programs
-- syntax
data Stm
  = Assignment Var AExp
  | Skip
  | Composition Stm Stm
  | If BExp Stm Stm
  | While BExp Stm
  | Switch AExp [Case]

type Case = (Integer, Stm)

showcase (i, s) = " case " ++ show i ++ ": " ++ show s

instance Show Stm where
  show (Assignment var aexp) = var ++ " := " ++ show aexp
  show (Skip) = "skip"
  show (Composition s1 s2) = show s1 ++ "; " ++ show s2
  show (If b s1 s2) =
    "if (" ++ show b ++ ") then " ++ show s1 ++ " else " ++ show s2
  show (While b s) = "while (" ++ show b ++ ") do " ++ show s
  show (Switch a cs) =
    "switch (" ++ show a ++ ")" ++ (foldl1 (++) $ (map showcase cs))

infix 0 <=>

(<=>) :: Var -> AExp -> Stm
(<=>) = Assignment

skip :: Stm
skip = Skip

infixr 0 <.>

(<.>) :: Stm -> Stm -> Stm
(<.>) = Composition

if_ :: BExp -> Stm -> Stm -> Stm
if_ = If

while :: BExp -> Stm -> Stm
while = While

switch :: AExp -> [Case] -> Stm
switch = Switch

swcase :: Integer -> Stm -> Case
swcase i s = (i, s)

aProgram = switch (var "x" %+% lit 1) [swcase 1 ("y" <=> lit 11), swcase 2 ("y" <=> lit 22)]

-- semantics
-- auxiliary functions
cond :: (State -> Bool, State -> State, State -> State) -> (State -> State)
cond (b, c1, c2) s
  | b s       = c1 s
  | otherwise = c2 s

update :: Var -> AExp -> (State -> State)
update var aexp s v
  | v == var  = sAExp aexp s
  | otherwise = s v

fix :: (f -> f) -> f
fix f = f (fix f)

-- "direct-style" (denotational) semantics of While
sDS :: Stm -> (State -> State)
sDS (Assignment var aexp) = update var aexp
sDS Skip                  = id
sDS (Composition s1 s2)   = sDS s2 . sDS s1
sDS (If b s1 s2)          = cond (sBExp b, sDS s1, sDS s2)
sDS (While b s)           = fix f
  where f g = cond(sBExp b, g . sDS s, id)
sDS (Switch a cs)         = \st -> findCase (sAExp a st) cs $ st

findCase i [] = sDS skip
findCase i ((i2, s):cs)
  | i == i2   = sDS s
  | otherwise = findCase i cs
