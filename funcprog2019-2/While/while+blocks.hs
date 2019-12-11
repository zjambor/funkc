import Data.List

-- * State
type Var   = String
type State = Var -> Integer

aState "x" = 1
aState "y" = 2
aState "z" = 3

showStore :: [Loc] -> Store -> String
showStore locs sto =
  intercalate ", "
  [ show l ++ " = " ++ show (sto l) | l <- locs ]

showState :: [Var] -> State -> String
showState vars s =
  intercalate ", "
  [ v ++ " = " ++ show (s v) | v <- vars ]

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

-- * Programs
-- syntax
data Stm
  = Assignment Var AExp
  | Skip
  | Composition Stm Stm
  | If BExp Stm Stm
  | While BExp Stm
  | Block DecV DecP Stm
  | Call Proc

data DecV
  = NoDecV
  | VarDec Var AExp DecV

data DecP
  = NoDecP
  | ProcDec Proc Stm DecP

instance Show Stm where
  show (Assignment var aexp) = var ++ " := " ++ show aexp
  show (Skip) = "skip"
  show (Composition s1 s2) = show s1 ++ "; " ++ show s2
  show (If b s1 s2) =
    "if (" ++ show b ++ ") then " ++ show s1 ++ " else " ++ show s2
  show (While b s) = "while (" ++ show b ++ ") do " ++ show s
  show (Block dv dp s) = "begin " ++ show dv ++ " " ++ show dp ++ " do " ++ show s ++ " end"
  show (Call p) = "call " ++ p

instance Show DecV where
  show (NoDecV) = ""
  show (VarDec v a dec) = "var " ++ v ++ " = " ++ show a ++ " " ++ show dec

instance Show DecP where
  show (NoDecP) = ""
  show (ProcDec p s dec) = "proc " ++ p ++ " is " ++ show s ++ " " ++ show dec

type Proc = String

type Loc = Integer
type Store = Loc -> Integer

type EnvV = Var -> Loc
type EnvP = Proc -> (Store -> Store)

-- not so pretty
next :: Loc
next = -1

lookupvar :: EnvV -> Store -> State
lookupvar env sto = sto . env

new :: Loc -> Loc
new n = n + 1

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

block :: DecV -> DecP -> Stm -> Stm
block = Block

-- begin var x : = 1 ; proc p is x : = 0 ;
--   begin var x : = 2 ;
--     call p
--   end
-- end

innerBlock = block (VarDec "x" (lit 2) NoDecV) (NoDecP) (Call "p")

aBlockProgram = block (VarDec "x" (lit 1) NoDecV) (ProcDec "p" ("x" <=> lit 0) NoDecP) innerBlock

aProgram = while (var "x" %<% lit 100) ("x" <=> var "x" %+% lit 1) 

-- semantics
-- auxiliary functions
cond :: (Store -> Bool, Store -> Store, Store -> Store) -> (Store -> Store)
cond (b, c1, c2) s
  | b s       = c1 s
  | otherwise = c2 s

updateStore :: Loc -> AExp -> EnvV -> (Store -> Store)
updateStore l aexp envV sto loc
  | l == loc  = sAExp aexp (lookupvar envV sto)
  | otherwise = sto loc

updateStore2 :: Loc -> Integer -> (Store -> Store)
updateStore2 l i sto loc
  | l == loc  = i
  | otherwise = sto loc

updateVEnv :: Var -> Loc -> (EnvV -> EnvV)
updateVEnv v l envV var
  | v == var  = l
  | otherwise = envV v

updatePEnv :: Proc -> (Store -> Store) -> (EnvP -> EnvP)
updatePEnv p s envP proc
  | p == proc  = s
  | otherwise = envP p

fix :: (f -> f) -> f
fix f = f (fix f)

--------------------------------------------------------------------------------

sDS' :: Stm -> EnvV -> (Store -> Store)
sDS' (Assignment var aexp) envV = updateStore l aexp envV
  where l = envV var
sDS' Skip                  envV = id
sDS' (Composition s1 s2)   envV = (sDS' s2 envV) . (sDS' s1 envV)
sDS' (If b s1 s2)          envV = cond ((sBExp b) . (lookupvar envV), sDS' s1 envV, sDS' s2 envV)
sDS' (While b s)           envV = fix f
  where f g = cond((sBExp b) . (lookupvar envV), g . (sDS' s envV), id)

--------------------------------------------------------------------------------

sDS :: Stm -> EnvV -> EnvP -> (Store -> Store)
sDS (Assignment var aexp) envV envP = updateStore l aexp envV
  where l = envV var
sDS Skip                  envV envP = id
sDS (Composition s1 s2)   envV envP = (sDS s2 envV envP) . (sDS s1 envV envP)
sDS (If b s1 s2)          envV envP = cond ((sBExp b) . (lookupvar envV), sDS s1 envV envP, sDS s2 envV envP)
sDS (While b s)           envV envP = fix f
  where f g = cond((sBExp b) . (lookupvar envV), g . (sDS s envV envP), id)
sDS (Block dv dp s)       envV envP = \sto -> sDS s (envV' sto) (envP' sto) (sto' sto)
  where envV' sto = fst $ dVDS dv (envV, sto)
        sto' sto = snd $ dVDS dv (envV, sto)
        envP' sto = dPDS dp (envV' sto) envP
sDS (Call p)              envV envP = envP p

dVDS :: DecV -> (EnvV, Store) -> (EnvV, Store)
dVDS (NoDecV) (envV, sto) = (envV, sto)
dVDS (VarDec v a dv) (envV, sto) = dVDS dv ((updateVEnv v l) envV, sto')
  where l = sto next
        val =  sAExp a (lookupvar envV sto)
        sto' = ((updateStore2 next (new l)) . (updateStore2 l val)) sto

dPDS :: DecP -> EnvV -> EnvP -> EnvP
dPDS (NoDecP) envV envP = envP
dPDS (ProcDec p s dp) envV envP = dPDS dp envV ((updatePEnv p g) envP)
  where g = sDS s envV envP

--------------------------------------------------------------------------------

initVEnv "x" = 0
initVEnv v = error $ "Unbound name: " ++ v
initStore 0 = 5
initStore l = error $ "Undefined location: " ++ show l

-- testWhile = showState ["x"] $ lookupvar initVEnv $ sDS' aProgram initVEnv initStore
-- testWhile = showState ["x"] $ ((lookupvar initVEnv) . (sDS' aProgram initVEnv)) initStore

sDS'' s = \env -> ((lookupvar env) . (sDS' aProgram env))
testWhile = showState ["x"] $ sDS'' aProgram initVEnv initStore

--------------------------------------------------------------------------------

initVEnv2 v = error $ "Unbound name: " ++ v
initPEnv2 p = error $ "Unbound name: " ++ p
initStore2 (-1) = 12
initStore2 l = error $ "Undefined location: " ++ show l

testWhileExt = showStore [12, 13, -1] $ sDS aBlockProgram initVEnv2 initPEnv2 initStore2
