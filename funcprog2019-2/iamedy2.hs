{-# LANGUAGE FlexibleInstances, InstanceSigs #-}
module Solution where

import Data.Map (Map(..))
import qualified Data.Map as Map

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Error.Class

------------------ EXERCISES -----------------

data XTree a = Omega | Alpha a (XTree a) | Beta a (XTree a) (XTree a)
  deriving (Eq, Ord, Show)

instance Functor XTree where
    fmap f Omega = Omega
    fmap f (Alpha a xs) = Alpha (f a) (fmap f xs)
    fmap f (Beta a t1 t2) = Beta (f a) (fmap f t1) (fmap f t2)

instance Foldable XTree where
    foldMap f Omega = mempty
    foldMap f (Alpha a xs) = f a <> foldMap f xs
    foldMap f (Beta a t1 t2) = f a <> foldMap f t1 <> foldMap f t2 

instance Traversable XTree where
    traverse f Omega = pure Omega
    traverse f (Alpha a xs) = Alpha <$> f a <*> traverse f xs
    traverse f (Beta a t1 t2) = Beta <$> f a <*> traverse f t1 <*> traverse f t2

--Adjuk meg azt a függvényt, amely visszaadja a legkisebb elemet egy XTree-ből!
leastElem :: Ord a => XTree a -> a
leastElem (Alpha a Omega) = a
leastElem (Alpha a ts) = minimum [a, leastElem ts]
leastElem (Beta a Omega Omega) = a
leastElem (Beta a ts Omega) = minimum [a, leastElem ts]
leastElem (Beta a Omega ts) = minimum [a, leastElem ts]
leastElem (Beta a t1 t2) = minimum [a, leastElem t1, leastElem t2]

labelPaths :: XTree a -> XTree (a, [a])
labelPaths t = evalState (traverse go t) [] where
    go :: a -> State [a] (a, [a])
    go a = do 
       n <- get
       put (a:n)
       pure (a, a:n)

-- containsAll :: Eq a => XTree a -> [a] -> Bool
-- containsAll ts (x:xs) = contains x ts where
--     contains :: a -> XTree a -> Bool
--     contains x (Beta x' l r) 
--         | x /= x' = contains x l && contains x r
--         | otherwise = True

ex1 :: XTree Int
ex1 = Alpha 1 $ Alpha 2 $
        Beta 3
          (Alpha 4 Omega)
          (Alpha 5 $ Alpha 6 $
            Beta 7
              (Alpha 8 Omega)
              Omega
          )

-------------------- SYNTAX --------------------

data Lit
  = LBool Bool
  | LInt Int
  deriving (Eq, Ord, Show)

type Name = String

newtype Var = Var Name
  deriving (Eq, Ord, Show)

data Expr
  -- atoms
  = ELit Lit
  | EVar Var
  -- arithmetic
  | Plus Expr Expr
  | Minus Expr Expr
  | Mul Expr Expr
  -- logical
  | And Expr Expr
  | Eq Expr Expr
  | LEq Expr Expr
  | Not Expr
  | Mod Expr Expr
  deriving (Eq, Ord, Show)

data Statement
  = Skip
  | Seq Statement Statement
  | If Expr Statement Statement
  | While Expr Statement
  | Assign Var Expr
  | Swap Var Var
  deriving (Eq, Ord, Show)


-------------------- PARSER BASE --------------------

type Parser a = StateT String Maybe a

runParser :: Parser a -> String -> Maybe (a, String)
runParser = runStateT

evalParser :: Parser a -> String -> Maybe a
evalParser p s = fmap fst $ runParser p s

eof :: Parser ()
eof = do
  str <- get
  case str of
    "" -> do
      put ""
      pure () -- lift $ Just (), lift $ pure ()
    _  -> lift $ Nothing

char :: Char -> Parser Char
char c = do
  str <- get
  case str of
    (x:xs) | x == c -> do
      put xs
      pure c
    _ -> lift $ Nothing


lowerAlpha :: Parser Char
lowerAlpha = foldr (<|>) empty $ map char ['a'..'z']

digit :: Parser Int
digit = fmap (\n -> n - 48)
      . fmap fromEnum
      . foldl (<|>) empty
      $ map char ['0'..'9']

natural :: Parser Int
natural = foldl1 (\acc cur -> 10*acc + cur) <$> some digit

token' :: String -> Parser ()
token' str = void $ traverse char str

token :: String -> Parser ()
token str = token' str <* ws

ws :: Parser ()
ws = void $ many (char ' ')


-------------------- PARSER WHILE --------------------

iLit :: Parser Lit
iLit = (LInt <$> natural) <* ws

bLit :: Parser Lit
bLit = token "true"  *> pure (LBool True)
    <|> token "false" *> pure (LBool False)

lit :: Parser Lit
lit = iLit <|> bLit

parens :: Parser a -> Parser a
parens p = token "(" *> p <* token ")"

-- "3+5" -> EPlus (ELit (Lit 3)) (ELit (Lit 5))
expr' :: Parser Expr
expr' = ELit <$> lit
    <|> parens expr

expr :: Parser Expr
expr = Plus <$> expr' <*> (token "+"  *> expr)
    <|> And  <$> expr' <*> (token "&&" *> expr)
    <|> Mod <$> expr' <*> (token "%" *> expr)
    <|> expr'

var :: Parser Var
var = (Var <$> some lowerAlpha) <* ws

statement' :: Parser Statement
statement' = (token "Skip" *> pure Skip)
        <|> Assign <$> var <*> (token ":=" *> expr)
        <|> If <$> (token "If"   *> expr)
                <*> (token "then" *> statement)
                <*> (token "else" *> statement)
        <|> While <$> (token "While" *> expr)
                  <*> (token "do"    *> statement <* token "end")
        <|> Swap <$> (token "(" *> var) <*> (token "," *> var) <* (token ")")

--string' "(" *> p <* string' ")"

statement :: Parser Statement
statement = Seq <$> statement' <*> (token ";" *> statement)
        <|> statement'


-------------------- INTERPRETER --------------------

data RTVal = RTLit Lit
  deriving (Eq, Ord, Show)

type VarMapping = Map Var RTVal

type Eval a = StateT VarMapping (ExceptT String Identity) a

runEval :: Eval a -> VarMapping -> Either String (a, VarMapping)
runEval m s = runExcept (runStateT m s)

evalEval :: Eval a -> VarMapping -> Either String a
evalEval m s = fst <$> runEval m s

evalLit :: Lit -> Eval RTVal
evalLit lit = return $ RTLit lit

evalVar :: Var -> Eval RTVal
evalVar v = do
  vars <- get
  let mVal = Map.lookup v vars
  case mVal of
    Just val -> return val
    Nothing  -> throwError $ "Undefined variable: " ++ show v

evalBinOp :: (Expr -> Eval a) ->
             (Expr -> Eval b) ->
             (c -> RTVal) ->
             (a -> b -> c) ->
             (Expr -> Expr -> Eval RTVal)
evalBinOp evalLhs evalRhs mkRetVal op lhs rhs = do
  lhs' <- evalLhs lhs
  rhs' <- evalRhs rhs
  let result = lhs' `op` rhs'
  return $ mkRetVal result

evalInt :: Expr -> Eval Int
evalInt e = do
  e' <- evalExpr e
  case e' of
    RTLit (LInt n) -> return n
    _ -> throwError $ show e ++ " does not evaluate to an Integer"

evalBool :: Expr -> Eval Bool
evalBool e = do
  e' <- evalExpr e
  case e' of
    RTLit (LBool b) -> return b
    _ -> throwError $ show e ++ " does not evaluate to a Boolean"

mkRTInt :: Int -> RTVal
mkRTInt = RTLit . LInt

mkRTBool :: Bool -> RTVal
mkRTBool = RTLit . LBool

(%) :: Int -> Int -> Int
(%) a b = fromIntegral (a `mod` b)

evalUnaryOp :: (Expr -> Eval a) ->
               (b -> RTVal) ->
               (a -> b) ->
               (Expr -> Eval RTVal)
evalUnaryOp evalArg mkRetVal op arg =
  evalBinOp evalArg evalArg mkRetVal (const <$> op) arg arg
  -- const <$> op is similar to: \lhs rhs -> op lhs

evalExpr :: Expr -> Eval RTVal
evalExpr (ELit l) = evalLit l
evalExpr (EVar v) = evalVar v
evalExpr (Plus lhs rhs) = evalBinOp evalInt evalInt mkRTInt (+) lhs rhs
evalExpr (Minus lhs rhs) = evalBinOp evalInt evalInt mkRTInt (-) lhs rhs
evalExpr (Mul lhs rhs) = evalBinOp evalInt evalInt mkRTInt (*) lhs rhs
evalExpr (And lhs rhs) = evalBinOp evalBool evalBool mkRTBool (&&) lhs rhs
evalExpr (LEq lhs rhs) = evalBinOp evalInt evalInt mkRTBool (<=) lhs rhs
evalExpr (Not arg) = evalUnaryOp evalBool mkRTBool (not) arg
evalExpr (Mod lhs rhs) = evalBinOp evalInt evalInt mkRTInt (%) lhs rhs


-------------------- MAIN ----------------------------

main = [
        fmap (2*) Omega                == Omega,
        fmap (2*) (Alpha 1 Omega)      == (Alpha 2 Omega),
        fmap (2*) (Beta 1 Omega Omega) == (Beta 2 Omega Omega),
        fmap ((+1) . (*2)) ex1         == (fmap (+1) . fmap (*2) $ ex1),

        sum Omega                 == 0,
        length (Alpha 1 Omega)    == 1,
        null (Beta 1 Omega Omega) == False,
        product ex1               == 40320,

        traverse Just ex1            == Just ex1,
        --traverse (const Nothing) ex1 == Nothing,

        leastElem ex1 == 1,
        leastElem (Alpha 3 $ Beta 4 (Alpha 2 Omega) (Omega)) == 2,

        -- containsAll ex1 [1,2,3]   == True,
        -- containsAll ex1 [0]       == False,
        -- containsAll ex1 [0,1,2,3] == False,      

        -- labelDepth (Alpha () $ Alpha () $ Alpha () $ Omega) == (Alpha ((),0) $ Alpha ((),1) $ Alpha ((),2) $ Omega)
        -- labelDepth (Alpha 3 $ Beta 4 (Alpha 2 Omega) (Alpha 4 Omega)) == (Alpha (3,0) $ Beta (4,1) (Alpha (2,2) Omega) (Alpha (4,2) Omega))
        -- fmap fst (labelDepth ex1) == ex1

        labelPaths (Alpha 1 $ Alpha 2 $ Alpha 3 $ Omega) == (Alpha (1,[1]) $ Alpha (2,[2,1]) $ Alpha (3,[3,2,1]) $ Omega),
        labelPaths (Alpha 1 $ Beta 2 (Alpha 3 Omega) (Alpha 4 Omega)) == (Alpha (1,[1]) $ Beta (2,[2,1]) (Alpha (3,[3,2,1]) Omega) (Alpha (4,[4,2,1]) Omega)),
        fmap (head . snd) (labelPaths ex1) == ex1,
        fmap fst (labelPaths ex1) == ex1,

        (fst <$> (runParser expr "11 % 3")) == Just (Mod (ELit $ LInt 11) (ELit $ LInt 3)),
        (fst <$> (runParser expr "11   %   3")) == Just (Mod (ELit $ LInt 11) (ELit $ LInt 3)),
        (fst <$> (runParser expr "11   %   true")) == Just (Mod (ELit $ LInt 11) (ELit $ LBool True)),
        (fst <$> (runParser expr "(11  %  3)  +  2")) == Just (Plus (Mod (ELit $ LInt 11) (ELit $ LInt 3)) (ELit $ LInt 2)),

        evalEval (evalExpr $ Mod (ELit $ LInt 11) (ELit $ LInt 3)) mempty == Right (RTLit (LInt 2)),
        (snd <$> runEval (evalExpr $ Mod (ELit $ LInt 11) (ELit $ LInt 3)) mempty) == Right mempty,

        (fst <$> (runParser statement "Swap(x,y)")) == Just (Swap (Var "x") (Var "y")),
        (fst <$> (runParser statement "Swap( x  , y )")) == Just (Swap (Var "x") (Var "y"))

        -- (snd <$> runEval (evalStatement (Swap (Var "x") (Var "y"))) (Map.fromList [(Var "x", RTLit $ LInt 0), (Var "y", RTLit $ LInt 1)])) == Right (Map.fromList [(Var "x", RTLit $ LInt 1), (Var "y", RTLit $ LInt 0)]),
        -- (snd <$> runEval (evalStatement (Swap (Var "x") (Var "y"))) (Map.fromList [(Var "x", RTLit $ LInt 0), (Var "y", RTLit $ LInt 1), (Var "z", RTLit $ LInt 2)])) == Right (Map.fromList [(Var "x", RTLit $ LInt 1), (Var "y", RTLit $ LInt 0), (Var "z", RTLit $ LInt 2)]),
        -- (snd <$> runEval (evalStatement (Swap (Var "x") (Var "y"))) (Map.fromList [(Var "x", RTLit $ LInt 0)])) == Left "undefined variable: y",
        -- (snd <$> runEval (evalStatement (Swap (Var "x") (Var "y"))) (Map.fromList [(Var "y", RTLit $ LInt 1)])) == Left "undefined variable: x"
        ]
