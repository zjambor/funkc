module Syntax where

import Data.Char
import Data.Map (Map(..))
import qualified Data.Map as Map

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Error.Class


------------------ EXERCISES -----------------

data Tree a b = Leaf b | Node a (Tree a b) (Tree a b)
  deriving (Eq, Show)

ex1 :: Tree Int Int
ex1 = Node 1 (Leaf 10) (Node 2 (Leaf 20) (Leaf 30))

instance Functor (Tree a) where
    fmap f (Leaf b) = Leaf (f b)
    fmap f (Node a t1 t2) = Node a (fmap f t1) (fmap f t2)

instance Foldable (Tree a) where
    foldMap f (Leaf b) = f b
    foldMap f (Node a t1 t2) = foldMap f t1 <> foldMap f t2 

instance Traversable (Tree a) where
    traverse f (Leaf a) = Leaf <$> f a
    traverse f (Node a t1 t2) = Node <$> pure a <*> traverse f t1 <*> traverse f t2 

bimap :: (a -> a') -> (b -> b') -> Tree a b -> Tree a' b'
bimap fa fb (Leaf b) = Leaf $ fb b
bimap fa fb (Node a t1 t2) = Node (fa a) (bimap fa fb t1) (bimap fa fb t2)

bitraverse :: Applicative f => (a -> f a') -> (b -> f b') -> Tree a b -> f (Tree a' b')
bitraverse fa fb (Leaf b) = Leaf <$> fb b
bitraverse fa fb (Node a t1 t2) = Node <$> (fa a) <*> bitraverse fa fb t1 <*> bitraverse fa fb t2 

helper :: Tree Int a -> State Int (Tree Int Int)
helper (Leaf _) = do
    n <- get
    pure $ Leaf n
helper (Node n t1 t2) = do
    m <- get
    let m' = n + m
    put m'
    t1' <- helper t1
    t2' <- helper t2
    put m
    pure $ Node n t1' t2'

annotateSums :: Tree Int a -> Tree Int Int
annotateSums t = evalState (helper t) 0

helper2 :: Tree a Int -> State Int (Tree Int Int)
helper2 (Leaf n) = do
    put n
    pure $ Leaf n
helper2 (Node n t1 t2) = do
    t1' <- helper2 t1
    m <- get
    t2' <- helper2 t2
    m' <- get
    put (m + m')
    pure $ Node (m + m') t1' t2'

annotateSums' :: Tree a Int -> Tree Int Int
annotateSums' t = evalState (helper2 t) 0

helper' :: Tree a b -> State Int (Tree (a, Int) (b, Int))
helper' (Leaf b) = do
    n <- get
    put (n + 1)
    pure $ Leaf (b, n)
helper' (Node a t1 t2) = do
    n <- get
    put (n + 1)
    t1' <- helper' t1
    t2' <- helper' t2
    pure (Node (a, n) t1' t2')

numberElems :: Tree a b -> Tree (a, Int) (b, Int)
numberElems t = evalState (helper' t) 0

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
  deriving (Eq, Ord, Show)

data Statement
  = Skip
  | Seq Statement Statement
  | If Expr Statement Statement
  | While Expr Statement
  | Assign Var Expr
  | Fail String
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
    "" -> pure ()
    _  -> empty

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
  str <- get
  case str of
    c:cs | f c -> c <$ put cs
    _          -> empty

char :: Char -> Parser Char
char c = satisfy (== c)

lowerAlpha :: Parser Char
lowerAlpha = satisfy isLower

natural :: Parser Int
natural = read <$> some (satisfy isDigit)

token :: Parser a -> Parser a
token pa = pa <* ws

string :: String -> Parser ()
string s = () <$ traverse char s

string' :: String -> Parser ()
string' s = token (string s)

ws :: Parser ()
ws = () <$ many (satisfy isSpace)

-------------------- PARSER WHILE --------------------

iLit :: Parser Lit
iLit = LInt <$> token natural

bLit :: Parser Lit
bLit =  (LBool True  <$ string' "true")
    <|> (LBool False <$ string' "false")

sLit :: Parser String
sLit = char '"' *> many (satisfy isSpace <|> satisfy isAlpha <|> satisfy isDigit <|> char '?' <|> char '*') <* token (char '"')

lit :: Parser Lit
lit = iLit <|> bLit

parens :: Parser a -> Parser a
parens p = string' "(" *> p <* string' ")"

keywords :: [String]
keywords = ["Skip", "If", "then", "else", "While", "do", "end", "true", "false", "not"]

ident :: Parser String
ident = do
  x <- token (some lowerAlpha)
  if elem x keywords
    then empty
    else pure x

var :: Parser Var
var = Var <$> ident

-- "3+5" -> EPlus (ELit (Lit 3)) (ELit (Lit 5))
expr' :: Parser Expr
expr' = (ELit <$> lit)
    <|> (EVar <$> var)
    <|> parens expr

expr :: Parser Expr
expr =
        (Not <$> (string' "not" *> expr))
    <|> Plus  <$> expr' <*> (string' "+"  *> expr)
    <|> Minus <$> expr' <*> (string' "-"  *> expr)
    <|> Mul   <$> expr' <*> (string' "*"  *> expr)
    <|> And   <$> expr' <*> (string' "&&" *> expr)
    <|> Eq    <$> expr' <*> (string' "==" *> expr)
    <|> LEq   <$> expr' <*> (string' "<=" *> expr)
    <|> expr'

statement' :: Parser Statement
statement' = (string' "Skip" *> pure Skip)
        <|> Assign <$> var <*> (string' ":=" *> expr)
        <|> If <$> (string' "If"   *> expr)
               <*> (string' "then" *> statement)
               <*> (string' "else" *> statement)
        <|> While <$> (string' "While" *> expr)
                  <*> (string' "do"    *> statement <* string' "end")
        <|> Fail <$> (string' "Fail" *> sLit)

statement :: Parser Statement
statement = Seq <$> statement' <*> (string' ";" *> statement)
        <|> statement'


-------------------- INTERPRETER --------------------


newtype RTVal = RTLit Lit
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

evalUnaryOp :: (Expr -> Eval a) ->
               (b -> RTVal) ->
               (a -> b) ->
               (Expr -> Eval RTVal)
evalUnaryOp evalArg mkRetVal op arg =
  evalBinOp evalArg evalArg mkRetVal (const <$> op) arg arg
  -- const <$> op is similar to: \lhs rhs -> op lhs

evalExpr :: Expr -> Eval RTVal
evalExpr (ELit l)        = evalLit l
evalExpr (EVar v)        = evalVar v
evalExpr (Plus lhs rhs)  = evalBinOp evalInt evalInt mkRTInt (+) lhs rhs
evalExpr (Minus lhs rhs) = evalBinOp evalInt evalInt mkRTInt (-) lhs rhs
evalExpr (Mul lhs rhs)   = evalBinOp evalInt evalInt mkRTInt (*) lhs rhs
evalExpr (And lhs rhs)   = evalBinOp evalBool evalBool mkRTBool (&&) lhs rhs
evalExpr (LEq lhs rhs)   = evalBinOp evalInt evalInt mkRTBool (<=) lhs rhs
evalExpr (Not arg)       = evalUnaryOp evalBool mkRTBool (not) arg

evalStatement :: Statement -> Eval ()
evalStatement Skip = pure ()
evalStatement (Seq s1 s2) = do {evalStatement s1; evalStatement s2; pure ()}
evalStatement (If e s1 s2) = do
    cond <- evalExpr e
    case cond of RTLit (LBool True)  -> evalStatement s1
                 RTLit (LBool False) -> evalStatement s2
                 RTLit _             -> throwError $ "Not a bool in if" 
    pure ()

evalStatement (While e s) = do
    cond <- evalExpr e
    case cond of RTLit (LBool True)  -> evalStatement s
                 RTLit (LBool False) -> evalStatement Skip
                 RTLit _             -> throwError $ "Not a bool in while" 
    pure ()

evalStatement (Assign var e) = do
    map <- get
    val <- evalExpr e 
    put (Map.insert var val map)
    pure ()

evalStatement (Fail str) = throwError str