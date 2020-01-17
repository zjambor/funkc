{-# LANGUAGE InstanceSigs,RecordWildCards #-}
module Vizsga_Gyak2 where
    
import Data.Functor (void)
import Data.Map (Map(..))
import qualified Data.Map as Map

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Error.Class

------------------ EXERCISES -----------------

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)

instance Functor Tree where
  fmap f Empty        = Empty
  fmap f (Leaf x)     = Leaf (f x)
  fmap f (Node l k r) = Node (fmap f l) (f k) (fmap f r)

instance Foldable Tree where
    foldMap f Empty        = mempty
    foldMap f (Leaf a)     = f a
    foldMap f (Node l k r) = foldMap f l <> (f k) <> foldMap f r

instance Traversable Tree where
    traverse f Empty        = pure Empty
    traverse f (Leaf x)     = Leaf <$> f x
    traverse f (Node l k r) = Node <$> traverse f l <*> f k <*> traverse f r

-------------------- SYNTAX --------------------

data Lit
  = LBool Bool
  | LInt Int
  | LStr String
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
  | Concat Expr Expr
  deriving (Eq, Ord, Show)

data Statement
  = Skip
  | Seq Statement Statement
  | If Expr Statement Statement
  | While Expr Statement
  | Assign Var Expr  
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

numeric :: Parser Char
numeric = foldr (<|>) empty $ map char ['0'..'9']

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

-- sLit
sLit :: Parser Lit
sLit = (LStr <$> (char '\"' *> many (lowerAlpha <|> numeric <|> char ' ') <* token "\"")) <* ws

lit :: Parser Lit
lit = iLit <|> bLit <|> sLit

parens :: Parser a -> Parser a
parens p = token "(" *> p <* token ")"

-- "3+5" -> EPlus (ELit (Lit 3)) (ELit (Lit 5))
expr' :: Parser Expr
expr' = ELit <$> lit
    <|> parens expr

expr :: Parser Expr
expr = Plus <$> expr' <*> (token "+"  *> expr)
    <|> And  <$> expr' <*> (token "&&" *> expr)
    <|> Concat <$> expr' <*> (token "++" *> expr)
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

evalString :: Expr -> Eval String
evalString e = do
  e' <- evalExpr e
  case e' of
    RTLit (LStr s) -> return s
    _ -> throwError $ show e ++ " does not evaluate to a String"

mkRTInt :: Int -> RTVal
mkRTInt = RTLit . LInt

mkRTBool :: Bool -> RTVal
mkRTBool = RTLit . LBool

mkRTString :: String -> RTVal
mkRTString = RTLit . LStr

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
evalExpr (Concat lhs rhs) = evalBinOp evalString evalString mkRTString (++) lhs rhs

