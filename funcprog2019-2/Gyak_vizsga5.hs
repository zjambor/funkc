{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# options_ghc -fwarn-incomplete-patterns #-}

module Solution where

import Data.Map (Map(..))
import qualified Data.Map as Map

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Error.Class
import Data.Char
import Data.Functor (void)

------------------ EXERCISES -----------------

data Either' a b = Left' a | Right' b | Both a b
  deriving (Eq, Ord, Show)
  --deriving (Eq, Show, Functor, Foldable, Traversable)

instance Functor (Either' a) where
    fmap f (Left' a) = Left' a
    fmap f (Right' b) = Right' (f b)
    fmap f (Both a b) = Both a (f b)

instance Foldable (Either' a) where
    foldMap f (Left' a) = mempty
    foldMap f (Right' b) = f b
    foldMap f (Both a b) = f b

instance Traversable (Either' a) where
    traverse f (Left' a) = pure (Left' a)
    traverse f (Right' b) = Right' <$> f b
    traverse f (Both a b) = Both a <$> f b

partition :: [Either' a b] -> ([a], [b], [(a, b)])
partition as = partition' as ([], [], []) where
    partition' :: [Either' a b] -> ([a], [b], [(a, b)]) -> ([a], [b], [(a, b)])
    partition' [] (x, y, z) = (reverse x, reverse y, reverse z)
    partition' (r:rs) (x, y, z) = case r of
        Left' a  -> partition' rs (a : x, y, z)
        Right' b -> partition' rs (x, b : y, z)
        Both a b -> partition' rs (x, y, (a, b) : z)

zipWith' :: (Either' a b -> c) -> [a] -> [b] -> [c]
zipWith' f []       []      = []
zipWith' f []       (b:bs)  = f (Right' b) : zipWith' f [] bs 
zipWith' f (a:as)   []      = f (Left' a) : zipWith' f as [] 
zipWith' f (a:as)   (b:bs)  = f (Both a b) : zipWith' f as bs 

mapMaybeLeft :: (a -> Maybe b) -> [Either' a c] -> Maybe [Either' b c]
mapMaybeLeft f xs = traverse go xs where
    go (Left' a) = Left' <$> f a
    go (Right' b) = pure (Right' b)
    go (Both a b) = flip Both b <$> f a

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

treeSums :: Tree Int -> Tree Int
treeSums t = evalState (traverse go t) 0 where
  go :: Int -> State Int Int
  go a = do 
      n <- get
      put (n + a)
      pure (n)

main = [
        treeSums (Leaf 10) == Leaf 0,
        treeSums (Node (Leaf 10) (Leaf 10)) == Node (Leaf 0) (Leaf 10),
        treeSums (Node (Leaf 10) (Node (Leaf 10) (Leaf 10))) == Node (Leaf 0) (Node (Leaf 10) (Leaf 20)),
        treeSums (Node (Node (Leaf 1) (Leaf 100)) (Leaf 0)) == Node (Node (Leaf 0) (Leaf 1)) (Leaf 101)
        ]
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
  | LogStr String 
  | LogInt Expr 
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
sLit = char '\"' *> many (satisfy isSpace <|> satisfy isAlphaNum <|> char '?' <|> char '*') <* token (char '\"')
--sLit = char '\"' *> many (lowerAlpha <|> satisfy isAlphaNum <|> char ' ') <* token (char '\"')

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
        <|> LogStr <$> sLit
        <|> LogInt <$> parens expr

statement :: Parser Statement
statement = Seq <$> statement' <*> (string' ";" *> statement)
        <|> statement'

-------------------- INTERPRETER --------------------

newtype RTVal = RTLit Lit
  deriving (Eq, Ord, Show)

type VarMapping = Map Var RTVal

type Eval a = StateT (VarMapping, [String]) (ExceptT String Identity) a

runEval :: Eval a -> VarMapping  -> Either String (a, (VarMapping, [String]))
runEval m s = runExcept (runStateT m (s,[]))

evalEval :: Eval a -> VarMapping -> Either String a
evalEval m s = fst <$> runEval m s

evalLit :: Lit -> Eval RTVal
evalLit lit = return $ RTLit lit

evalVar :: Var -> Eval RTVal
evalVar v = do
  (vars, _) <- get
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
evalExpr (Eq l r)        = evalBinOp evalInt evalInt mkRTBool (==) l r

evalStatement :: Statement -> Eval ()
evalStatement s = case s of
  Skip      -> pure ()
  Seq s s'  -> evalStatement s >> evalStatement s'
  If e s s' -> do
    b <- evalBool e
    if b then evalStatement s
         else evalStatement s'
  While e s -> do
    b <- evalBool e
    if b then evalStatement s >> evalStatement (While e s)
         else pure ()
  Assign x e -> do
    v <- evalExpr e
    modify $ \(vm, log) -> (Map.insert x v vm, log)
  LogStr s  -> do
      modify $ \(vm, log) -> (vm, s : log)
  LogInt e  -> do
      v <- evalExpr e
      modify $ \(vm, log) -> (vm, show v : log)
