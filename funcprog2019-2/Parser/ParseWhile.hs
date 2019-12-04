module ParseWhile where

import Control.Applicative
import Data.Functor

import Syntax
import Practice9

ws :: Parser () 
ws = void $ many (char ' ')

token' :: String -> Parser ()
token' str = void $ string str 

token :: String -> Parser ()
token str = token' str <* ws

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
   <|> expr'

var :: Parser Var 
var = (Var <$> some anyChar) <* ws

statement' :: Parser Statement 
statement' = (token "Skip" *> pure Skip)
        <|> Assign <$> var <*> (token ":=" *> expr)
        <|> If <$> (token "If"   *> expr) 
               <*> (token "then" *> statement) 
               <*> (token "else" *> statement)
        <|> While <$> (token "While" *> expr)
                  <*> (token "do"    *> statement) 

statement :: Parser Statement 
statement = Seq <$> statement' <*> (token ";" *> statement)
        <|> statement'