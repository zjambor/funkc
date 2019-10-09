{-# language DeriveFunctor #-}
module N5 where

import Control.Monad

newtype State s a = State {runState :: s -> (a, s)}
  deriving Functor

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return a = State $ \s -> (a, s)
  State sa >>= f = State $ \s -> case sa s of (a, s) -> runState (f a) s

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f =
  get >>= \s ->
  put (f s)

evalState :: State s a -> s -> a
evalState ma = fst . runState ma

execState :: State s a -> s -> s
execState ma = snd . runState ma

data NatF a = ZeroF | SucF a
  deriving (Eq, Ord, Show)
  
-- state monad
-- ahhoz hogy applikatív legyen, funktornak kell lennie
--state s a - egy a tipusu erteket szeretnenk kiszamitani egy s tipusra
--pl. state [int] double - egy doublet akarunk int-re
--get :: (state s) s - s globális állapotot visszaadja
--ebből a (State s) a monad
-- put - kap s erteket, beemeli ezt a kontextusba, nem szamit a visszateresi erteke
-- új állapottal felülírja a régit

--def state monad:
--newtype State s a = State(s -> (a,s)) -- a-t kiszamítja, s-t kapja

data BinTree a = Nil | Branch a (BinTree a) (BinTree a)
  deriving (Eq, Show)

--
-- instance Functor (State s) where
--     fmap :: (a -> b) -> State s a -> State s b
--     fmap f (State g) = State $ \s -> (f (fst $ g s), s)
--functor
-- applicative
--monad
--(>>=)

-- runState
-- evalState
-- execState
-- hoogle control.monad.state.lazy example-ök
