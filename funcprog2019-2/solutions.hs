solutions.hs

-- 1
data List a = Nil | Cons a (List a)

len :: List a -> Int
len Nil = 0
len (Cons a l) = 1 + (len l)

-- 2
newtype E a = E { appE :: a -> a }

instance Semigroup (E a) where
  E f <> E g = E (f . g)

instance Monoid (E a) where
  mempty = E id

-- 3
module C where

newtype Swap a = Swap { getSwap :: a } deriving (Eq, Show)

instance (Semigroup m) => Semigroup (Swap m) where
  Swap a <> Swap b = Swap (b <> a)

instance (Monoid m) => Monoid (Swap m) where
  mempty = Swap mempty

-- 4
data NatF a = ZeroF | SucF a
  deriving (Eq, Ord, Show)

instance Functor NatF where
  fmap f ZeroF    = ZeroF
  fmap f (SucF a) = SucF (f a)

data NonEmpty a = NonEmpty a [a] 
  deriving (Eq, Ord, Show)

instance Functor NonEmpty where
  fmap f (NonEmpty a l) = NonEmpty (f a) (f <$> l)
