{-# LANGUAGE GADTs, StandaloneDeriving #-}

import Prelude hiding(head, tail)

data Zero
data Succ r

data List n a where
    Nil :: List Zero a
    Cons :: a -> List n a -> List (Succ n) a

deriving instance Show a => Show (List n a)

head :: List (Succ n) a -> a
head (Cons a _) = a

tail :: List (Succ n) a -> List n a
tail (Cons _ l) = l

list :: List (Succ (Succ (Succ Zero))) Int
list = Cons 4 $ Cons 2 $ Cons 8 $ Nil
