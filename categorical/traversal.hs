{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

import Prelude hiding
    ( Functor(..)
    , Applicative(..)
    , Monad(..)
    , Foldable(..)
    , Traversable(..)
    , (<$>)
    )

class Functor f where
    fmap :: (a -> b) -> f a -> f b

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

-- instance Applicative f => Functor f where
--     fmap f u = pure f <*> u

class Functor f => Applicative f where
    pure :: a -> f a 
    ap :: f (a -> b) -> f a -> f b

(<*>) :: Applicative f => f (a -> b) -> f a -> f b
(<*>) = ap

newtype Const a b = Const { getConst :: a }

instance Functor (Const a) where
    fmap _ (Const a) = Const a

instance Monoid a => Applicative (Const a) where
    pure = const $ Const mempty
    Const a `ap` Const b = Const (a `mappend` b)

-- instance Monad f => Applicative f where
--     pure = return
--     ap phi u 
--         = phi >>= \f -> 
--           u >>= \a ->
--           return (f a)

class Applicative m => Monad m where
    return :: a -> m a
    return = pure
    (>>=) :: m a -> (a -> m b) -> m b


class Foldable t where
    fold :: Monoid a => t a -> a

-- instance Traversable 
instance Traversable t => Foldable t where
    fold = 

class (Foldable t, Functor t) => Traversable t where
    sequence :: Applicative f => t (f a) -> f (t a)

    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    traverse f = sequence . fmap f

instance Functor [] where
    fmap _ [] = []
    fmap f (a:as) = f a:fmap f as

instance Applicative [] where
    pure = (:[])
    [] `ap` _ = []
    (f:fs) `ap` as = fmap f as ++ (fs `ap` as)

instance Monad [] where
    return = (:[])
    [] >>= f = []
    (a:as) >>= f = f a ++ (as >>= f)

instance Traversable [] where
    sequence [] = pure []
    sequence (u:us) = (:) <$> u <*> sequence us

instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just a) = Just (f a)

instance Traversable Maybe where
    sequence Nothing = pure Nothing
    sequence (Just act) = Just <$> act
