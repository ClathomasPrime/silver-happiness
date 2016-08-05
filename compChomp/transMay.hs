{-# LANGUAGE DeriveFunctor, LambdaCase #-}

import Control.Monad
import Control.Applicative
import Control.Monad.Identity

compJoin :: (Monad m, Traversable m, Applicative f) => (a -> f (m b)) -> m a -> f (m b)
compJoin f = fmap join . traverse f

redJoin :: (Monad n, Traversable n, Applicative f) => n (f (n b)) -> f (n b)
redJoin = fmap join . sequenceA


compBind :: (Monad m, Monad n, Traversable n) => (a -> m (n b)) -> m (n a) -> m (n b)
compBind f u = fmap join . join . fmap sequenceA . fmap (fmap f) $ u

compBind' :: (Monad m, Monad n, Traversable n) => (a -> m (n b)) -> m (n a) -> m (n b)
compBind' f u = join . fmap (fmap join . sequenceA . fmap f) $ u

compBind'' :: (Monad m, Monad n, Traversable n) => (a -> m (n b)) -> m (n a) -> m (n b)
compBind'' f u = join . fmap (fmap join) . fmap (traverse f) $ u


-- join is a natural transformation, so `join . fmap f === fmap f . join`
-- hmmmm not quite: join . fmap (fmap f) === fmap f . join

data Comp f g a = Comp { getComp :: f (g a) }

instance (Functor f, Functor g) => Functor (Comp f g) where
    fmap f = Comp . fmap (fmap f) . getComp

instance (Applicative f, Applicative g) => Applicative (Comp f g) where
    pure = Comp . pure . pure
    u <*> v = Comp ((<*>) <$> getComp u <*> getComp v)

-- instance (Monad f, Monad g) => Monad (Comp f g) where
--     return = pure
--     u >>= f = Comp (getComp u >>= (>>= getComp . f)) -- \ga -> return . f =<< ga

-- consider Comp 

instance (Monad m, Monad n, Traversable n) => Monad (Comp m n) where
    return = pure
    u >>= f = Comp . compBind (getComp . f) . getComp $ u


-- ExceptT comparison

data ExceptT e m a 
    = ExceptT { runExceptT :: m (Either e a) }
    deriving(Functor)

instance Monad m => Applicative (ExceptT e m) where
    pure = ExceptT . return . Right
    ExceptT u <*> ExceptT v = ExceptT $ phi <$> u <*> v
        where phi (Right f) (Right a) = Right $ f a
              phi (Left e) _ = Left e
              phi (Right _) (Left e) = Left e

instance Monad m => Monad (ExceptT e m) where
    return = pure 
    u >>= f = ExceptT . exceptBind (runExceptT . f) . runExceptT $ u


exceptBind :: Monad m => (a -> m (Either e b)) -> m (Either e a) -> m (Either e b)
exceptBind f u = 
    do u' <- u
       case u' of
            Left e -> return $ Left e
            Right a -> f a

exceptBind' :: Monad m => (a -> m (Either e b)) -> m (Either e a) -> m (Either e b)
exceptBind' f -- = u >>= \case{ Left e -> return $ Left e; Right a -> f a; }
    = join . fmap (\case{ Left e -> return $ Left e; Right a -> f a; })

-- fmap join . sequenceA . fmap f =?= \case{ Left e -> return $ Left e; Right a -> f a; })


-- recall that (>>= f) = join . fmap f


divErr :: Monad m => Double -> ExceptT () m Double
divErr 0 = ExceptT . return . Left $ ()
divErr x = ExceptT . return . Right $ (1/x)

divErr' :: Monad m => Double -> Comp m (Either ()) Double
divErr' 0 = Comp . return . Left $ ()
divErr' x = Comp . return . Right $ (1/x)
