{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Except
    ( Outer(Outer)
    , cc
    , ccAs
    , hand
    ) where

data Inner e m a = Inner { fromInner :: m (Either e a) }

instance Functor m => Functor (Inner e m) where
    fmap f = Inner . fmap (fmap f) . fromInner
instance Applicative m => Applicative (Inner e m) where
    pure = Inner . pure . Right
    u <*> v = Inner (phi <$> fromInner u <*> fromInner v)
        where phi (Right f) (Right a) = Right (f a)
              phi (Left e) _ = Left e
              phi (Right _) (Left e) = Left e
              -- incidentally, phi = (<*>) :: Either e (a -> b) -> Either e a -> Either e b
instance Monad m => Monad (Inner e m) where
    return = pure
    u >>= f = Inner 
        $ do u' <- fromInner u
             case u' of
                  Left e -> return $ Left e
                  Right a -> fromInner $ f a

-- all functions should have return type `Outer e m`
data Outer e m a = Outer { fromOuter :: Inner e m a }

cc :: Outer e m a -> Inner e m a
cc = fromOuter

ccAs :: Functor m => (e -> f) -> Outer e m a -> Inner f m a
ccAs f u = Inner . fmap (mapLeft f) . fromInner . fromOuter $ u
    where mapLeft :: (x -> y) -> Either x a -> Either y a
          mapLeft _ (Right a) = Right a
          mapLeft f (Left x) = Left (f x)

hand :: Functor m => Outer e m a -> Inner f m (Either e a)
hand u = Inner . fmap Right . fromInner . fromOuter $ u
-- = Inner $ do u' <- fromInner . fromOuter $ u
--              case u' of 
--                   Left e -> return $ Right (Left e)
--                    Right a -> return $ Right (Right a)

