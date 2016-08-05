{-# LANGUAGE MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleInstances
           , DeriveFunctor
  #-}
import Prelude

ap :: Monad m => m (a -> b) -> m a -> m b
ap u v = do { f <- u; a <- v; return $ f a }

class Monad m => MonadMaybe m where
  throw :: m ()
  catch :: m a -> m a -> m a
  -- catch =?= <|>
  -- ermmm or catch =?= <<

data MaybeT m a
  = MaybeT { runMaybeT :: m (Maybe a) }
  deriving(Functor)

instance Applicative f => Applicative (MaybeT f) where
  pure = MaybeT . pure . Just
  MaybeT u <*> MaybeT v = MaybeT (phi <$> u <*> v)
    where phi Nothing _ = Nothing
          phi (Just _) Nothing = Nothing
          phi (Just f) (Just a) = Just $ f a
instance Monad m => Monad (MaybeT m) where
  return = pure
  MaybeT u >>= f = MaybeT
    $ do u' <- u
         case u' of
              Nothing -> return Nothing
              Just a -> runMaybeT (f a)

instance Monad m => MonadMaybe (MaybeT m) where
  throw = MaybeT . return $ Nothing
  catch u v = MaybeT
    $ do u' <- runMaybeT u
         case u' of
              Nothing -> runMaybeT v
              Just a -> return (Just a)



-- Partial a === Maybe (a,Bool)
-- (this implementation is considered elsewhere, but it 
--  is less explicit / self documenting and thus probably
--  not preferable)
data Partial a
  = Success a
  | Problems a
  | Failure

markProblematic :: Partial a -> Partial a
markProblematic (Success a) = Problems a
markProblematic p = p

data PartialT m a
  = PartialT { runPartialT :: m (Partial a) }
  deriving(Functor)

instance Applicative f => Applicative (PartialT f) where
  pure = MaybeT . pure . Success
  MaybeT u <*> MaybeT v = MaybeT (phi <$> u <*> v)
    where phi Failure _ = Failure
          phi _ Failure = Failure
          phi (Problems f) (Problems a) = Problems (f a)
          phi (Problems f) (Success a) = Problems (f a)
          phi (Success f) (Problems a) = Problems (f a)
          phi (Success f) (Success a) = Success (f a)
instance Monad m => Monad (PartialT m) where
  return = pure
  MaybeT u >>= f = MaybeT
    $ do u' <- u
         case u' of
              Failure -> return Failure
              Problems a -> markProblematic <$> runMaybeT (f a)
              Success a -> runMaybeT (f a)

failure :: PartialT m a
failure = PartialT . return $ Failure


