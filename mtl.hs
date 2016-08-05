{-# LANGUAGE MultiParamTypeClasses 
           , DeriveFunctor
           , FlexibleInstances
           , ScopedTypeVariables
           , FunctionalDependencies 
  #-}

class Monad m => ReaderMonad m r | m -> r where
    ask :: m r
    ask = reader id

    reader :: (r -> a) -> m a
    reader query = fmap query ask

    local :: (r -> r) -> m a -> m a

data ReaderT r m a = ReaderT 
    { runReaderT :: r -> m a }
    deriving(Functor)

instance Monad m => Applicative (ReaderT r m) where
    pure = return
    ReaderT u <*> ReaderT v 
        = ReaderT $ \r -> u r <*> v r

instance Monad m => Monad (ReaderT r m) where
    return a = ReaderT $ const (return a)
    (ReaderT u) >>= f = 
        ReaderT $ \r -> do a <- u r
                           b <- runReaderT (f a) r
                           return b

instance Monad m => ReaderMonad (ReaderT r m) r where
    ask = ReaderT return

    local modify u
        = ReaderT $ \r -> runReaderT u (modify r)

-- class Monad m => MonadState m a where
--     

    -- withDefault :: m a -> a -> m a
    -- withDefault u a = u `catch` (const (return a) :: e -> m a)

-- withDefault :: forall m e a. MonadException m e => m a -> a -> m a
-- withDefault u a = u `catch` (const (return a) :: e -> m a)

-- withDefault :: MonadException m e => m a -> a -> m a
-- withDefault u a = u `catch` (\e -> return a)

class Monad m => MonadException m e | m -> e where
    throw :: e -> m ()

    catch :: m a -> (e -> m a) -> m a

instance MonadException Maybe () where
    throw () = Nothing
    Just a `catch` _ = Just a
    Nothing `catch` err = err ()

data ExceptT e m a = ExceptT 
    { runExceptT :: m (Either e a) }
    deriving(Functor)

instance Applicative m => Applicative (ExceptT e m) where
    pure = ExceptT . pure . Right

    ExceptT f <*> ExceptT a 
        = ExceptT $ collectException <$> f <*> a
        where collectException ef ea = 
                case (ef, ea) of
                     (Left err, _) -> Left err
                     (Right _, Left err) -> Left err
                     (Right f, Right a) -> Right (f a)

instance Monad m => Monad (ExceptT e m) where
    return = pure

    ExceptT ma >>= f = ExceptT $ 
        do e <- ma
           case e of 
                Left err -> return $ Left err
                Right a -> runExceptT $ f a
