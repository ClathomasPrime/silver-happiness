{-# LANGUAGE DeriveFunctor
  #-}

ap :: Monad m => m (a -> b) -> m a -> m b
ap u v = do { f <- u; a <- v; return $ f a }

data ReaderT r m a 
    = ReaderT { runReaderT :: r -> m a }
    deriving(Functor)

data WriterT w m a 
    = WriterT { runWriterT :: m (a, w) }
    deriving(Functor)

data StateT s m a 
    = StateT { runStateT :: s -> m (a, s) }
    deriving(Functor)

data ExceptT e m a 
    = ExceptT { runExceptT :: m (Either e a) }
    deriving(Functor)

instance Monad m => Applicative (ReaderT r m) where
    pure = return
    (<*>) = ap
instance Monad m => Monad (ReaderT r m) where
    return a = ReaderT $ const (return a)
    ReaderT u >>= f = ReaderT $ \r ->
        do a <- u r
           runReaderT (f a) r

instance (Monoid w, Monad m) => Applicative (WriterT w m) where
    pure = return
    (<*>) = ap
instance (Monoid w, Monad m) => Monad (WriterT w m) where
    return a = WriterT $ return (a,mempty)
    WriterT u >>= f = WriterT 
        $ do (a, w) <- u
             (b, w') <- runWriterT (f a)
             return (b, w `mappend` w')

instance Monad m => Applicative (StateT s m) where
    pure = return
    (<*>) = ap
instance Monad m => Monad (StateT s m) where
    return a = StateT $ \s -> return (a, s)
    StateT u >>= f = StateT $ \s -> 
        do (a, s') <- u s
           runStateT (f a) s'

instance Monad m => Applicative (ExceptT e m) where
    pure = return
    (<*>) = ap
instance Monad m => Monad (ExceptT e m) where
    return = ExceptT . return . Right
    ExceptT u >>= f = ExceptT $ 
        do eith <- u
           case eith of
                Left e -> return $ Left e
                Right a -> runExceptT (f a)



data IOT m a 
    = IOT { runIOT :: m (IO a) }
    deriving(Functor)

instance Monad m => Applicative (IOT m) where
    pure = return
    (<*>) = ap
instance Monad m => Monad (IOT m) where
    return = IOT . return . return
    IOT u >>= f = IOT 
        $ do i <- u -- i :: IO a
             -- fmap f i :: IO (IOT m b) ~= IO (m (IO b))
             -- no general way to swap the IO and the m
             undefined

data IOT' m a 
    = IOT' { runIOT' :: IO (m a) }
    deriving(Functor)

instance Monad m => Applicative (IOT' m) where
    pure = return
    (<*>) = ap
instance Monad m => Monad (IOT' m) where
    return = IOT' . return . return
    IOT' u >>= f = IOT'
        $ do ma <- u
             -- fmap f ma :: m (IOT m b) ~= m (IO (m b))
             undefined
