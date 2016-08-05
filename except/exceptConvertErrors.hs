{-# LANGUAGE MultiParamTypeClasses, RankNTypes, DeriveFunctor, FlexibleInstances #-}
import Control.Applicative
import Control.Monad

-- This is an attempt to implement the idea 
--    * Add a pure function to convert e -> f (something like a subtype embedding)
--        * for example add it to the ErrorOf typeclass somehow (see exceptConvertErrors.hs)
class Monad (m e) => ErrorOf m e
-- ErrorOf :: (* -> * -> *) -> * -> Constraint

-- hmmmm m is a phantom type here... that may be strange
class (ErrorOf m e, ErrorOf m f) => SubError m e f where
    convert :: e -> f

instance ErrorOf m e => SubError m e e where
    convert = id

class MonadExcept m where
    hardThrow :: forall a e. ErrorOf m e => e -> m e a
    softThrow :: forall a e. ErrorOf m e => e -> m e ()
    softThrow = softThrow -- ^ this is a strange default implementation... 

    softCatch :: forall a e f. (SubError m e f) => m e a -> (e -> m f a) -> m f a
    hardCatch :: forall a e. ErrorOf m e => m e a -> (e -> m e a) -> m e a
    hardCatch = softCatch -- ^ this is a strange default implementation... 



-- Short circuiting error behavior

newtype ExceptT m e a 
    = ExceptT { runExceptT :: m (Either e a) }
    deriving(Functor)

instance Monad m => Applicative (ExceptT m e) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (ExceptT m e) where
    return = ExceptT . return . Right
    ExceptT u >>= f = ExceptT 
        $ do u' <- u
             case u' of
                  Left e -> return $ Left e
                  Right a -> runExceptT (f a)

instance Monad m => ErrorOf (ExceptT m) e

instance Monad m => SubError (ExceptT m) e f where
    convert = undefined 
        -- this instance should not be exported, but we never need to convert
        -- between errors in the "total failure" behavior

instance Monad m => MonadExcept (ExceptT m) where
    hardThrow = ExceptT . return . Left
    softCatch u h = ExceptT 
        $ do u' <- runExceptT u
             case u' of
                  Right a -> return $ Right a
                  Left e -> runExceptT $ h e


-- (combining attempts) This is an attempt to implement the idea "Embed the distinction between errors and warnings better" idea
data Partial e a 
    = Success a
    | Warnings e a
    | Errors e
    deriving(Functor)

appendWarnings :: Monoid e => e -> Partial e a -> Partial e a
appendWarnings e (Success a) = Warnings e a
appendWarnings e (Warnings e' a) = Warnings (e' `mappend` e) a
appendWarnings e (Errors e') = Errors (e' `mappend` e)

newtype ErrorsT m e a 
    = ErrorsT { runErrorsT :: m (Partial e a) }
    deriving(Functor)

instance (Monoid e, Monad m) => Applicative (ErrorsT m e) where
    pure = return
    (<*>) = ap

instance (Monoid e, Monad m) => Monad (ErrorsT m e) where
    return = ErrorsT . return . Success
    ErrorsT u >>= f = ErrorsT 
        $ do u' <- u
             case u' of
                  Success a -> runErrorsT $ f a
                  Warnings e a -> fmap (appendWarnings e) (runErrorsT $ f a)
                  Errors e -> return $ Errors e

instance Monad m => MonadExcept (ErrorsT m) where
    hardThrow e = ErrorsT . return . Errors $ e
    softThrow e = ErrorsT . return $ Warnings e ()

    softCatch u h = ErrorsT 
        $ do u' <- runErrorsT u
             case u' of
                  Success a -> return $ Success a
                  Warnings e a -> return $ Warnings ((convert :: SubError (ErrorsT m) e f => e -> f) e) a
                  Errors e -> runErrorsT $ h e
    hardCatch u h = ErrorsT 
        $ do u' <- runErrorsT u
             case u' of
                  Success a -> return $ Success a
                  Warnings e a -> return $ Warnings e a
                  Errors e -> runErrorsT $ h e


{-
-- Aggregating error behavior

newtype ErrorsT m e a 
    = ErrorsT { runErrorsT :: m (e, Maybe a) }
    deriving(Functor)

instance (Monoid e, Monad m) => Applicative (ErrorsT m e) where
    pure = return
    (<*>) = ap

instance (Monoid e, Monad m) => Monad (ErrorsT m e) where
    return a = ErrorsT . return $ (mempty, Just a)
    ErrorsT u >>= f = ErrorsT 
        $ do u' <- u
             case u' of
                  (e, Nothing) -> return (e, Nothing)
                  (e, Just a) 
                    -> fmap (\(e',b) -> (e' `mappend` e, b)) (runErrorsT (f a))

instance (Monoid e, Monad m) => ErrorOf (ErrorsT m) e

instance Monad m => MonadExcept (ErrorsT m) where
    softThrow e = ErrorsT . return $ (e, Nothing)
    softCatch u h = ErrorsT 
        $ do u' <- runErrorsT u
             case u' of
                  (e, Nothing) -> runErrorsT $ h e
                  (e, Just a) -> return $ ({-mempty-}undefined, Just a)


throwPartial :: Monad m => e -> ErrorsT m e ()
throwPartial e = ErrorsT . return $ (e, Just ())
-- ^ It is also strange that this is not included in the core interface, as
-- this function is basically required to use ErrorsT effectively

throwPartial' :: (MonadExcept m, ErrorOf m e) => e -> m e ()
throwPartial' e = throw e `catch` \_ -> return ()
-- ^ Goal: should be a no-op with ExceptT and ==throwPartial with ErrorsT

-}
