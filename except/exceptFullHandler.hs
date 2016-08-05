{-# LANGUAGE MultiParamTypeClasses, RankNTypes, DeriveFunctor, FlexibleInstances #-}
import Control.Applicative
import Control.Monad

class Monad (m e) => ErrorOf m e
-- ErrorOf :: (* -> * -> *) -> * -> Constraint

-- Ideal: class (ErrorOf m e => Monad (m e)) => MonadExcept m where
-- not quite sure what it should be instead...
class MonadExcept m where
    throw :: forall a e. ErrorOf m e => e -> m e a
    catch :: forall a e f. (ErrorOf m e, ErrorOf m f) => m e a -> (e -> m f a) -> m f a


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

instance Monad m => MonadExcept (ExceptT m) where
    throw = ExceptT . return . Left
    catch u h = ExceptT 
        $ do u' <- runExceptT u
             case u' of
                  Right a -> return $ Right a
                  Left e -> runExceptT $ h e


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
    throw e = ErrorsT . return $ (e, Nothing)
    catch u h = ErrorsT 
        $ do u' <- runErrorsT u
             case u' of
                  (e, Nothing) -> runErrorsT $ h e
                  (e, Just a) -> return $ ({-mempty-}undefined, Just a)
                  -- ^ this is strange behavior without a much better option:
                  --    (e, Just a) -> return $ (e, Just a)
                  --       * Intuitively what we want, but doesn't type check
                  --    (e, Just a) -> return $ (mempty, Just a)
                  --       * throws away the old error/warning messages with no way to retain them
                  --    (e, Just a) -> fmap (\(f, _) -> (f, Just a)) (runErrorsT $ h e)
                  --       * Throws away the result of the handler (not so bad, we don't really want to run the handler at all)
                  --       * gives the handler control over the m-structure of the result
                  --           * we could "ask" the handlers to respect the m-structure if it's only a warning, not an error,
                  --             but the difference between warnings and errors is not accessible to the handler.
                  --             We would have to require users to embed the distinction into e/f, which is already broken
                  --             by ErrorsT which only requires a Monoid.
                  -- Solution ideas:
                  --    * Embed the distinction between errors and warnings better (see exceptAggroPartial.hs)
                  --    * Add a pure function to convert e -> f (something like a subtype embedding)
                  --        * for example, add it to the signature of catch or to the ErrorOf typeclass somehow (see exceptConvertErrors.hs)


throwPartial :: Monad m => e -> ErrorsT m e ()
throwPartial e = ErrorsT . return $ (e, Just ())
-- ^ It is also strange that this is not included in the core interface, as
-- this function is basically required to use ErrorsT effectively

throwPartial' :: (MonadExcept m, ErrorOf m e) => e -> m e ()
throwPartial' e = throw e `catch` \_ -> return ()
-- ^ Goal: should be a no-op with ExceptT and ==throwPartial with ErrorsT
