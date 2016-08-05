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



-- This is an attempt to implement the idea "Embed the distinction between errors and warnings better" idea

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
                  -- (e, Just a) 
                  --   -> fmap (\(e',b) -> (e' `mappend` e, b)) (runErrorsT (f a))
                  Errors e -> return $ Errors e

instance (Monoid e, Monad m) => ErrorOf (ErrorsT m) (Partial e a)
--  Could not deduce (Monoid (Partial e a))
--    arising from the superclasses of an instance declaration
--  from the context (Monoid e, Monad m)
--    bound by the instance declaration at exceptAggroPartial.hs:48:10-65
--  In the instance declaration for ‘ErrorOf (ErrorsT m) (Partial e a)’
    -- It's trying to verify that there is an instance `Monad (ErrorsT m (Partial e a))`
    -- for which it would need a monoid instance for `Partial e a`, as dictated by
    -- instance (Monoid e, Monad m) => Monad (ErrorsT m e) where

instance Monad m => MonadExcept (ErrorsT m) where
    throw = ErrorsT . return . Errors
    catch u h = ErrorsT 
        $ do u' <- runErrorsT u
             case u' of
                  Success a -> return $ Success a
                  Warnings e a -> undefined -- return $ ({-mempty-}undefined, Just a)
                  Errors e -> runErrorsT $ h e
