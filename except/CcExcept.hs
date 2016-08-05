{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module CcExcept
    ( Outer(Outer)
    , cc
    , ccAs
    , hand
    , E.MonadError(..)
    -- ^ Export the Monad and MonadError instances for ExceptT,
    --   but do not export ExceptT itself.
    ) where

import qualified Control.Monad.Except as E

-- all client functions should have return type `Outer e m`
data Outer e m a = Outer { fromOuter :: E.ExceptT e m a }

cc :: Outer e m a -> E.ExceptT e m a
cc = fromOuter

ccAs :: Functor m => (e -> f) -> Outer e m a -> E.ExceptT f m a
ccAs f u = E.ExceptT . fmap (mapLeft f) . E.runExceptT . fromOuter $ u
    where mapLeft :: (x -> y) -> Either x a -> Either y a
          mapLeft _ (Right a) = Right a
          mapLeft f (Left x) = Left (f x)

hand :: Functor m => Outer e m a -> E.ExceptT f m (Either e a)
hand u = E.ExceptT . fmap Right . E.runExceptT . fromOuter $ u

-- cannot use functions from Control.Monad.Except because the
-- type of the error changes
catchOuter :: Monad m => Outer e m a -> (e -> Outer f m a) -> Outer f m a
catchOuter u h = Outer . E.ExceptT 
    $ do u' <- E.runExceptT . fromOuter $ u
         case u' of
              Left e -> E.runExceptT . fromOuter . h $ e
              Right a -> return $ Right a
