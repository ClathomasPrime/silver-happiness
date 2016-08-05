{-# LANGUAGE MultiParamTypeClasses, RankNTypes, DeriveFunctor, FlexibleInstances, TypeFamilies #-}
import Control.Applicative
import Control.Monad

-- ErrorOf :: (* -> * -> *) -> * -> Constraint

-- Ideal: class (ErrorOf m e => Monad (m e)) => MonadExcept m where
-- not quite sure what it should be instead...
class Monad (m e) => MonadExcept m e where
    type family ErrorOf m e :: *
    throw :: forall a e. ErrorOf m e -> m e a
    catch :: forall a e f. m e a -> (ErrorOf m e -> m f a) -> m f a


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

-- instance Monad m => ErrorOf (ExceptT m) e

instance Monad m => MonadExcept (ExceptT m) e where
    type ErrorOf (ExceptT m) e = e
    throw = ExceptT . return . Left
    catch u h = ExceptT 
        $ do u' <- runExceptT u
             case u' of
                  Right a -> return $ Right a
                  Left e -> runExceptT $ h e

