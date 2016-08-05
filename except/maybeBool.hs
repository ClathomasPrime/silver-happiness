{-# LANGUAGE MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleInstances
           , DeriveFunctor
           , TupleSections
  #-}
-- Note: True == everything is good
--   False == something went wrong

type Partial a = Maybe (a, Bool)

markProblematic :: Partial a -> Partial a
markProblematic u = fmap (\(a,_) -> (a,False)) u

newtype PartialT m a
  = PartialT { runPartialT :: m (Partial a) }
  deriving(Functor)

instance Applicative f => Applicative (PartialT f) where
  pure = PartialT . pure . Just . (,True)
  PartialT u <*> PartialT v = PartialT (phi <$> u <*> v)
    where phi Nothing _ = Nothing
          phi _ Nothing = Nothing
          phi (Just (f, b)) (Just (a, b')) = Just (f a, b && b')
instance Monad m => Monad (PartialT m) where
  return = pure
  PartialT u >>= f = PartialT
    $ do u' <- u
         case u' of
              Nothing -> return Nothing
              Just (a, True) -> runPartialT $ f a
              Just (a, False) -> fmap markProblematic (runPartialT $ f a)

-- state machine:
--
--           successful
--     ignore /     \ manhandle
--           /       \ 
--          /         \
--  issues /           \   failure
--      issues -------- failed 
--        failure



failure :: Applicative m => PartialT m a
failure = PartialT . pure $ Nothing

issues :: Applicative m => PartialT m ()
issues = PartialT . pure $ Just ((), False)

ignore :: Functor m => PartialT m a -> PartialT m a
ignore u = PartialT . fmap phi . runPartialT $ u
    where phi Nothing = Nothing
          phi (Just (a,_)) = Just (a, True)

manhandle :: Monad m => PartialT m a -> PartialT m (Partial a)
manhandle u = PartialT . fmap (Just . (,True)) . runPartialT $ u
