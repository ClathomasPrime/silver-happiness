{-# LANGUAGE MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleInstances
           , DeriveFunctor
  #-}
import Prelude

ap :: Monad m => m (a -> b) -> m a -> m b
ap u v = do { f <- u; a <- v; return $ f a }

data ExceptT e m a
  = ExceptT { runExceptT :: m (Either e a) }
  deriving(Functor)
instance Applicative m => Applicative (ExceptT e m) where
  pure = ExceptT . pure . Right
  -- NOTE: this implementation will perform the effects of both arguments
  -- (allowing parrallel computation, but possibly not what you want)
  ExceptT u <*> ExceptT v = ExceptT (phi <$> u <*> v)
    where phi (Left e) (_) = Left e
          phi (Right _) (Left e) = Left e
          phi (Right f) (Right a) = Right . f $ a
instance Monad m => Monad (ExceptT e m) where
  return = pure
  ExceptT u >>= f = ExceptT
    $ do u' <- u
         case u' of
              Left e -> return $ Left e
              Right a -> runExceptT (f a)

class Monad m => MonadExcept e m | m -> e where
  throw :: e -> m a
  catch :: m a -> (e -> m a) -> m a

-- instance Monad m => MonadExcept e (ExceptT e m) where
--   throw = ExceptT . return . Left
--   u `catch` h = ExceptT
--     $ do v <- runExceptT u
--          case v of
--               Left e -> runExceptT (h e)
--               Right a -> return . Right $ a

-- Sometimes functions may produce useful default values
--   (e.g. ocScore could default to 0).
-- Other times they may not (e.g. userId has no reasonable default).
-- For computations of the second type, the answer is always either
--   required, or custom logic is required to surmount a missing answer.
-- For computations of the first type, sometimes we want to guaruntee
--   that we get an answer. Other times we'd be okay with the default.

  -- Origional representation: m (Maybe e, Maybe a)...
  -- (Nothing, Just a) :: Success
  -- (Just e, Just a) :: Partial Failure
  -- (Just e, Nothing) :: Total Failure
  -- (Nothing, Nothing) is probably not a reasonable state
  --   * one issue with this representation
  -- Also, multiple errors should probably be possible



-- data Partial e a
--   = Success a
--   | PartialFailure [e] a
--   | Failure [e] e
--   deriving(Functor)
--
-- data PartialFailureT e m a
--   = PartialFailureT { runPartialFailureT :: m (Partial e a) }
--   deriving(Functor)
-- instance Monad m => Applicative (PartialFailureT e m) where
--   pure = return
--   (<*>) = ap
-- instance Monad m => Monad (PartialFailureT e m) where
--   return = PartialFailureT . return . Success
--   PartialFailureT u >>= f = PartialFailureT $
--     do u' <- u
--        case u' of
--             Success a -> runPartialFailureT $ f a
--             PartialFailure es a ->
--               do v <- runPartialFailureT $ f a
--                  return $ case v of
--                       Success b -> PartialFailure es b
--                       PartialFailure es' b -> PartialFailure (es' ++ es) b
--                       Failure es' e -> Failure (es' ++ es) e
--             Failure es e -> return $ Failure es e
--
--
--
-- class Monad m => PartialFailure e m | m -> e where
--   -- addFailure :: e -> m a -- ^ I don't think this is possible to implement
--   addFailure :: e -> m ()
--   throwTotal :: e -> m a
--   catchTotal :: m a -> (e -> m a) -> m a
--
--   -- the computation must succeed, or it is aborted
--   require :: m a -> m a
--   -- we want to
--   manhandle :: m a -> m (Either e a)
--
-- instance Monad m => PartialFailure e (PartialFailureT e m) where
--   addFailure e = PartialFailureT . return $ PartialFailure [e] ()
--   throwTotal e = PartialFailureT . return $ Failure [] e

-- data NonEmptyList a
--   = Head a
--   | Cons (NonEmptyList a)

data NonEmptyList a = NonEmptyList
  { safeHead :: a
  , safeTail :: [a]
  } deriving(Eq, Show, Functor)

toList :: NonEmptyList a -> [a]
toList (NonEmptyList a as) = a:as

performNonEmpty :: ([a] -> b) -> NonEmptyList a -> b
performNonEmpty f = f . toList

cons :: a -> NonEmptyList a -> NonEmptyList a
cons a (NonEmptyList h t) = NonEmptyList a (h:t)

(<>) :: NonEmptyList a -> NonEmptyList a -> NonEmptyList a
NonEmptyList a as <> NonEmptyList b bs
  = NonEmptyList a (as ++ (b:bs))

data Partial e a
  = Success a
  | PartialFailure (NonEmptyList e) a
  | Failure (NonEmptyList e)
  deriving(Functor)

prependErrors :: NonEmptyList e -> Partial e a -> Partial e a
prependErrors es (Success a) = PartialFailure es a
prependErrors es (PartialFailure es' a) = PartialFailure (es' <> es) a
prependErrors es (Failure es') = Failure (es' <> es)

data PartialFailureT e m a
  = PartialFailureT { runPartialFailureT :: m (Partial e a) }
  deriving(Functor)
instance Monad m => Applicative (PartialFailureT e m) where
  pure = return
  (<*>) = ap
instance Monad m => Monad (PartialFailureT e m) where
  return = PartialFailureT . return . Success
  PartialFailureT u >>= f = PartialFailureT $
    do u' <- u
       case u' of
            Success a -> runPartialFailureT $ f a
            PartialFailure es a ->
              do v <- runPartialFailureT $ f a
                 return $ case v of
                      Success b -> PartialFailure es b
                      PartialFailure es' b -> PartialFailure (es' <> es) b
                      Failure es' -> Failure (es' <> es)
            Failure es -> return $ Failure es



class Monad m => PartialFailure e m | m -> e where
  -- addFailure :: e -> m a -- ^ I don't think this is possible to implement
  addFailure :: e -> m ()
  throwTotal :: e -> m a
  catchTotal :: m a -> (e -> m a) -> m a
  catchPartial :: m a -> (e -> m ()) -> m a
  -- | we will almost certainly need something of this functionality:
  -- catchPartial :: m a -> ([e] -> m ()) -> m a

  -- the computation must succeed, or it is aborted
  require :: m a -> m a
  -- maybe optional is implicit...
  optional :: m a -> m a
  -- we want full access to the errors
  clearErrors :: m a -> m (Partial e a)
  getErrors :: m a -> m (Partial e a)
  -- (we may need something even more expressive allowing
  --  clients to REMOVE errors selectively)

addFailures :: PartialFailure e m => [e] -> m ()
addFailures es = sequence_ . fmap addFailure $ es



instance Monad m => PartialFailure e (PartialFailureT e m) where
  addFailure e = PartialFailureT . return $ PartialFailure (NonEmptyList e []) ()
  throwTotal e = PartialFailureT . return $ Failure (NonEmptyList e [])

  catchTotal u h = PartialFailureT 
    $ do u' <- runPartialFailureT u
         case u' of 
              Success a -> return $ Success a
              PartialFailure e a -> return $ PartialFailure e a
              -- | this is pretty strange behavior, for the following
              --   reason: if a function throws several errors, there
              --   may be no way of knowing how may, and thus no 
              --   way of getting back to a Success value...
              Failure (NonEmptyList e []) -> runPartialFailureT $ h e
              Failure (NonEmptyList e (r:rs)) 
                -> let es = NonEmptyList r rs
                    in fmap (prependErrors es) (runPartialFailureT $ h e)
  
  catchPartial u h = PartialFailureT
    $ do u' <- runPartialFailureT u
         case u' of
              Success a -> return $ Success a
              PartialFailure (NonEmptyList e []) a 
                -> runPartialFailureT (h e) >> return (Success a)
              PartialFailure (NonEmptyList e (r:rs)) a
                -> let es = NonEmptyList r rs
                    in fmap (prependErrors es) (runPartialFailureT $ h e) 
                        >> return (Success a)
              Failure es -> return $ Failure es

  require u = PartialFailureT
    $ do u' <- runPartialFailureT u
         return 
           $ case u' of
                  Success a -> Success a
                  -- hmm loosing information... somehow it feels okay though.
                  -- Like throw, it seems clear that this should disrupt the computation
                  PartialFailure es _ -> Failure es
                  Failure es -> Failure es

  -- This is also strange behavior... wasn't the whole
  -- point of partial failure that it was okay for the 
  -- error to be there?
  optional u = PartialFailureT
    $ do u' <- runPartialFailureT u
         return 
           $ case u' of
                  Success a -> Success a
                  PartialFailure _ a -> Success a
                  Failure es -> Failure es

  clearErrors u = PartialFailureT 
    $ do u' <- runPartialFailureT u
         return . Success $ u'
           -- $ case u' of
           --        Success a -> Success (Success a)
           --        PartialFailure -> 

  getErrors u = PartialFailureT 
    $ do u' <- runPartialFailureT u
         return 
           $ case u' of
                  Success a -> Success (Success a)
                  PartialFailure es a -> PartialFailure es (PartialFailure es a)
                  -- erm.... probably not this... it won't allow you to manhandle total failures
                  -- Failure es -> Failure es
                  -- probably this
                  Failure es -> PartialFailure es (Failure es)




