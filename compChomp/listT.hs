

data MList' m a = MNil | MCons a (MList m a)
type MList m a = m (MList' m a)

collapse :: Monad m => MList m a -> m [a]
collapse u = u >>= phi
    where phi MNil = return []
          phi (MCons a u') = (a:) <$> collapse u'

instance Applicative m => Monoid (ListT m a) where
    mempty = ListT . pure $ MNil
    mappend u v = ListT $ concatMList (runListT u) (runListT v)
concatMList :: Applicative m => MList m a -> MList m a -> MList m a
concatMList u v = phi <$> u <*> v
    where phi MNil x = x
          phi (MCons a u') _ = MCons a (concatMList u' v)


newtype ListT m a = ListT { runListT :: MList m a }


instance Functor m => Functor (ListT m) where
    fmap f u = ListT . mapMList f . runListT $ u
mapMList :: Functor m => (a -> b) -> MList m a -> MList m b
mapMList f = fmap phi
    where phi MNil = MNil
          phi (MCons a u) = MCons (f a) (mapMList f u)


instance Monad m => Applicative (ListT m) where
    pure a = ListT . pure $ MCons a (pure MNil)
    u <*> v = ListT $ apMList (runListT u) (runListT v)
apMList :: Monad m => MList m (a -> b) -> MList m a -> MList m b
apMList u v = u >>= phi
    where phi (MCons f u') = mapMList f v `concatMList` apMList u' v
          phi MNil = pure MNil

-- | As for zip lists:
apMListZip :: Applicative m => MList m (a -> b) -> MList m a -> MList m b
apMListZip u v = phi <$> u <*> v
    where phi (MCons f u') (MCons a v') = MCons (f a) (apMListZip u' v')
          phi _ _ = MNil

instance Monad m => Monad (ListT m) where
    return = pure
    u >>= f = ListT $ bindMList (runListT u) (runListT . f)
bindMList :: Monad m => MList m a -> (a -> MList m b) -> MList m b
bindMList u f = u >>= phi
    where phi MNil = return MNil
          phi (MCons a u') = f a `concatMList` bindMList u' f
          

contents :: ListT IO String
contents = ListT 
    $ do s <- getLine 
         case s of
              "" -> return MNil
              s -> return $ MCons s (runListT contents)

