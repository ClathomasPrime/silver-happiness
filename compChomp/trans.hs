import Control.Monad
import Control.Applicative

compJoin :: (Monad m, Traversable m, Applicative f) => (a -> f (m b)) -> m a -> f (m b)
compJoin f = fmap join . traverse f

redJoin :: (Monad n, Traversable n, Applicative f) => n (f (n b)) -> f (n b)
redJoin = fmap join . sequenceA


compBind :: (Monad m, Monad n, Traversable n) => (a -> m (n b)) -> m (n a) -> m (n b)
compBind f u = fmap join . join . fmap sequenceA . fmap (fmap f) $ u

compBind' :: (Monad m, Monad n, Traversable n) => (a -> m (n b)) -> m (n a) -> m (n b)
compBind' f u = join . fmap (fmap join . sequenceA . fmap f) $ u

compBind'' :: (Monad m, Monad n, Traversable n) => (a -> m (n b)) -> m (n a) -> m (n b)
compBind'' f u = join . fmap (fmap join) . fmap (traverse f) $ u


-- join is a natural transformation, so `join . fmap f === fmap f . join`
-- hmmmm not quite: join . fmap (fmap f) === fmap f . join

data Comp f g a = Comp { getComp :: f (g a) }

instance (Functor f, Functor g) => Functor (Comp f g) where
    fmap f = Comp . fmap (fmap f) . getComp

instance (Applicative f, Applicative g) => Applicative (Comp f g) where
    pure = Comp . pure . pure
    u <*> v = Comp ((<*>) <$> getComp u <*> getComp v)

-- instance (Monad f, Monad g) => Monad (Comp f g) where
--     return = pure
--     u >>= f = Comp (getComp u >>= (>>= getComp . f)) -- \ga -> return . f =<< ga

-- consider Comp 

instance (Monad m, Monad n, Traversable n) => Monad (Comp m n) where
    return = pure
    u >>= f = Comp . compBind (getComp . f) . getComp $ u



a :: (a -> IO (Maybe b)) -> Maybe a -> IO (Maybe b)
a f = fmap join . traverse f

b :: (a -> IO b) -> Maybe a -> IO (Maybe b)
b = traverse

c :: (a -> IO (Maybe b)) -> a -> IO (Maybe b)
c = ($)

d :: (a -> IO b) -> a -> IO b
d = ($)




-- folding though??

reduce :: t (f (t a)) -> f (t a)
reduce = undefined

reduceEither :: Applicative f => Either e (f (Either e a)) -> f (Either e a)
reduceEither (Left e) = pure (Left e)
reduceEither (Right u) = u

reduceList :: Applicative f => [f [a]] -> f [a]
reduceList [] = pure []
reduceList (x:xs) = (++) <$> x <*> reduceList xs

reduceList' :: Applicative f => [f [a]] -> f [a]
reduceList' = foldl (liftA2 (++)) (pure [])


reduceMix :: (Foldable t, Applicative f, Alternative s) => t (f (s a)) -> f (s a)
reduceMix = foldl (liftA2 (<|>)) (pure empty)
