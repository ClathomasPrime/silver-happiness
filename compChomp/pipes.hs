import Control.Monad

--    up     down
-- a' <=      <= b' 
--    |   m   | 
-- a  =>  |   => b
--        v 
--        r

data Proxy a' a b' b m r
  = Request a' (a -> Proxy a' a b' b m r)
  | Response b (b' -> Proxy a' a b' b m r)
  | M (m (Proxy a' a b' b m r))
  | Pure r

instance Functor m => Functor (Proxy a' a b' b m) where
  fmap f (Request a' phi) = Request a' (fmap f . phi)
  fmap f (Response b phi) = Response b (fmap f . phi)
  fmap f (M m) = M . fmap (fmap f) $ m
  fmap f (Pure r) = Pure (f r)

instance Functor m => Applicative (Proxy a' a b' b m) where
  pure = Pure
  
  Request a' phi <*> u = Request a' (\a -> phi a <*> u)
  Response b phi <*> u = Response b (\b' -> phi b' <*> u)
  M m <*> u = M $ fmap (<*> u) m
  Pure f <*> u = fmap f u

instance Functor m => Monad (Proxy a' a b' b m) where
  return = pure

  Request a' phi >>= f = Request a' ((>>= f) . phi)
  Response b phi >>= f = Response b ((>>= f) . phi)
  M m >>= f = M $ fmap (>>= f) m
  Pure x >>= f = f x
