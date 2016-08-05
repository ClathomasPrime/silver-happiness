
instance Functor ((->) r) where
    fmap f u = f . u

instance Applicative ((->) r) where
    pure = const
    phi <*> theta = \r -> (phi r) (theta r)

instance Monad ((->) r) where
    return = pure
    phi >>= f = \r -> f (phi r) r
