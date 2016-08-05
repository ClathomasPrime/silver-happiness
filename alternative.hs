
class Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a

guard :: Alternative f => Bool -> f ()
guard True = pure ()
guard False = empty

optional :: Alternative f => f a -> f (Maybe a)
optional u = fmap Just u <|> pure Nothing
