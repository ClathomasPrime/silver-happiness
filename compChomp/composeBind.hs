
import Prelude hiding(Foldable(..))
import Control.Monad
import qualified GHC.IO as IO

-- data Any = Any { getAny :: Bool }
-- instance Monoid Any where
--     mempty = Any False
--     Any u `mappend` Any v = Any $ u || v
-- data Add = Add { getAdd :: Int }
-- instance Monoid Add where
--     mempty = Add 0
--     Add u `mappend` Add v = Add $ u + v
    -- null :: t a -> Bool
    -- null = getAny . foldMap (const $ Any True)
    -- length :: t a -> Int
    -- length = getAdd . foldMap (const $ Add 1)

class Foldable t where
    foldMap :: Monoid m => (a -> m) -> t a -> m
instance Foldable Maybe where
    foldMap _ Nothing = mempty
    foldMap f (Just a) = f a
instance Foldable (Either e) where
    foldMap _ (Left _) = mempty
    foldMap f (Right a) = f a
instance Foldable [] where
    foldMap _ [] = mempty
    foldMap f (a:as) = f a `mappend` foldMap f as


-- class Applicative f => Alternative f where
--     empty :: f a
--     (<|>) :: f a -> f a -> f a
-- instance Alternative Maybe where
--     empty = Nothing
--     Nothing <|> u = u 
--     Just v <|> _ = Just v
-- instance Alternative [] where
--     empty = []
--     (<|>) = (++)
-- instance Alternative IO where
--     empty = fail "empty"
--     m <|> n = m `catch` const n
--         where catch = IO.catchException :: IO a -> (IOError -> IO a) -> IO a

-- newtype Apply f a = Apply { getApply :: f a }

-- extremely literal translation of Alternative's functions to the land of monoids
-- instance Alternative f => Monoid (Apply f a) where
--     mempty = Apply $ empty
--     Apply u `mappend` Apply v = Apply (u <|> v)

newtype Myply f a = Myply { getMyply :: f a }

-- A way to build a monoid that IS NOT natural/universal in m
instance (Monoid m, Applicative f) => Monoid (Myply f m) where
    mempty = Myply $ pure mempty
    Myply u `mappend` Myply v = Myply (mappend <$> u <*> v)


redMay :: Applicative f => Maybe (f (Maybe a)) -> f (Maybe a)
redMay Nothing = pure Nothing
redMay (Just u) = u

-- | NOT THE RIGHT MONOID INSTANCE FOR MAYBE
-- redMay'' :: (Alternative f, Monoid a) => Maybe (f (Maybe a)) -> f (Maybe a)
-- redMay'' = getMyply . foldMap Myply

redEither :: Applicative f => Either e (f (Either e a)) -> f (Either e a)
redEither (Left e) = pure $ Left e
redEither (Right u) = u

redList :: Applicative f => [f [a]] -> f [a]
redList [] = pure []
redList (u:us) = (++) <$> u <*> redList us



-- problem: it is not reasonable for f to have to be an Alternative
-- red :: (Alternative f, Foldable t) => t (f (t a)) -> f (t a)
-- red = getApply . foldMap Apply
-- possible solution: use Myply instead of Apply

-- this is litterally just sequence...
-- red0 :: (Applicative f, Foldable t, Monoid u) => t (f u) -> f (t u)
-- red0 = sequenceA

red' :: (Applicative f, Foldable t, Monoid (t a)) => t (f (t a)) -> f (t a)
red' = getMyply . foldMap Myply

-- These constraints are reasonable
red'' :: (Applicative f, Foldable t, Monoid m) => t (f m) -> f m
red'' = getMyply . foldMap Myply
-- And the signature seems to imply that the "inner container type" is independent of the "outer container"



-- comp :: (Functor t, Foldable t, Alternative f) => (a -> f (t b)) -> t a -> f (t b)
-- comp phi u = red . fmap phi $ u

-- This version is very reasonable!
-- These are probably the correct versions:
compJoin :: (Monad m, Traversable m, Applicative f) => (a -> f (m b)) -> m a -> f (m b)
compJoin f = fmap join . traverse f

redJoin :: (Monad m, Traversable m, Applicative f) => m (f (m b)) -> f (m b)
redJoin = fmap join . sequenceA




