import Prelude hiding(Foldable(..))
import Control.Monad
import qualified GHC.IO as IO

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
-- IO is definitely NOT a Foldable


class Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a
instance Alternative Maybe where
    empty = Nothing
    Nothing <|> u = u 
    Just v <|> _ = Just v
instance Alternative [] where
    empty = []
    (<|>) = (++)
-- IO is probably not a very sensible Foldable...
instance Alternative IO where
    empty = fail "empty"
    m <|> n = m `catch` const n
        where catch = IO.catchException :: IO a -> (IOError -> IO a) -> IO a


newtype Apply f a = Apply { getApply :: f a }
-- extremely literal translation of Alternative's functions to the land of monoids
instance Alternative f => Monoid (Apply f a) where
    mempty = Apply $ empty
    Apply u `mappend` Apply v = Apply (u <|> v)

redMay :: Applicative f => Maybe (f (Maybe a)) -> f (Maybe a)
redMay Nothing = pure Nothing
redMay (Just u) = u

-- problem: these are also definitely not the same. For example, 
-- redMay Nothing :: IO (Maybe a) is just return Nothing, but
-- redMay' Nothing :: IO (Maybe a) is an exception
redMay' :: Alternative f => Maybe (f (Maybe a)) -> f (Maybe a)
redMay' = getApply . foldMap Apply

redEither :: Applicative f => Either e (f (Either e a)) -> f (Either e a)
redEither (Left e) = pure $ Left e
redEither (Right u) = u

redEither' :: Alternative f => Either e (f (Either e a)) -> f (Either e a)
redEither' = getApply . foldMap Apply

redList :: Applicative f => [f [a]] -> f [a]
redList [] = pure []
redList (u:us) = (++) <$> u <*> redList us

-- problem: these are definitely not the same. The first uses
-- the fact that [a] is a monoid and f is an Applicative. 
-- The second uses f's abilities as an alternative
redList' :: Alternative f => [f [a]] -> f [a]
redList' = getApply . foldMap Apply



-- problem: it is not reasonable for f to have to be an Alternative
red :: (Alternative f, Foldable t) => t (f (t a)) -> f (t a)
red = getApply . foldMap Apply
-- possible solution: use Myply instead of Apply

comp :: (Functor t, Foldable t, Alternative f) => (a -> f (t b)) -> t a -> f (t b)
comp phi u = red . fmap phi $ u


listToRed :: [IO [String]]
listToRed =
    [ return ["oh"]
    , return ["oh", "ohoh"]
    , return ["oh", "ohoh", "oh oh"]
    ]
    -- [ fmap ((++["!"]) . lines) getContents
    -- , return ["oh", "ohoh", "oh oh"]
    -- , fmap (:[]) getLine
    -- ]

maybeToRed :: Maybe (IO (Maybe Int))
maybeToRed = Just . return . Just $ 2



guard :: Alternative f => Bool -> f ()
guard True = pure ()
guard False = empty

optional :: Alternative f => f a -> f (Maybe a)
optional u = fmap Just u <|> pure Nothing
