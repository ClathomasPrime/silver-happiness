{-# LANGUAGE Rank2Types #-}

data Void

--    |-----|
-- a =>  m  => b
--    |- | -|
--       v
--       r

data Pipe a b m r 
  = Request (a -> Pipe a b m r)
  | Response b (Pipe a b m r)
  | M (m (Pipe a b m r))
  | Pure r

type Producer b m r = Pipe () b m r

type Consumer a m r = Pipe a Void m r

type Effect m r = Pipe () Void m r

runEffect :: Monad m => Effect m r -> m r
runEffect (Request phi) = runEffect $ phi () -- ^ probably not encouraged?
runEffect (Response _ _) = undefined -- ^ b :: Void
runEffect (M m) = m >>= runEffect
runEffect (Pure r) = pure r

lift :: Functor m => m r -> Pipe a b m r
lift m = M (fmap Pure m)

instance Functor m => Functor (Pipe a b m) where
  fmap f (Request phi) = Request $ fmap f . phi
  fmap f (Response b p) = Response b (fmap f p)
  fmap f (M m) = M $ fmap (fmap f) m
  fmap f (Pure r) = Pure (f r)

instance Functor m => Applicative (Pipe a b m) where
  pure = Pure

  Request phi <*> u = Request $ (<*> u) . phi
  Response b p <*> u = Response b (p <*> u)
  M m <*> u = M $ fmap (<*> u) m
  Pure f <*> u = fmap f u

  -- a *> b = flip const <$> a <*> b
  Request phi *> u = Request $ (*> u) . phi
  Response b p *> u = Response b (p *> u)
  M m *> u = M $ fmap (*> u) m
  Pure _ *> u = u

instance Functor m => Monad (Pipe a b m) where
  return = Pure

  Request phi >>= f = Request $ (>>= f) . phi
  Response b p >>= f = Response b (p >>= f)
  M m >>= f = M $ fmap (>>= f) m
  Pure a >>= f = f a

each :: [a] -> Producer a m ()
each = foldr Response (Pure ())

-- = yield category:

yield :: b -> Pipe a b m ()
yield b = Response b (Pure ())

-- | "into"
(~>) :: Applicative m 
  => (a -> Producer b m ())
  -> (b -> Producer c m ())
  -> a -> Producer c m ()
f ~> g = \a -> for (f a) g
-- f ~> g = \a -> 
--   case f a of
--        Request phi -> 

for :: Applicative m => Producer a m r -> (a -> Producer b m ()) -> Producer b m r
for (Request phi) f = for (phi ()) f
for (Response a pipe) f = f a *> for pipe f
for (M m) f = M $ fmap (\p -> for p f) m
for (Pure r) _ = Pure r


-- = await category: 

await :: Consumer a m a
await = Request Pure

-- | "feed"
(>~) :: Functor m => Effect m b -> Consumer b m c -> Effect m c
Request phi >~ u = Request $ (>~ u) . phi
Response b _ >~ _ = undefined -- ^ b :: Void
M m >~ u = M $ fmap (>~ u) m
Pure b >~ u = undefined

stdIn :: Producer String IO Int
stdIn = 
  do line <- lift getLine
     if line == "" 
       then return 4
       else do yield line
               stdIn


(>->) :: Functor m => Pipe a b m r -> Pipe b c m r -> Pipe a c m r
pipe >-> Request phi = undefined
pipe >-> Response c u =  undefined
-- Request phi >-> u = Request $ (>-> u) . phi
-- 
-- Response b >-> Request phi = Response b >-> phi b
-- Response b >-> Response c = Response c
-- Response b >-> M m = M $ fmap (Response b >->) m
-- Response b >-> Pure r = Pure r
-- 
-- M m >-> u = M $ fmap (>-> u) m
-- Pure r >-> _ = Pure r


