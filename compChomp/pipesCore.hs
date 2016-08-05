module PipesCore where

data Pipe a b m r
    = Request (a -> Pipe a b m r)
    | Respond b (Pipe a b m r)
    | M (m (Pipe a b m r))
    | Pure r

{-| Functor, Applicative, Monad instances. These instances don't do
    a whole lot, as Pipe is a somewhat like a free monad. 
    They just maintain the shape of the computation.
-}
instance Monad m => Functor (Pipe a b m) where
    fmap f (Request fa) = Request (fmap f . fa)
    fmap f (Respond b p) = Respond b (fmap f p)
    fmap f (M m) = M $ fmap (fmap f) m
    fmap f (Pure r) = Pure (f r)

instance Monad m => Applicative (Pipe a b m) where
    pure      = Pure
    
    Request phi <*> px = Request $ (<*> px) . phi
    Respond b p <*> px = Respond b (p <*> px)
    M m <*> px = M $ fmap (<*> px) m
    Pure f <*> px = fmap f px

instance Monad m => Monad (Pipe a b m) where
    return = pure

    Request phi >>= f = Request $ (>>= f) . phi
    Respond b p >>= f = Respond b (p >>= f)
    M m >>= f = M $ fmap (>>= f) m
    Pure r >>= f = f r

newtype X = X X
-- | Use 'closed' to \"handle\" impossible outputs
closed :: X -> a
closed (X x) = closed x


-- | An effect in the base monad
type Effect = Pipe () X

-- | 'Producer's can only 'Pipes.yield'
type Producer b = Pipe () b

-- | 'Pipe's can both 'Pipes.await' and 'Pipes.yield'
type Pipe a b = Pipe a b

-- | 'Consumer's can only 'Pipes.await'
type Consumer a = Pipe a X


-- | Run a self-contained 'Effect', converting it back to the base monad
runEffect :: Monad m => Effect m r -> m r
runEffect (Request phi) = runEffect (phi ()) -- ^ probably shouldn't fire
runEffect (Respond b _) = closed b -- b :: X
runEffect (M m) = m >>= runEffect
runEffect (Pure r) = pure r


{-| Send a value of type @a@ downstream and provide a default 
    computation to keep the pipe churning 
-}
respond :: Monad m => a -> Pipe x a m ()
respond a = Respond a (Pure ())

{-| @for p f@ replaces every @yield b@ in @p@ with @f b@ (ignoreing @f b@'s returns)
-}
for :: Monad m => Pipe x b m r -> (b -> Pipe x c m s) -> Pipe x c m r
for (Request phi) fb = Request (flip for fb . phi)
for (Respond b p) fb = fb b *> (for p fb)
for (M m) fb = M $ fmap (flip for fb) m
for (Pure r) _ = Pure r


{-| Send a value of type @a'@ upstream and block waiting for a reply of type @a@
    'request' is the identity of the request category. -}
request :: Monad m => Pipe a b m a
request = Request Pure

{-| @(u >>~ v)@ replaces each 'request' in @v@ with a return value from @u@. -}
(>>~) :: Monad m => Pipe a y m b -> Pipe b y m c -> Pipe a y m c
p >>~ Request phi = p >>= (p >>~) . phi 
p >>~ Respond y p' = Respond y (p >>~ p')
p >>~ M m = M $ fmap (p >>~) m
p >>~ Pure c = Pure c

push :: Monad m => a -> Pipe a a m r
push a = Respond a (Request push)

{-| Compose two proxies blocked while 'request'ing data, creating a new proxy
    blocked while 'request'ing data
    @(p >>~ f)@ pairs each 'respond' in @p@ with a 'request' in @f@.
    Point-ful version of ('>~>')
-}
(>>~) :: Monad m => Pipe a b m r -> (b -> Pipe b c m r) -> Pipe a c m r
Request fa >>~ fb = Request $ (>>~ fb) . fa
Respond b p >>~ fb = p +>> fb b
M m >>~ fb = M $ fmap (>>~ fb) m
Pure r >>~ _ = Pure r

{-| Forward requests followed by responses:
@ 'pull' = 'request' 'Control.Monad.>=>' 'respond' 'Control.Monad.>=>' 'pull' @
-}
pull :: Monad m => Pipe a a m r
pull = Request (\a -> Respond a pull)

{-| Compose two proxies blocked in the middle of 'respond'ing, creating a new
    proxy blocked in the middle of 'respond'ing

    @(f +>> p)@ pairs each 'request' in @p@ with a 'respond' in @f@.
    Point-ful version of ('>+>')
-}
(+>>) :: Monad m => Pipe a b m r -> Pipe b c m r -> Pipe a c m r
p +>> Request fb = p >>~ fb
p +>> Respond c p' = Respond c (p +>> p')
p +>> M m = M $ fmap (p +>>) m
p +>> Pure r = Pure r
