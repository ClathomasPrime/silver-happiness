{-# LANGUAGE Rank2Types, TupleSections #-}

data Void

data Const a b = Const { getConst :: a }
instance Functor (Const a) where
  fmap _ (Const a) = Const a
instance Monoid a => Applicative (Const a) where
  pure _ = Const mempty
  Const u <*> Const v = Const $ u `mappend` v

data Identity a = Identity { runIdentity :: a }
instance Functor Identity where
  fmap f = Identity . f . runIdentity
instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity $ f a


type Lens s t a b 
  = forall f. Functor f 
  => (a -> f b) -> s -> f t

type Setter s t a b = (a -> Identity b) -> s -> Identity t

lens :: (s -> a) -> ((a -> b) -> s -> t) -> Lens s t a b
lens gt ovr = \phi s -> undefined -- flip ovr s <$> _

view :: Lens s t a b -> s -> a
view l s = getConst $ l Const s

over :: Setter s t a b -> (a -> b) -> s -> t
over l f s = runIdentity $ l (Identity . f) s

set :: Setter s t a b -> b -> s -> t
set l b = over l (const b)

_1 :: Lens (a,x) (b,x) a b
_1 phi (a,x) = (,x) <$> phi a

mapped :: Functor f => Setter (f a) (f b) a b
mapped phi s = Identity $ fmap (runIdentity . phi) s

-- both :: Lens s s a b -> Lens s s c d -> Lens s s (a,c) (b,d)
-- both l m phi s =  phi $ \(a,c) -> undefined
  -- let first a' = fst <$> phi (a',c)
  --     second c' = snd <$> phi (a,c')
  --  in undefined


-- type Lens s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
-- type Setter s t a b -- = (a -> Identity b) -> s -> Identity t
--   = forall f. Functor f => (a -> f b) -> s -> f t
-- getter requires Applicative? (applicative <= functor, getter <= setter)
-- type Getter s a -- = (a -> Const a a) -> s -> Const a s
--   = forall f. Applicative f => (a -> f a) -> s -> f s
-- view :: Lens s t a b -> s -> a
-- view l s = getConst $ l Const s
-- over :: Setter s t a b -> (a -> b) -> s -> t
-- over l f s = runIdentity $ l (Identity . f) s
-- _1 :: Lens (a,x) (b,x) a b
-- _1 phi (a,x) = (,x) <$> phi a
-- type MySetter s t a b = forall . (s -> Const a r) -> 
-- getTwo :: L.Getter s a -> L.Getter s b -> L.Getter s (a, b)
-- getTwo u v = undefined

class Profunctor p where
  promap :: (x -> a) -> (b -> y) -> p a b -> p x y

instance Profunctor (->) where
  promap f g h = g . h . f

data Indexed i a b 
  = Indexed { runIndexed :: i -> a -> b }

instance Profunctor (Indexed i) where
  promap f g (Indexed h) = Indexed $ \i -> g . h i . f
