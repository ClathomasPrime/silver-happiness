module ADT where
import List (List(..))

import Prelude hiding(filter, length, filter, zip, null, iterate, take, map, fromInteger,
    toInteger, Functor(..), fst, snd, Either(..), either )

-- = Algebraic data types

-- In the preceding section we have given specic notations for some recursion patterns in connection
-- with the particular type of cons-lists. In order to dene the notions of cata-, ana-, hyloand
-- paramorphism for arbitrary data types, we now present a generic theory of data types and
-- functions on them. For this we consider a recursive data type (also called `algebraic' data type
-- in Miranda) to be dened as the least xed point of a functor


-- == Functors

-- A bifunctor y is a binary operation taking types into types and functions into functions such
-- that if f 2 A ! B and g 2 C ! D then f y g 2 A y C ! B y D, and which preserves identities
-- and composition:

class Bifunctor p where
    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d

-- prop> bimap id id === id
-- prop> bimap f g . bimap h j = bimap (f . h) (h . j)

-- A monofunctor is a unary type operation F, which is also an operation on functions, F 2 (A !
-- B) ! (AF ! BF) that preserves the identity and composition.

class Functor f where
    fmap :: (a -> b) -> f a -> f b

-- prop> fmap id === id
-- prop> fmap f . fmap g = fmap (f . g)

-- In x5 we will show that List is a functor.
-- The data types found in all current functional languages can be dened by using the following
-- basic functors.

-- === Products

-- The (lazy) product DkD0 of two types D and D0
-- and its operation k on functions are dened as:

data Product a b = Product a b

-- We shall use the built in Haskell type (,) for this notion:

instance Bifunctor (,) where
    bimap f g (a,c) = (f a, g c)

-- Closely related to the functor k are the projection and tupling combinators:

fst :: (a, b) -> a
fst (a, b) = a

snd :: (a, b) -> b
snd (a, b) = b

both :: (a -> b) -> (a -> c) -> a -> (b,c)
both f g = \a -> (f a, g a)

-- Using ;   and 4 we can express

bimap' :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
bimap' f g = both (f . fst) (g . snd)

-- We can also define @both@ using @bimap@ and the doubling combinator  

doub :: a -> (a, a)
doub a = (a, a)

-- as

both' :: (a -> b) -> (a -> c) -> a -> (b, c)
both' f g = bimap f g . doub


-- === Sum

-- The sum of two types @a@ and @b@ is defined as:

data Either a b = Left a | Right b

instance Bifunctor Either where
    bimap f _ (Left a) = Left (f a)
    bimap _ g (Right b) = Right (g b)

-- @Left :: a -> Either a b@ and @Right :: b -> Either a b@ serve to inject values into 
-- the sum type. Closely related is the either combinator which allows you to remove
-- elements from the sum type:

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left a) = f a
either _ g (Right b) = g b


-- === Function

-- The operation ! that forms the function space D ! D0
-- of continuous functions from
-- D to D0
-- , has as action on functions the `wrapping' function

class Profunctor p where
    promap :: (a -> b) -> (c -> d) -> p b c -> p a d

instance Profunctor (->) where
    -- | promap :: (a -> b) -> (c -> d) -> (b -> c) -> a -> d
    promap f g h = g . h . f 

-- We define some combinators

(<--) :: (c -> d) -> (a -> b) -> (b -> c) -> a -> d
(<--) = flip promap

-- So that @(f <-- g) h === f . h . g@

(<-*-) :: Functor f => (f c -> d) -> (a -> f b) -> (b -> c) -> a -> d
(f <-*- g) h = f . fmap h . g

-- wraps its Functor-boxed argument between f and g.

-- Closely related are the combinators

curry :: ((a,b) -> c) -> a -> b -> c
curry f a b = f (a,b)

uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry f (a,b) = f a b

eval :: (a -> b, a) -> b
eval (f, a) = f a

-- Note that promap is contra-variant in its first argument, i.e. 
--
-- prop> promap f g . promap h j === promap (h . f) (g . j)
--
-- Proof: (promap f g . promap h j) k 
--          === promap f g (j . k . h)
--          === g . j . k . h . f
--          === promap (h . f) (g . j) k


-- === Identity, Constants 

-- The identity functor is defined as

newtype Indentity a = Identity a

instance Functor Indentity where
    fmap f (Identity a) = Identity (f a)


-- Any type x induces a functor @Const x@ which ignores its input type

data Const a b = Const a

instance Functor (Const x) where
    fmap _ (Const x) = Const x


-- === Lifting 

-- For mono-functors f g and bifunctor p we define the monofunctors @Compose f g@ and 
-- @Lift p f g@ by

data Compose f g a = Compose (f (g a))

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap u (Compose x) = Compose (fmap (fmap u) x)

data Lift p f g a = Lift (p (f a) (g a))

instance (Bifunctor p, Functor f, Functor g) => Functor (Lift p f g) where
    fmap u (Lift x) = Lift (bimap (fmap u) (fmap u) x)


-- === Sectioning 

-- Analogous to the sectioning of binary operators, (a) b = a  b and (b) a =
-- a  b we dene sectioning of bi-functors y;

-- hence B(Ay) = A y B and f(Ay) = id y f. Similarly we can dene sectioning of y in its second
-- argument, i.e. (yB) and (yf).
-- It is not too dicult to verify the following two properties of sectioned functors:

-- === Laws for the basic combinators

-- Relating to products: 
--
-- prop> fst . bimap f g === f . fst  ;  snd . bimap f g === g . snd
-- prop> fst . both f g === f  ;  snd . both f g === g
-- prop> both (fst . h) (snd . h) === h :: a -> (b, c)
-- prop> both fst snd === id :: (a, b) -> (a, b)
-- 
-- Relating to sums:
--
-- prop> bimap f g . Left === Left . f  ;  bimap f g . Right === Right . f
-- prop> either f g . Left === f  ;  either f g . Right === g
-- prop> either (h . Left) (h . Right) === h :: Either a b -> c
-- prop> either Left Right === id :: Either a b -> Either a b
--
-- A nice law relating @either@ and @both@ is the abides law:
--
-- prop> either (both f g) (both h j) = both (either f h) (either g j)
-- Proof:
--   either (both f g) (both h j) (Left a) = both f g a = (f a, g a)
--     = (either f h $ Left a, either g j $ Left a)
--     = both (either f h) (either g j) (Left a)
--   either (both f g) (both h j) (Right b) = both h j b = (h b, j b)
--     = (either f h $ Right b, either g j $ Right b)
--     = both (either f h) (either g j) (Right b)
--


-- === Varia

-- The one element type is denoted 1 and can be used to model constants of type A by nullary
-- functions of type 1 ! A. The only member of 1 called void is denoted by ().

data Unit = Unit

fork :: (a -> Bool) -> a -> Either a a
fork p a | p a = Left a
         | otherwise = Right a

-- thus @either f g . fork p@ models the familiar conditional if p then f else g . The function @const Unit@
-- maps its argument to @Unit@:

const :: a -> b -> a
const a _ = a

-- Some laws that hold for these functions are:
--
-- prop> const Unit . f = const Unit
-- prop> fork p . f = either f f . fork (p . f)

-- In order to make recursion explicit, we use @fix@:

fix :: (a -> a) -> a
fix f = x 
    where x = f x

-- Let F; G be functors and 'A 2 AF ! AG for any type A. Such a ' is called a polymorphic
-- function. A natural transformation is a family of functions 'A (omitting subscripts whenever
-- possible) such that:
-- 8f : f 2 A ! B : 'B  fF = fG  'A (11)
-- As a convenient shorthand for (11) we use ' 2 F
--
-- ! G to denote that ' is a natural transformation.
-- The \Theorems For Free!" theorem of Wadler, deBruin and Reynolds [28, 9, 22]
-- states that any function denable in the polymorphic -calculus is a natural transformation. If
-- ' is dened using , one can only conclude that (11) holds for strict f.


-- == Recursive types

-- After all this stu on functors we have nally armed ourselves suciently to abstract from the
-- peculiarities of cons-lists, and formalize recursively dened data types in general.

-- Let F be a functor. Then I claim that there exists a type L and two 
-- functions @inF :: F L -> L@ and @outF :: L -> F L@ which are inverses of each other. 
-- Furthermore, @inF@ and @outF@ respect fmap, in the following sense:
-- 
-- prop> id === (inF <-*- outF) 
-- in other words, 
-- prop> (h :: L -> L) === id h === (inF <-*- outF) h 
--          === inF . fmap h . outF
--
-- See [6, 23, 16, 24, 30, 12]. We will construct L in Haskell in a moment. 
--
-- We say that L is \the least xed point of F".
-- Since in and out are each others inverses we have that LF is isomorphic to L, and
--  indeed L is | upto isomorphism | a xed point of F.

data Fix f = Fix { unFix :: f (Fix f) }

-- And take inF = Fix, outF = unFix.
-- Indeed, we see that given @h :: Fix f -> Fix f@ we can define
-- g :: f (Fix f) -> f (Fix f) 
-- g = unFix . h . Fix
--
-- (Fix . fmap h . unFix) (Fix u) === (Fix . fmap h) u === Fix (fmap h u)

--  For example taking 

data ListF a r = NilF | ConsF a r

instance Functor (ListF a) where
    fmap _ NilF = NilF
    fmap f (ConsF a r) = ConsF a (f r)

-- we have that @Fix (ListF a)@ denes the data type of conslists
-- over A for any type A. In fact, we can exhibit an isomorphism using
-- explicit recursion:

toList :: Fix (ListF a) -> List a
toList (Fix NilF) = Nil
toList (Fix (ConsF a r)) = Cons a (toList r)

-- Another example of data types, binary trees with leaves of type A results from taking 
-- @Fix (TreeF a)@, where

data TreeF a r = NilLeafF | LeafF a | BranchF r r

--  Backward lists with elements of type A, or snoc lists as they are sometimes called, are the
--  least xed point of @SnofF a@, where

data SnocF a r = SnofNilF | SnocF r a 

-- Natural numbers are specied as the least xed point of

data NatF r = ZeroF | SuccF r

instance Functor NatF where
    fmap _ ZeroF = ZeroF
    fmap f (SuccF r) = SuccF $ f r

natFToInteger :: Fix NatF -> Integer
natFToInteger (Fix ZeroF) = 0
natFToInteger (Fix (SuccF u)) = 1 + natFToInteger u

-- Aside: All of the above data types represent a "link" in a recursive data structure.
-- The links are then recursively built up, with the type variable r representing
-- the recursive substructures. One pattern that we see is that all of the "link types"
-- are Sum types with default cases that do not include any fields of type @r@. These
-- provide "default" or "base" cases and allow the data structures to be finite.
-- Excluding these types can still produce useful data structures, but they
-- will be infinite and we will have to faithfully leverage laziness to use them effectively.

