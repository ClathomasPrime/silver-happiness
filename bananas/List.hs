module List (List(..)) where

import Prelude hiding(filter, length, filter, zip, null, iterate, take, map, fromInteger, toInteger)

-- = The Data Type of List

-- | We shall illustrate the recursion patterns of interest by 
-- means of the specic data type of conslists.
-- So, the definitions given here are actually specic instances of those given in section 4. Modern
-- functional languages allow the definition of cons-lists over some type A by putting:

data List a = Nil | Cons a (List a)
    deriving(Show, Eq)

null :: List a -> Bool
null Nil = True
null _ = False

-- ^ The recursive structure of this definition is employed when writing functions List @a -> b@ that
-- destruct a list; these have been called catamorphisms (from the greek preposition meaning
-- "downwards" as in "catastrophe"). Anamorphisms are functions @b -> [a]@ (from the greek
-- preposition  meaning "upwards" as in "anabolism") that generate a list of type A from a
-- seed from B. Functions of type @a -> b@ whose call-tree has the shape of a cons-list are called
-- hylomorphisms (from the Aristotelian philosophy that form and matter are one, hylo meaning
-- "dust" or "matter").

-- == Catamorphisms

-- | Let @b :: b@ and @f :: a -> b -> b@, then a list-catamorphism @h :: [a] -> b@ is a function
-- of the form @h = cata f b@, where

cata :: (a -> b -> b) -> b -> List a -> b
cata _ b Nil = b
cata f b (Cons a l) = f a (cata f b l)

-- ^ Note that we have @h Nil = b@ and @h (Cons a l) = f a (h l)@.
-- In the notation of Bird&Wadler [5] one would write @h = foldr b f@.
-- Indeed in modern Haskell notation, we would write @h = foldr f b@.

-- | Countless list processing functions are readily recognizable as catamorphisms, for example

length :: List a -> Integer
length = cata (\_ i -> i + 1) 0

filter :: (a -> Bool) -> List a -> List a
filter p = cata phi Nil
    where phi a as | p a = Cons a as
                   | otherwise = as

-- Separating the recursion pattern for catamorphisms from its ingredients b and  makes it
-- feasible to reason about catamorphic programs in an algebraic way. For example the Fusion
-- Law for catamorphisms over lists reads
--
-- prop> f (u a x) = v a (f x) ==>> f . cata u b = cata v (f b)
--
-- Without special notation pinpointing catas, such as @cata@ or @foldr@, we would be 
-- forced to formulate the fusion law as follows
--
-- Let h; g be given by
-- @
-- h Nil = b 
-- h (Cons a as) = u a (h as)
--
-- g Nil = f b
-- g (Cons a as) = v a (g as)
-- @
--
-- then if @f (u a x) = v a (f x)@, then @h = g@
-- 
-- A clumsy way of stating such a simple algebraic property.


-- == Anamorphisms

-- Given a predicate @p :: b -> Bool@ and a function @g :: b -> (a, b)@ , a list-anamorphism
-- @h :: b -> List a@ is defined as @h = ana p g@, where

ana :: (b -> Bool) -> (b -> (a, b)) -> b -> List a
ana p g b 
    | p b = Nil
    | otherwise
        = let (a,b') = g b
           in Cons a (ana p g b')

-- Anamorphisms are not well-known in the functional programming folklore, they are called
-- unfold by Bird&Wadler, who spend only few words on them.

-- Aside: The predicate @p@ above give a "stopping condition" for the unfolding process.
-- Of course, when the stopping condition returns true, the function @g@ is not evaluated at
-- that particular value of b. So it is possible that the function @g@ cannot be meaninfull defined
-- at some @b@, the its usage in @ana@ will still be valid. Thus, perhaps a more reasonable type for 
-- ana would be @(b -> Maybe (a, b))@. Because we are not making full use of our type
-- system, some of the functions we will use to construct list anamorphisms will be partial.
-- Such is life. Of course, this and other problems will be rectified seamlessly in Part 2 ;).

-- Many important list-valued functions are anamorphisms; for example 
-- @zip :: List a -> List b -> List (a,b)@ which "zips" a pair of lists into a list of pairs.

zip :: List a -> List b -> List (a,b)
zip as bs = ana p g (as, bs)
    where p :: (List a, List b) -> Bool
          p (as, bs) = null as || null bs
          g :: (List a, List b) -> ((a, b), (List a, List b))
          g (Cons a as, Cons b bs) = ((a, b), (as, bs))

-- Another anamorphism is @iterate f@ which given @a@, constructs the infinite 
-- list of iterated applications of @f@ to @a@.

iterate :: (a -> a) -> a -> List a
iterate f = ana (const False) (\a -> (a, f a))
    where const :: a -> b -> a
          const a _ = a     -- ^ As in the Prelude

-- Given @f :: a -> b@, the function @map f :: List a -> List b@ applies @f@ to every 
-- element in a given list.

map :: (a -> b) -> List a -> List b
map _ Nil = Nil
map f (Cons a as) = Cons (f a) (map f as)

-- Since a list appears at both sides of its type, we might suspect that map can be written
-- both as a catamorphism and as an anamorphisms. Indeed this is the case:

map', map'' :: (a -> b) -> List a -> List b

map' f = cata (\a bs -> Cons (f a) bs) Nil

map'' f = ana null (\(Cons a as) -> (f a, as))


-- == Hylomorphisms

-- A recursive function @h :: a -> c@ whose call-tree is isomorphic to a cons-list, i.e., a linear
-- recursive function, is called a hylomorphism. Let @c :: c@ and @u :: b -> c -> c@ and @g :: a -> (b, a)@
-- and @p :: a -> Bool@  then these determine the hylomorphism @h = hylo p g c u :: a -> c@, where

hylo :: (a -> Bool) -> (a -> (b, a)) -> c -> (b -> c -> c) -> a -> c
hylo p g c u a
    | p a = c
    | otherwise 
        = let (b, a') = g a
           in u b (hylo p g c u a')

-- This is exactly the same structure as an anamorphism except that Nil has been replaced by c
-- and Cons by u.

-- A hylomorphism corresponds to the composition of an anamorphism that builds the call-tree as
-- an explicit data structure and a catamorphism that reduces this data object into the required
-- value.

hylo' :: (a -> Bool) -> (a -> (b, a)) -> c -> (b -> c -> c) -> a -> c
hylo' p g c u = cata u c . ana p g

-- A proof of this equality will be given in section 15.

-- An archetypical hylomorphism is the factorial function

factorial :: Integer -> Integer
factorial = hylo (==0) (\i -> (i, i-1)) 1 (*)



-- == Paramorphisms

-- The hylomorphism definition of the factorial may be correct but is unsatisfactory from a theoretic
-- point of view since it is not inductively defined on the data type 

data Nat = Zero | Succ Nat

toInteger :: Nat -> Integer
toInteger Zero = 0
toInteger (Succ n) = 1 + toInteger n

fromInteger :: Integer -> Nat
fromInteger 0 = Zero
fromInteger n = Succ (fromInteger $ n-1)

-- There is however no "simple" @phi@ such that @factorial = cata phi@. 
-- NOTE: the above cata is the one from section 3, not the one specialized to lists. 
-- The problem with the factorial is that it "eats its argument and keeps it too" [27],
-- the brute force catamorphic solution would therefore have @factorial'@
-- return a pair @(n, n!)@ to be able to compute (n + 1)!.

-- Paramorphisms were investigated by Meertens [19] to cover this pattern of primitive recursion.
-- For type @Nat@ a paramorphism is a function @h = paraNat f b@, where

paraNat :: (Nat -> b -> b) -> b -> Nat -> b
paraNat _ b Zero = b
paraNat f b (Succ n) = f n (paraNat f b n)

-- For lists a paramorphism is a function @h = paraList f b@, where

paraList :: (a -> List a -> b -> b) -> b -> List a -> b
paraList _ b Nil = b
paraList f b (Cons a as) = f a as (paraList f b as)

-- Thus we may write 

factorial' :: Nat -> Integer
factorial' = paraNat phi 1
    where phi n nFac = toInteger (Succ n) * nFac

-- The function tails, which gives the list of all tail segments of a given list,
-- is defined by the paramorphism

tails :: List a -> List (List a)
tails = paraList (\a as aas -> Cons (Cons a as) aas) (Cons Nil Nil)

-- ASIDE: Right now, the implementation of para for Lists and Natural numbers seems
-- totally different. The main achievement of the origional paper was to unify
-- these two, along with other recursive data types








-- = Variations

cataState :: u -> (u -> a -> b -> (b, u)) -> b -> List a -> b
cataState u0 phi b0 = fst . cata (\a (b, u) -> phi u a b) (b0, u0)

-- doesn't work, fuck this
take' :: Integer -> List a -> List a
take' n = cataState n phi Nil
    where phi :: Integer -> a -> List a -> (List a, Integer)
          phi i a as | i > 0 = (Cons a as, i - 1)
                     | otherwise = (Nil, i - 1)

randoMclist :: List Integer
randoMclist = 3 `Cons` (4 `Cons` (8 `Cons` (5 `Cons` Nil)))

-- Things I just need

take :: Integer -> List a -> List a
take 0 _ = Nil
take n Nil = Nil
take n (Cons a as) = Cons a (take (n-1) as)



