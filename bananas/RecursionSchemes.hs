{-# LANGUAGE TupleSections #-}
module RecursionSchemes where

import Prelude hiding(filter, length, filter, zip, null, iterate, take, map, fromInteger,
    toInteger, Functor(..), fst, snd, Either(..), either )

import List
import ADT

-- Now that we have given a generic way of dening recursive data types, we can dene cata-,
-- ana-, hylo- and paramorphisms over arbitrary data types. Let (L; in) = F, ' 2 AF ! A; 2
-- A ! AF;  2 (AkL)F ! A then we dene

cata :: Functor f => (f a -> a) -> Fix f -> a
cata phi = fix (phi <-*- unFix)

ana :: Functor f => (a -> f a) -> a -> Fix f
ana psi = fix (Fix <-*- psi)

hylo :: Functor f => (a -> f a) -> (f b -> b) -> a -> b
hylo phi psi = fix (psi <-*- phi)

para :: Functor f => (f (a, Fix f) -> a) -> Fix f -> a
para xsi = fix $ \f -> xsi . fmap (both f id) . unFix



-- ASIDE: The editor believes that the following definitions of our four 
-- recursion combinators are a bit more clear in their meaning, if perhaps
-- less eligant because they do not reduce all recursion to @fix@.
--
-- REVISION: The following derivations show more directly recursive definitions.
-- recall that
-- u = fix v
-- is equivalent to
-- u = v u

cata1, cata2 :: Functor f => (f a -> a) -> Fix f -> a
cata1 phi = fix (\h -> phi . fmap h . unFix)
cata2 phi = phi . fmap (cata2 phi) . unFix

ana1, ana2 :: Functor f => (a -> f a) -> a -> Fix f
ana1 psi = fix (\h -> Fix . fmap h . psi)
ana2 psi = Fix . fmap (ana2 psi) . psi

hylo1, hylo2, hylo3 :: Functor f => (a -> f a) -> (f b -> b) -> a -> b
hylo1 phi psi = fix (\h -> psi . fmap h . phi)
hylo2 phi psi = psi . fmap (hylo2 phi psi) . phi
hylo3 phi psi = cata psi . ana phi

hylo phi psi = fix (psi <-*- phi)
hylo phi psi = fix (psi <-*- phi)

para1, para2 :: Functor f => (f (a, Fix f) -> a) -> Fix f -> a
para1 xsi = xsi . fmap (both (para1 xsi) id) . unFix
para2 xsi u = xsi . fmap (\x -> (x,u)) . fmap (para2 xsi) . unFix $ u

-- Denition (13) agrees with the denition given in x2; where we wrote (je; j) we now write
-- (je5 ()j).

fold :: (a -> b -> b) -> b -> Fix (ListF a) -> b
fold f b = cata phi
    where phi NilF = b
          phi (ConsF a b) = f a b
-- As ListF a r =~= Either () (a,r), this is
-- =~= cata (either (const b) (uncurry f))

-- Denition (14) agrees with the informal one given earlier on; the notation db(g; p)ec of 2 now
-- becomes db((VOID j g)  p?)

unfold :: (b -> Bool) -> (b -> (a,b)) -> b -> ListF a
unfold p g = cata psi
    where psi b | p b = NilF
                | otherwise = let (a,b') = g b in ConsF a b'
-- As ListF a r =~= Either () (a,r), this is
-- =~= cata (bimap (const ()) g . fork p)

-- Denition (15) agrees with the earlier one in the sense that taking ' = c
-- 5  and = (VOID j g)  p? makes [[(c ; ); (g; p)]] equal to [['; ]].

-- Denition (15) agrees with the description of paramorphisms as given in x2 in the sense that
-- h[b; i] equals h[b
-- 5 ()i] here

-- == Program Calculation Laws

-- Rather than letting the programmer use explicit recursion, we encourage the use of the above
-- xed recursion patterns by providing a shopping list of laws that hold for these patterns. For
-- each -morphism, with {fcata, ana, para}, we give an evaluation rule, which shows how
-- such a morphism can be evaluated, a Uniqueness Property, a canned induction proof for a given
-- function to be a -morphism, and a fusion law, which shows when the composition of some
-- function with an -morphism is again an -morphism. All these laws can be proved by mere
-- equational reasoning using the following properties of general recursive functions.
-- The rst one is a `free theorem' for the xed point operator  2 (A ! A) ! A

-- prop> If f . g = h . f, then f (fix g) = fix h

-- Theorem (16) appears under dierent names in many places2
-- [20, 8, 2, 15, 7, 25, 13, 31]. In
-- this paper it will be called xed point fusion
--
-- The strictness condition in (16) can sometimes be relaxed by using
-- prop>  If f undefined = f' undefined and f . g = h . f and f' . g' = h . f'
--          then f (fix g) = f' (fix g')

-- Fixed point induction over the predicate P(g; g0)  f g = f 0 g0 will prove (17).
-- For hylomorphisms we prove that they can be split into an ana- and a catamorphism and show
-- how computation may be shifted within a hylomorphism. A number of derived laws show
-- the relation between certain cata- and anamorphisms. These laws are not valid in SET. The
-- hylomorphism laws follow from the following theorem

-- prop> If g . h = id, then fix (f <-*- g) . fix (h <-*- j) = fix (f <-* j)

-- == Catamorphisms

-- === Evaluation rule 

-- The evaluation rule for catamorphisms follows from the xed point property
-- that If x = fix f, then x = f x:
--
-- CataEval> cata phi . Fix = phi . fmap (cata phi)
--
-- It states how to evaluate an application of phi to an arbitrary element of Fix f (returned by the
-- constructor Fix); namely, apply `cata phi` recursively to the argument of `Fix` and 
-- then phi to the result.

-- For cons lists (A; Nil 5 Cons) = L where XL = 1 j AkX and fL = id j idkf with
-- catamorphism (jc 5 j) the evaluation rule reads:
--> cata 
-- (jc 5 j)  Nil = c (19)
-- (jc 5 j)  Cons =   idk(jc 5 j) (20)
-- i.e. the variable free formulation of (1). Notice that the constructors, here Nil 5 Cons are
-- used for parameter pattern matching.

