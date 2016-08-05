{-# LANGUAGE GeneralizedNewtypeDeriving 
           , LambdaCase
           , DeriveFunctor
           , TypeFamilies
           , FlexibleContexts
  #-}
import Data.Char

powList :: Int -> [a] -> [[a]]
powList 0 _ = [[]]
powList n as = [ a:as' | a <- as, as' <- powList (n-1) as ]

capCombs :: Int -> [[Char -> Char]]
capCombs n = powList n [toUpper, toLower]

cap :: String -> [String]
cap s = fmap f (capCombs $ length s)
    where f c = zipWith ($) c s




-- yea

data Fix f = Fix { unFix :: f (Fix f) }

-- ana :: Functor f => (f a -> a) -> Fix f -> a
-- ana alg = alg . fmap (ana alg) . unFix

-- cata :: Functor f => (a -> f a) -> a -> Fix f
-- cata coalg = Fix . fmap (cata coalg) . coalg

data NatF r = ZeroF | NatF r
    deriving(Functor)

class Functor (Base u) => FoldRec u where
    data Base u :: * -> *

    out :: u -> Base u u

    inn :: Base u u -> u
    
    ana :: (Base u a -> a) -> u -> a
    ana alg = alg . fmap (ana alg) . out

    para :: (Base u a -> u -> a) -> u -> a
    para alg u = alg (fmap (para alg) $ out u) u

    cata :: (a -> Base u a) -> a -> u
    cata coalg = inn . fmap (cata coalg) . coalg


instance FoldRec Integer where
    -- Base Integer ~= Maybe
    data Base Integer r 
        = Zero | Succ r
        deriving(Functor)

    out 0 = Zero
    out n | n > 0 = Succ (n-1)

    inn Zero = 0
    inn (Succ n) = n + 1

instance FoldRec [a] where
    data Base [a] r
        = Null | Cons a r
        deriving(Functor)

    out [] = Null
    out (a:as) = Cons a as

    inn Null = []
    inn (Cons a as) = a:as
