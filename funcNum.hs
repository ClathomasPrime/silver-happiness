{-# LANGUAGE FlexibleInstances #-}

instance Num a => Num (a -> a) where
    (f + g) x = f x + g x
    (f * g) x = f x * g x
    (f - g) x = f x - g x
    negate f x = negate (f x)
    abs f x = abs (f x)
    signum f x = signum (f x)

pref :: [String] -> String
pref = foldl1 (\a b -> fmap fst . takeWhile (uncurry (==)) $ zip a b)
