
fix :: (a -> a) -> a
fix f = f (fix f)

fibs :: [Int]
fibs = fmap fst . fix $ \ns -> (0,1) : fmap (\(i,j) -> (j,i+j)) ns

fibs1 :: [Int]
fibs1 = fmap fst fibPairs
    where fibPairs = fix phi
          phi ns = (0,1) : fmap (\(i,j) -> (j,i+j)) ns

fibs2 :: [Int]
fibs2 = fmap fst fibPairs
    where fibPairs = phi (phi (phi (phi (fix phi))))
          phi ns = (0,1) : fmap (\(i,j) -> (j,i+j)) ns

fibs3 :: [Int]
fibs3 = fmap fst fibPairs
    where fibPairs = phi fibPairs
          phi ns = (0,1) : fmap (\(i,j) -> (j,i+j)) ns

fibs4 :: [Int]
fibs4 = fmap fst fibPairs
    where fibPairs = (0,1) : fmap (\(i,j) -> (j,i+j)) fibPairs

-- in general, 
-- u = fix v
-- is equivalent to 
-- u = v u
          
fib :: Int -> Int
fib = snd . fib'
    where fib' :: Int -> (Int,Int)
          fib' 0 = (0,0)
          fib' 1 = (0,1)
          fib' n = let (i,j) = fib' (n-1) in (j,i+j)

