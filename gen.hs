myAnyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
myAnyM _ [] = return False
myAnyM f (a:as) = do
  b <- f a
  if b
    then return True
    else myAnyM f as

myFoldM :: Monad m => (a -> b -> m b) -> [a] -> b -> m b
myFoldM _ [] b = return b
myFoldM f (a:as) b = do
  b' <- f a b
  myFoldM f as b'
