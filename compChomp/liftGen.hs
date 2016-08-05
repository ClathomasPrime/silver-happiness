
-- mapM :: (a -> m b) -> [a] -> m [b]
--   = \f -> sequence . map f
-- filterM :: (a -> b Bool) -> [a] -> m [a]
--   = \p -> 
-- concatMapM :: (a -> m [b]) -> [a] -> m [b]

liftGen :: ((a -> b) -> t a -> t (s b)) -> (a -> m b) -> t a -> m (t c)
liftGen lft f t = undefined

liftGen' :: ((a -> b) -> c -> d) -> (a -> m b) -> c -> m d 
liftGen' = undefined
