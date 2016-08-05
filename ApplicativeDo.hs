
-- data Free a 
--     = Return a
--     | Bind (a -> f 

-- regular moad stuff: 

trans(
    do a <- v
       BLK
) = 
    v >>= \a -> trans(BLK)

trans(
    do let a = b
       BLK
) = 
    let a = b in trans(BLK)

trans(
    do stmt
       BLK
) =
    stml >> trans(BLK)

-- 
-- do a <- u
--    b <- v
--    return $ f a b
-- ==
-- f <$> u <*> v 


    a <- u
    b <- v
    c <- f a
    g b c
== 
    u >>= \a -> v >>= \b -> f a >>= \c -> g b c
== 
    ((,) <$> (u >>= f) <*> v) >>= uncurry g


    a <- u 
    b <- f a
    let c = g b
    d <- v
    e <- w
    x <- h e c
    return $ j c d x
== 
    fmap g (u >>= f) >>= \c -> v >>= \d -> w >>= \e -> h e c >>= \x -> return $ j c d x
== 
    ((,,) <$> fmap g (u >>= f) <*> v <*> w) >>= \(c,d,e) -> j c d <$> h e c






