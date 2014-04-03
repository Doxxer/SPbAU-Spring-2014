----------- List ---------------
instance Applicative [] where
    pure x = [x]
    gs <*> xs = [ g x | g <- gs, x <- xs ]
  
1. Identity (pure id <*> v ≡ v)

pure id <*> v =
[id] <*> v = case v of
    [] -> [g x | g <- [id], x <- []] = [] (OK)
    x:xs -> [g y | g <- [id], y <- x:xs] = x:xs (OK, тут довольно очевидно)
        
2. Composition pure (.) <*> u <*> v <*> w ≡ u <*> (v <*> w)
-- тут в целом все аналогично Maybe, но только работа идет внутри списков
pure (.) <*> u <*> v <*> w = 
([(.)] <*> u) <*> v) <*> w
    case u of
        [] -> [] (OK потому что [] <*> _ = [] и _ <*> [] = [])
        u:us -> ([(.)] <*> u:us) <*> v) <*> w = ([(.) u' | u' <- u:us] <*> v) <*> w =
            case v of
                [] -> ....... = [] (OK аналогично)
                v:vs -> ([(.) u' | u' <- u:us] <*> v:vs) <*> w = [u' . v' | u' <- u:us, v' <- v:vs] <*> w
                    case w of
                        [] -> .... = [] (OK аналогично)
                        w:ws = [u' . v' | u' <- u:us, v' <- v:vs] <*> w = [(u' . v') w' | u' <- u:us, v' <- v:vs, w' <- w:ws] =
                        [ u' (v' w') | u' <- u:us, v' <- v:vs, w' <- w:ws] = 
    u <*> [ v' w' | v' <- v:vs, w' <- w:ws] = u <*> (v <*> w) (OK)

3. Homomorphism pure g <*> pure x ≡ pure (g x)
pure g <*> pure x = 
[g] <*> [x] == [g x] = pure (g x) (OK)

4. Interchange g <*> pure x ≡ pure ($ x ) <*> g
g <*> pure x = 
g <*> [x] = case g of 
    [] -> [] <*> [x] = [g y | g <- [], y <- [x]] = [] (OK: [$ x] <*> [] = [g y | g <- [$ x], y <- [] = [])
    y:ys -> [g z | g <- y:ys, z <- [x]]
    
pure ($ x) <*> y:ys = [$ x] <*> y:ys = [g z | g <- [$ x], z <- y:ys] = [g' z' | g <- y:ys, z' <- [x]] (OK)

5. Закон, связывающий Applicative и Functor fmap g as ≡ pure g <*> as
fmap g as = case as of
    [] -> fmap g [] = [] (OK: pure g <*> [] = [])
    y:ys -> fmap g (y:ys) = g y:fmap g ys

pure g <*> y:ys = [g] <*> y:ys = [f x | f <- [g], x <- y:ys] = [g x | x <- y:ys] = g y:fmap g ys (OK)

Все законы Applicative List выполняются.
        