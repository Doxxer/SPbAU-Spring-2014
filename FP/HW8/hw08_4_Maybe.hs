----------- Maybe ---------------
instance Applicative Maybe where
    pure = Just
    (Just f) <*> (Just k) = Just (f k)
    _ <*> _ = Nothing    

1. Identity (pure id <*> v ≡ v)

pure id <*> v =
Just id <*> v =
    case v of
        Nothing -> Nothing (OK)
        Just v' -> Just id <*> Just v' = Just (id v') = Just v' (OK)
        
2. Composition pure (.) <*> u <*> v <*> w ≡ u <*> (v <*> w)

pure (.) <*> u <*> v <*> w = 
((Just (.) <*> u) <*> v) <*> w
    case u of
        Nothing -> ((Just (.) <*> Nothing) <*> v) <*> w = (Nothing <*> v) <*> w = Nothing <*> w = Nothing (OK: Nothing <*> (v <*> w) = Nothing)
        Just u' -> ((Just (.) <*> Just u') <*> v) <*> w = (Just (u' .) <*> v) <*> w =
            case v of
                Nothing -> ....... = Nothing (OK аналогично)
                Just v' -> (Just (u' .) <*> Just v' ) <*> w = Just (u' . v') <*> w = 
                    case w of
                        Nothing -> .... = Nothing (OK аналогично)
                        Just w' = Just (u' . v') <*> Just w' = Just ((u' . v') w') = Just (u'(v' w')) = Just u' <*> Just (v' w') = 
u <*> Just (v' w') = u <*> (v <*> w) (OK)

3. Homomorphism pure g <*> pure x ≡ pure (g x)
pure g <*> pure x = 
Just g <*> Just x == Just (g x) (OK сразу)

4. Interchange g <*> pure x ≡ pure ($ x ) <*> g
g <*> pure x = 
g <*> Just x = case g of 
    Nothing -> Nothing <*> Just x = Nothing (OK: Just ($ x) <*> Nothing = Nothing)
    Just g' -> Just g' <*> Just x = Just (g' x)
    
pure ($ x) <*> g = Just ($ x) <*> Just g' = Just (($ x) g') = (Just g' x) (OK)

5. Закон, связывающий Applicative и Functor fmap g as ≡ pure g <*> as
fmap g as = case as of
    Nothing -> fmap g Nothing = Nothing (OK: Just g <*> Nothing = Nothing)
    Just x -> fmap g (Just x) = Just (g x)

pure g <*> as = Just g <*> as = Just g <*> Just x = Just (g x) (OK)

Все законы Applicative Maybe выполняются.
        