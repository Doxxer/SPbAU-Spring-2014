module PR09 where
--import Control.Monad
    
-- 1. Напишите реализацию функции filter, используя монаду списка и do-нотацию.
filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = do
    x <- xs
    True <- return (p x)
    return x

-- 2. Выразите (>=>) через (>>=)
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(>=>) k1 k2 x = k1 x >>= k2

-- 3. Выразите join через (>>=)
-- 4. Запишите join в do-нотации.
join :: Monad m => m (m a) -> m a
--join m = m >>= id
join m = do { x <- m; id x }

-- 6. Выразите (>=>) через join (и fmap).
(>==>) :: (Monad m, Functor m) => (a -> m b) -> (b -> m c) -> a -> m c
(>==>) k1 k2 x = join $ fmap k2 (k1 x)

-- 7. Выразите fmap через (>>=) и return
fmap' :: Monad m => (a -> b) -> m a -> m b
fmap' f xs = xs >>= \x -> return $ f x

--8. Запишите эту реализацию fmap, используя do-нотацию.
fmap'' :: Monad m => (a -> b) -> m a -> m b
fmap'' f xs = do
    ys <- xs
    return $ f ys

main :: IO()
main = do
    print $ "------ 0 ----------"
    print $ do        
        [1,2,3]
        [4,5]
        [6]
        [7,8]
            
    print $ "------ 1 ----------"
    print $ do        
        x <- Just 17
        Just (x < 21)
        
    print $ do        
        x <- [1,2,3]
        [x, x*2]
        
    print $ do
        x <- [1,2]
        y <- ['a','b']
        return (x, y)
        
    print $ filter' (>5) [1..10]
    
    print $ "------ 2 ----------"
    print $ join ["aaa", "bb"]
    print $ replicate 2 >=> replicate 3 $ 'x'
    print $ (\x -> [x,x+10]) >=> (\x -> [x,2*x]) $ 1
    
    print $ "------ 3 ----------"
    print $ fmap' (+2) [1..10]
    print $ fmap'' (+2) [1..10]
    print $ replicate 2 >==> replicate 3 $ 'x'