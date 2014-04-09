module HW09 where

-- 1. Повторите каждый элемент списка заданное число раз, используя монаду списка и do-нотацию.
replicate' :: Int -> [a] -> [a]
replicate' k xs = do
    x <- xs
    replicate k x

-- 2. Разложите число на два сомножителя всевозможными способами, используя монаду списка и do-нотацию.
factor :: Int -> [(Int, Int)]
factor k = factor' [1..k] k where
    factor' :: [Int] -> Int -> [(Int, Int)]
    factor' xs k = do
        x <- xs
        (d, m) <- return (k `divMod` x)
        True <- return (m == 0)
        True <- return (x <= d)
        return (x, d)

-- 3. Вычислите модули разностей между соседними элементами списка, используя монаду списка и do-нотацию.
nmod :: [Int] -> [Int]
nmod [x, y] = [abs $ x - y]
nmod (x:y:xs) = do
    ys <- return $ nmod $ y:xs
    z <- return $ abs $ x - y    
    z:ys
nmod _ = []

-- но непонятно, зачем оборачивать в монаду (делать return), потом вынимать (делать <-), когда можно сразу
-- nmod (x:y:xs) = do
--     (nmod $ y:xs):(abs $ x - y)
-- а теперь встает вопрос: зачем do, когда можно сразу
-- nmod (x:y:xs) = (nmod $ y:xs):(abs $ x - y)

-- изврат и тормоз, конечно, но... (тем более не работает на бесконечных списках)
nmod' :: [Int] -> [Int]
nmod' xs = do
    x <- [0 .. (length xs) - 2]
    return $ abs $ (xs !! x) - (xs !! (x+1))


-- 4. Покажите, что каждая монада — это аппликативный функтор
pure :: (Monad m) => a -> m a
pure = return
(<**>) :: (Monad m) => m (a -> b) -> m a -> m b
(<**>) f x = x >>= \a -> f >>= \g -> return $ g a

(<*>) :: (Monad m) => m (a -> b) -> m a -> m b -- оно же в do-нотации
(<*>) f x = do
    a <- x
    do
        g <- f
        return $ g a

main :: IO()
main = do
    putStrLn "------- 1 --------"
    print $ replicate' 5 [1..2]
    print $ replicate' 4 [1..10]
    print $ replicate' 0 [1..1]
    print $ replicate' 4 [1..1]
    
    
    putStrLn "------- 2 --------"
    print $ factor 1
    print $ factor 2
    print $ factor 8
    print $ factor 2047 -- mersenn' number
    
    putStrLn "------- 3 --------"
    print $ nmod [2, 7, 22, 9]
    print $ nmod [2, 7]
    print $ nmod [2]
    
    print $ nmod' [2, 7, 22, 9]
    print $ nmod' [2, 7]
    print $ nmod' [2]
    
    putStrLn "------- 4 --------"
    print $ Just (+2) <*> Just 5
    print $ Just (+2) <**> Just 5
    print $ [\x->2*x, \x->3+x, \x->4-x] <*> [1,2]
    print $ [\x->2*x, \x->3+x, \x->4-x] <**> [1,2]    