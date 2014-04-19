module HW10 where
import Control.Monad.State
import Control.Monad.Writer
import System.Random
import Data.IORef
import Data.List
import System.IO

-- 1. Напишите функцию вычисляющую факториал с использованием монады State
factorial :: Int -> Int
factorial n | n < 0 = error "n must be non-negative"
            | otherwise = evalState helper (0, 1)
            where
                tickFactorial :: State (Int, Int) Int
                tickFactorial = do
                    modify (\(n, acc) -> (n + 1, acc * (n + 1)))
                    gets snd                
                helper :: State (Int, Int) Int
                helper = do
                    replicateM n tickFactorial
                    gets snd

-- 2. Напишите функцию вычисляющую числа Фибоначчи с использованием монады State.
fib :: Int -> Int
fib n
    | n < 0 = error "n must be non-negative"
    | otherwise = evalState helper (1, 0)
    where
        tickFibonacci :: State (Int, Int) Int
        tickFibonacci = do
            modify (\(a, b) -> (b, a + b))
            gets snd
        helper :: State (Int, Int) Int
        helper = do
            replicateM n tickFibonacci
            gets snd

-- 3. Напишите функцию while, позволяющую описывать «императивные циклы»
while :: IORef t -> (t -> Bool) -> IO () -> IO ()
while param predicate body = do
    x <- readIORef param
    if predicate x then do
        body
        while param predicate body
    else return ()

factorial' :: Int -> IO Int
factorial' n = do
    r <- newIORef 1
    i <- newIORef 1
    while i (<= n) ( do
        ival <- readIORef i
        modifyIORef r (* ival)
        modifyIORef i (+ 1) )
    readIORef r

-- 4. Используя монаду Writer, напишите sumLogged
sumLogged :: (Num a, Show a) => [a] -> Writer String a
sumLogged l = sumLogged' l 0
    where
        sumLogged' [] a = do
            tell $ "0"
            return a
        sumLogged' (x:xs) a = do
            tell $ "(" ++ show x ++ "+"
            res <- listen $ sumLogged' xs (a+x)
            tell $ ")"
            return $ fst res

-- 5. Монетки
getSample :: IO Int
getSample = do
    q <- (replicateM 1000 $ randomRIO (0, 1))
    return $ abs (500 - sum q)

runExperiment :: IO Double
runExperiment = do
    xs <- replicateM 1000 $ getSample
    return $ realToFrac (sum xs) / genericLength xs
      
main = do
    putStrLn "------- 1 --------"
    print $ factorial 10
    putStrLn "------- 2 --------"
    print $ fib 7
    putStrLn "------- 3 --------"
    factorial' 10 :: IO Int
    putStrLn "------- 4 --------"
    print $ runWriter $ sumLogged [1..10]
    putStrLn "------- 5 --------"
    runExperiment