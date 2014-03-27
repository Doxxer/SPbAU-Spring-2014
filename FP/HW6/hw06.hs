module HW06 where
-- 1. Повторите каждый элемент списка заданное число раз.

rep :: Int -> [a] -> [a]
rep = concatMap . replicate

-- 2. Удалите каждый n-ый элемент списка.
removeEvery :: Int -> [a] -> [a]
removeEvery _ [] = []
removeEvery k l = take (k-1) l ++ removeEvery k (drop k l)

-- 3. Выделите подсписок с n-го по k-ый номер [n;k).
part :: Int -> Int -> [a] -> [a]
part n k a = take (k-n) $ drop n a 

-- 4. Задайте циклическую ротацию списка влево.
rotate :: Int -> [a] -> [a]
rotate k l
    | k < 0 = take (length l) $ drop newK $ cycle l
    | otherwise = drop newK l ++ take newK l
    where newK = mod k (length l)
    
    
-- 5. Удалите k-ый элемент из списка, вернув его и список.
remove :: Int -> [a] -> (a, [a])
remove k l = (head q, concat [p, drop 1 q])
    where (p, q) = splitAt k l
    
-- 6. Найдите все сочетания из элементов заданного списка по n элементов.
comb :: Int -> [a] -> [[a]]
comb 0 _ = [[]]
comb _ [] = []
comb k (x:xs) = map (x:) (comb (k-1) xs) ++ comb k xs


data Tree a = Empty | Branch (Tree a) a (Tree a) deriving (Show, Eq)

--7. Напишите функцию, возвращающую список всех полностью сбалансированных деревьев типа Tree () с n узлами.
generateBalancedTrees :: Int -> [Tree ()]
generateBalancedTrees 0 = [Empty]
generateBalancedTrees n = [Branch l () r | i <- [q .. q + r], l  <- generateBalancedTrees i, r <- generateBalancedTrees (n - i - 1)]
                            where (q, r) = divMod (n - 1) 2


generateBalancedTrees2 :: Int -> [Tree ()]
generateBalancedTrees2 0 = [Empty]
generateBalancedTrees2 n
    | mod n 2 == 1 = [Branch l () r | l  <- t1, r <- t1]
    | otherwise = concat $ [[Branch l () r, Branch r () l] | l  <- t1, r <- t2]
    where
        t1 = generateBalancedTrees2 $ div (n-1) 2
        t2 = generateBalancedTrees2 $ div n 2

                                
-- 8. Напишите функцию, проверяющую, является ли дерево струк- турно симметричным относительно корня (значения в узлах не важны).
isSymmetric :: (Tree a) -> Bool
isSymmetric Empty = True
isSymmetric (Branch l _ r) = mirror l r where
    -- Проверяем, являются ли 2 дерева зеркальными отраженяями друг друга: iff
    --  правая ветка первого есть зеркальное отражение  левой ветки второго
    -- и левая ветка первого есть зеркальное отражение правой ветки второго
    mirror :: (Tree a) -> (Tree a) -> Bool
    mirror Empty Empty = True
    mirror (Branch l1 _ r1) (Branch l2 _ r2) = mirror r1 l2 && mirror l1 r2
    mirror _ _ = False  
    
-- 9. Напишите функцию строющую из списка дерево бинарного поиска.
listToBST :: Ord a => [a] -> Tree a
listToBST l = apply add l Empty where
    -- функция, добавляющая в дерево поиска элемент.
    -- Смотрим в какое поддерево его нужно добавить и добавляем его туда.
    add :: Ord a => Tree a -> a -> Tree a
    add Empty x = Branch Empty x Empty
    add t@(Branch l v r) x
        | x < v = Branch (add l x) v r
        | x > v = Branch l v (add r x)
        | otherwise = t
    -- фукнция, которая к каждому элементу списка применяет некую функцию и накапливает результат
    -- нам это нужно чтобы пройтись по списку и каждый элемент добавить в дерево поиска, которое в начале Empty
    apply :: (a -> b -> a) -> [b] -> a -> a
    apply f xs z = apply' xs z where
        apply' [] z = z
        apply' (x:xs) z = apply' xs (f z x)
        
-- 10. Напишите функцию, возвращающую список всех сбалансированных по высоте деревьев типа Tree () с n узлами.

-- Fibonacci numbers. Честно, не мое, но понравилось
fibs :: [Int]
fibs = 0 : 1 : next fibs where next (a : t@(b:_)) = (a+b) : next t

-- минимально возможное число узлов с высотой h -- это числа Фибоначчи:
-- чтобы построить минимальное дерево высотой h, нужно слева подвесить дерево высотой h-1, а справа h-2                
minNodes h = fibs !! (h+2) - 1
-- наоборот: максимально возможная высота дерева с n узлами -- берем числа Фибоначчи пока можно
maxHeight n = length (takeWhile (<= n+1) fibs) - 3
-- нижняя граница, очевидно, логарифм
minHeight n = ceiling (logBase 2 (fromIntegral (n+1)))

-- Строит все сбалансированные по высоте деревья высотой h и числом внутренних узлов n
hbTree :: Int -> Int -> [Tree ()]
hbTree 0 n = [Empty]
hbTree 1 n = [Branch Empty () Empty]
hbTree h n = [Branch l () r |
                 -- определяем высоты поддеревьев
                (hleft, hright) <- [(h-1, h-1), (h-2, h-1), (h-1, h-2)],
                 -- ищем сколько может быть узлов в левом поддереве
                leftSize <- [max (minNodes hleft) (n - 1 - (maxNodes hright)) .. min (maxNodes hleft) (n - 1 - (minNodes hright))],
                -- строим соответствующие поддеревья
                l <- hbTree hleft leftSize, r <- hbTree hright (n-1-leftSize)] where
                -- максимально возможное число узлов в дереве
                maxNodes x = 2^x - 1

-- ответ: перебираем все высоты и строим дерево
heightBalancedTrees :: Int -> [Tree ()]
heightBalancedTrees n = [t | h <- [minHeight n .. maxHeight n], t <- hbTree h n]