module FP05 where
-- Practic part 1 ------------------------------------------
-- Найдите количество четных элементов в заданном списке.
evenCount :: (Integral a) => [a] -> Int
evenCount [] = 0
evenCount (x:xs) | x `mod` 2 == 0 = 1 + (evenCount xs)
                 | otherwise = (evenCount xs)

-- Сформируйте новый список, содержащий только нечетные элементы исходного.                 
odds :: (Integral a) => [a] -> [a]
odds [] = []
odds (x:xs) | x `mod` 2 == 1 = x : odds xs
            | otherwise = odds xs
            
-- Сформируйте новый список, в котором переставлены местами четные и нечетные (по порядку следования) элементы исходного.
shake :: [a] -> [a]
shake (x:y:xs) = y:x:shake xs
shake xs = xs

-- Даны два списка целых чисел. Сформируйте список, каждый элемент которого равен сумме соответствующих элементов исходных списков.
sumLists :: (Num a) => [a] -> [a] -> [a]
sumLists (x:xs) (y:ys) = (x+y):(sumLists xs ys)
sumLists _ _ = []


-- Поменяйте порядок элементов списка на противоположный. (see rev)
addToTail :: a -> [a] -> [a]
addToTail x [] = [x]
addToTail x (y:ys) = y:(addToTail x ys)

rev :: [a] -> [a]
rev (x:xs) = addToTail x (rev xs)
rev [] = []

-- Сформируйте список, содержащий подсписок длины n исходного списка, начиная с k-го элемента исходного.
take' :: Int -> [a] -> [a]
take' n _ | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : (take' (n-1) xs)

skip :: Int -> [a] -> [a]
skip n x | n <= 0 = x
skip _ [] = []
skip n (x:xs) = skip (n-1) xs

-- Practic part 2 ------------------------------------------
-- Дан список целых чисел. Увеличить все его элементы в два раза.
mult2 :: (Num a) => [a] -> [a]
mult2 x = map (*2) x

-- Дан список целых чисел. Увеличить все его элементы с четными значениями в два раза.            
evenMult2 :: (Integral a) => [a] -> [a]
evenMult2 x = map (evenMult2') x
    where evenMult2' :: (Integral a) => a -> a
          evenMult2' x | even x = x*2
                       | otherwise = x

-- Дан список целых чисел. Обнулить все его элементы с нечетными значениями.                      
zeroOdd :: (Integral a) => [a] -> [a]
zeroOdd x = map (zeroOdd') x
    where zeroOdd' :: (Integral a) => a -> a
          zeroOdd' x | odd x = 0
                     | otherwise = x

-- Дан список целых чисел. Построить список пар: элемент, его порядковый номер.
-- Хоть и существует оригинальный зип, но выглядел бы он, наверное, так
zip' :: [x] -> [y] -> [(x,y)]
zip' (x:xs) (y:ys) = (x,y) : zip xs ys
zip' _ _ = []

enumerate :: (Enum a, Num a) => [b] -> [(a, b)]
enumerate x = zip' [1..] x

-- Дан список целых чисел. Обнулить все его элементы с нечетными порядковыми номерами.
zeroOdd' :: (Num a) => [a] -> [a]
zeroOdd' (x:y:xs) = 0:y:zeroOdd' xs
zeroOdd' [x] = [0]
zeroOdd' _ = []

-- Дан список целых чисел. Удалить из него элементы, большие заданного числа k.
delGreater :: (Integral a) => a -> [a] -> [a]
delGreater k (x:xs) | x > k = delGreater k xs
                    | otherwise = x:delGreater k xs
delGreater _ _ = []

-- Даны три списка целых чисел. Составить список сумм соответствующих элементов этих списков.
sum3Lists :: (Integral a) => [a]->[a]->[a]->[a]
sum3Lists (a:as) (b:bs) (c:cs) = (a+b+c) : sum3Lists as bs cs
sum3Lists _ _ _ =  []

-- Или так: 
sum3Lists' :: (Integral a) => [a]->[a]->[a]->[a]
sum3Lists' a b c = zipWith3 (\x y z -> x+y+z) a b c