module HW05 where
-- 1.
-- Локатор ориентирован на одну из сторон света (север, запад, юг, восток) и может принимать три команды поворота:
-- поворот налево, поворот направо, разворот
-- Определите ориентацию локатора после выполнения заданной команды, если локатор находится в заданной ориентации.
data Loc = North | East | South | West
    deriving (Show)
    
data Turn = TRight | TLeft | Around
    deriving (Show)
    
action :: Loc -> Turn -> Loc
action North TRight = East
action North TLeft = West
action North Around = South
action East TRight = South
action East TLeft = North
action East Around = West
action South TRight = West
action South TLeft = East
action South Around = North
action West TRight = North
action West TLeft = South
action West Around = East

-- 2.1 Сформируйте список цифр заданного натурального числа.
digits :: (Integral a) => a -> [a]
digits 0 = []
digits x = digits (div x 10) ++ [mod x 10]

-- вспомогательная функция
has :: (Eq a) => [a] -> a -> Bool
has [] _ = False
has (x:xs) y | x == y = True
             | otherwise = has xs y

-- 2.2 Определите, содержит ли заданное натуральное число все цифры от 1 до 9.
containsAllDigits :: (Integral a) => a -> Bool
containsAllDigits a = not $ has (map (has $ digits a) [1..9]) False

-- 2.3 Определите, содержит ли заданное натуральное число все цифры от 1 до 9 в точности по одному разу.
containsAllDigitsOnlyOne :: (Integral a) => a -> Bool
containsAllDigitsOnlyOne a = (containsAllDigits a) && ((length $ filter (>0) (digits a)) == 9)

-- 3. Дан список (возможно бесконечный) [a1, a2, ...] и целое число n.
-- Создайте спиок «скользящих» подсписков длины n, то есть список списков следующего вида:
-- [[a1, ..., an], [a2, ..., a(n+1)], [a3, ..., a(n+2)], ...]
movingLists :: Int -> [a] -> [[a]]
movingLists k s@(x:xs)
    | length part == k = part : (movingLists k xs)
    | otherwise = []
    where part = take k s
movingLists _ [] = []

-- 4. Определите рекурсивный тип для представления узла бинарного дерева. 
data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Show)
    
tree1 = Node 50 (Node 20 Empty Empty) (Node 10 Empty Empty)
tree2 = Node 100 Empty Empty

-- Вычисление суммы элементов дерева;
sumTree :: Tree Integer -> Integer
sumTree (Node val a b) = val + (sumTree a) + (sumTree b)
sumTree Empty = 0

-- Вычисление максимальной высоты дерева
height :: Tree a -> Int
height (Node _ a b) = 1 + (max (height a) (height b))
height Empty = 0