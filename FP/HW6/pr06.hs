module FP06 where
    
data Tree a = Leaf a | Branch (Tree a) a (Tree a)
--    deriving (Show)

-- 0. Полезные функции для дерева    
getRoot :: Tree a -> a
getRoot (Branch l v r) = v
getRoot (Leaf v) = v

getChildren :: Tree a -> [Tree a]
getChildren (Branch l v r) = [l, r]
getChildren (Leaf v) = []

-- 1. Сделайте тип Tree a представителем класса типов Eq.
instance (Eq a) => Eq (Tree a) where
    Leaf a == Leaf b = a == b
    Branch a b c == Branch a' b' c' = a == a' && b == b' && c == c'
    _ == _ = False




-- 2. Реализуйте функцию elemTree, определяющую, хранится ли заданное значение в заданном дереве.
elemTree :: (Eq a) => a -> Tree a -> Bool
elemTree x t = elem x (flatten t)

flatten :: Tree a -> [a]
flatten x = flatten' [x] where
        flatten' :: [Tree a] -> [a]
        flatten' [] = []
        flatten' x = (map getRoot x) ++ (flatten' $ concat $ map getChildren x)

-- example
rinf n = Branch (Leaf (n+1)) n (rinf (n+2))
linf n = Branch (linf (n+2)) n (Leaf (n+1))
t = Branch (Branch (Leaf 9) 7 (Leaf 8)) 5 (Branch (Leaf 2) 3 (Leaf 1))





-- 3. Напишите версию instance Show для типа List a, так чтобы они выводили список в следующем виде
data List a = Nil | Cons a (List a)
    deriving (Show)
l = Cons 2 (Cons 3 (Cons 5 Nil))
 
-- instance Show a => Show (List a) where
--     showsPrec _ = myShowsList
myShowsList :: Show a => List a -> ShowS
myShowsList Nil         = ("|" ++)
myShowsList (Cons x xs) = showChar '<' . shows x . myShowsList xs . (showChar '>')




-- 4. Напишите две версии instance Show для типа Tree a, так чтобы они выводили дерево в следующем виде
-- <1{2}<3{4}5>>
t1 = Branch (Leaf 1) 2 (Branch (Leaf 3) 4 (Leaf 5))
t0 = Leaf 5
t00 = Branch (Leaf 1) 2 (Leaf 3)

instance Show a => Show (Tree a) where
--    show = myShowTree1
    showsPrec _ = myShowTree2
    
myShowTree1 :: Show a => Tree a -> String
myShowTree1 (Leaf a) = show a
myShowTree1 (Branch l v r) = "<" ++ myShowTree1 l ++ "{" ++ show v ++ "}" ++ myShowTree1 r ++ ">"

myShowTree2 :: Show a => Tree a -> ShowS
myShowTree2 (Leaf a) = (shows a)
myShowTree2 (Branch l v r) = showChar '<' . myShowTree2 l . showChar '{' . shows v . showChar '}' . myShowTree2 r . showChar '>'




-- 5. Напишите функцию myReadsTree :: (Read a) => ReadS (Tree a) так чтобы
-- *Test> (myReadsTree "<1{2}<3{4}5>> something else") :: [(Tree Int, String)]
-- [(Branch (Leaf 1) 2 (Branch (Leaf 3) 4 (Leaf 5))," something else")]

myReadsList :: (Read a) => ReadS (List a)
myReadsList ('|':s) = [(Nil, s)]
myReadsList ('<':s) = [(Cons x l, u) | (x, t) <- reads s,
                                     (l, '>':u) <- myReadsList t ]
                                     
myReadsTree :: (Read a) => ReadS (Tree a)
myReadsTree ('<':s) = [(Branch l v r, q) | (l, '{':r1) <- myReadsTree s,
                                      (v, '}':r2) <- reads r1,
                                      (r, '>':q) <- myReadsTree r2]
myReadsTree s = [(Leaf q, u) | (q, u) <- reads s]