module HW08 where
import Data.List

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap
infixl 4 <$>

class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
infixl 4 <*>

newtype ZipList a = ZipList { getZipList :: [a] }

instance Functor ZipList where
        fmap f (ZipList xs) = ZipList (map f xs)

instance Applicative ZipList where
        pure x = ZipList (repeat x)
        ZipList fs <*> ZipList xs = ZipList (zipWith ($) fs xs)
        
instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> x = fmap f x
    
instance Applicative [] where
    pure x = [x]
    fs <*> xs = [ f x | f <- fs, x <- xs ]
    
data Tree a = Nil | Branch (Tree a) a (Tree a)
    deriving (Show)

instance Functor Tree where
  fmap f Nil = Nil
  fmap f (Branch l x r) = Branch (fmap f l) (f x) (fmap f r)
  
-- 1. instance Applicative Tree
instance Applicative Tree where
    pure x = Branch Nil x Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Branch fl fx fr) <*> (Branch l x r) = Branch (fl <*> l) (fx x) (fr <*> r)

-- 2. Реализуйте операторы (>*<) и (>$<), позволяющие спрятать упаковку ZipList и распаковку getZipList:
(>*<) :: [a -> b] -> [a] -> [b]
fs >*< xs = getZipList $ ZipList fs <*> ZipList xs
infixl 4 >*<

(>$<) :: (a -> b) -> [a] -> [b]
f >$< xs = getZipList $ f <$> ZipList xs
infixl 4 >$<

-- 3. 
newtype Compose f g x = Compose { getCompose :: f (g x) }

-- Каков кайнд этого конструктора типов?
-- kind : f :: * -> *, g :: * -> *, x :: *
-- Значит, Compose :: (* -> *) -> (* -> *) -> * -> *

-- Приведите пример замкнутого типа, сконструированного с помощью Compose
-- Compose ((->) Integer) ((->) Integer) Integer
t3 = Compose (\x y -> 2*x + 3*y)
-- print $ getCompose t3 10 20 --------------- 80

ffmap h = getCompose . fmap h . Compose
-- ffmap :: Functor (Compose f g) => (a -> x) -> f (g a) -> f (g x)
-- Я это понимаю так:
-- Это значит, что есть какая-то функция типа (a -> x), и есть контейнер в контейнере f (g a),
-- И действие ffmap -- изменить самый вложенный контейнер, преобразовать из a в x

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose x) = Compose (fmap (fmap f) x)
    
instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure = Compose . pure . pure
    Compose f <*> Compose x = Compose (fmap (<*>) f <*> x)

main :: IO()
main = do
    putStrLn "------- 1 --------"
    print $ (+) <$> Branch (Branch Nil 1 Nil) 2 Nil <*> Branch (Branch Nil 3 Nil) 4 (Branch Nil 5 Nil)
    
    putStrLn "------- 2 --------"
    let x1s = [1,2,3]
    let x2s = [4,5,6]    
    let x3s = [7,8,9]
    let x4s = [10,11,12]
    
    print $ getZipList $ (\a b -> 2*a+3*b) <$> ZipList x1s <*> ZipList x2s
    print $ (\a b -> 2*a+3*b) >$< x1s >*< x2s
    
    print $ getZipList $ (\a b c -> 2*a+3*b+5*c) <$> ZipList x1s <*> ZipList x2s <*> ZipList x3s
    print $ (\a b c -> 2*a+3*b+5*c) >$< x1s >*< x2s >*< x3s
    
    print $ zipWith4 (\a b c d -> 2*a+3*b+5*c-4*d) x1s x2s x3s x4s
    print $ getZipList $ (\a b c d -> 2*a+3*b+5*c-4*d) <$> ZipList x1s <*> ZipList x2s <*> ZipList x3s <*> ZipList x4s
    
    putStrLn "------- 3 --------"
    print $ getCompose t3 10 20 -- 80
    print $ ffmap (+42) $ Just [1,2,3]
    print $ ffmap (+42) $ [Just 1, Just 2, Nothing]    
    print $ getCompose $ (+) <$> Compose [Just 1,Just 2] <*> Compose [Nothing,Just 40]
    print $ getCompose $ (+) <$> Compose [Just 1,Just 2] <*> Compose [Just 30,Just 40]
    print $ getCompose $ Compose [[(+1)],[(+2),(+3)]] <*> Compose [[10,20],[]]