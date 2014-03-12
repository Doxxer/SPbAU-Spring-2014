module FP05 where
    
evenCount :: (Integral a) => [a] -> Int
evenCount [] = 0
evenCount (x:xs) | x `mod` 2 == 0 = 1 + (evenCount xs)
                 | otherwise = (evenCount xs)
                 
odds :: (Integral a) => [a] -> [a]
odds [] = []
odds (x:xs) | x `mod` 2 == 1 = x : odds xs
            | otherwise = odds xs
            
shake :: [a] -> [a]
shake (x:y:xs) = y:x:shake xs
shake xs = xs

sumLists :: (Num a) => [a] -> [a] -> [a]
sumLists (x:xs) (y:ys) = (x+y):(sumLists xs ys)
sumLists [] x = x
sumLists x [] = x

addToTail :: a -> [a] -> [a]
addToTail x [] = [x]
addToTail x (y:ys) = y:(addToTail x ys)

rev :: [a] -> [a]
rev (x:xs) = addToTail x (rev xs)
rev [] = []

take' :: Int -> [a] -> [a]
take' n _ | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : (take' (n-1) xs)

skip :: Int -> [a] -> [a]
skip n x | n <= 0 = x
skip _ [] = []
skip n (x:xs) = skip (n-1) xs

-----------------------------------

mult2 :: (Num a) => [a] -> [a]
mult2 x = map (*2) x

evenMult2 :: (Integral a) => a -> a
evenMult2 x | even x = x*2
            | otherwise = x
            
evenMult2' :: (Integral a) => [a] -> [a]
evenMult2' x = map (evenMult2) x