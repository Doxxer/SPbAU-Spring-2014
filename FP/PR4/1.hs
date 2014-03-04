triple0 = \x -> x * 3
triple1 x = x * 3
triple = (* 3)

sign x
    | x > 0 = 1
    | x < 0 = -1
    | otherwise = 0
    
nor False False = True
nor _ _ = False

nand True True = False
nand _ _ = False

fib n = helper 0 1 n
    where helper prev acc n | n > 1 = helper acc (acc + prev) (n-1)
                            | n == 0 = 0
                            | otherwise = acc
                            
flip f x y = f y x  