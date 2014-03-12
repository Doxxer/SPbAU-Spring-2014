module Pr04 where

-- 1. Реализуйте функцию, описывающую плотность равномерного распределения на промежутке от 3 до 4.
f1 x
    | x >= 3 && x <= 4 = 1
    | otherwise = 0
    
-- 2. Реализуйте функцию, находящую наибольший общий делитель двух целых чисел с помощью алгоритма Евклида:
gcd' 0 0 = error "gcd undefined"
gcd' a b = gcd'' (abs a) (abs b) where
    gcd'' a 0 = a
    gcd'' a b = gcd'' b $ a `mod` b
      
-- 3.1 Реализуйте функцию, находящие сумму цифр заданного целого числа.
sumDigits a = sumDigits' $ abs a where
    sumDigits' 0 = 0
    sumDigits' a = (a `mod` 10) + (sumDigits' $ a `div` 10)

-- 3.2 Реализуйте функцию, находящи количество цифр заданного целого числа.    
countDigits 0 = 1
countDigits a = countDigits' $ abs a where
    countDigits' 0 = 0
    countDigits' a = 1 + (countDigits' $ a `div` 10)
    
-- 4. Реализуйте функцию, находящую элементы следующей рекуррентной последовательности a[0] = 1; a[1] = 2; a[2] = 3;
-- a[k+3] =a [k+2] + a[k+1] − 2*a[k];
f2 n = helper 1 2 3 n
    where helper prev2 prev1 acc n | n > 2 = helper prev1 acc (acc + prev1 - 2*prev2) (n-1)
                                   | n == 2 = acc
                                   | n == 1 = prev1
                                   | n == 0 = prev2
                                   | otherwise = error "undefined"

-- 5. Реализуйте функцию, находящую значение определённого интеграла от заданной функции на заданном интервале по методу трапеций.
-- (параметр n -- точность)
f3 f a b n | n <= 0 = error "n must be positive"
           | b < a = (-1) * (helper f b a 0 n)
           | otherwise = helper f a b 0 n
           where helper f a b acc n | n > 0 = helper f (a + h) b (acc + ps * h) (n-1)
                                    | otherwise = acc
                                    where h = (b - a) / n
                                          ps = (f a + f (a + h)) / 2.0
                                                                   
-- пример: 
-- > f3 (\x -> x*x) (-3) 3 10000
-- > ~18.0

-- 6. Реализуйте функцию, находящую значение квадратного корня методом Ньютона.
sqrt' n | n == 0 = 0
        | n < 0 = 0/0
        | otherwise = until (check) (improve) 1
                where
                    improve ans = (ans + n/ans)/2.0
                    check ans = abs (n - ans**2) <= 10**(-9)