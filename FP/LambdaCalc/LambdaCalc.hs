module LambdaCalc where

-- символы для переменных
type Sym = String 

infixl 2 :@
              
data Term = Var Sym        -- переменная
          | Term :@ Term   -- аппликация терма к терму
          | Lam Sym Term   -- абстракция терма по переменной
          deriving (Eq)

instance Show Term where
    show (Var x) = x
    show (Lam x a) = "(\\" ++ x ++ ". " ++ show a ++ ")"
    show (a :@ b) = "(" ++ show a ++ " " ++ show b ++ ")"

-- список свободных переменных в терме (fv). bv -- связанные переменные
-- просто по определению. Это лекция №1 курса (слайд 23) :)
fv :: Term -> [Sym]
fv term = helper term [] where
    helper (Var name) bv                                    -- свободные переменные в переменной это она сама
        | elem name bv = []                                 -- за исключением того случая, когда переменная свободной быть не может
        | otherwise = [name]
    helper (Lam x term) bv = helper term (x:bv)             -- свободные в лямбде это все под лямбдой кроме той, по которой абстрагируемся
    helper (t1 :@ t2) bv = (helper t1 bv) ++ (helper t2 bv) -- свободные в аппликации -- объединение свободных


-- редукция
reduction :: Term -> Term
reduction (Var x) = Var x                                      -- нечего редуцировать
reduction (Lam x term) = Lam x (reduction term)                -- редуцируем абстракцию: все что под лямбдой

--reduction ((Lam var term) :@ t2) = substitution term var (reduction t2)  -- аппликативная стратегия
reduction ((Lam var term) :@ t2) = reduction $ substitution term var t2  -- нормальная стратегия
reduction (t1 :@ t2) = (reduction t1) :@ (reduction t2)        -- редуцируем каждую часть аппликации


-- подстановка в 1) куда (терм) 2) вместо какой переменной 3) какой терм. На выходе имеем терм
-- использовалось http://en.wikipedia.org/wiki/Lambda_calculus#substitutionitution и голова
substitution :: Term -> Sym -> Term -> Term 
-- разбираем случаи
substitution t1@(Var name) v t2    -- если первый параметр переменная, то это либо
    | name == v = t2                -- что подставляли, то и получили (в случае если совпадают имена)
    | otherwise = t1                -- эта сама переменнаю, если подставляем в то, чего там нет

-- если первый параметр аппликация, то подставляем в обе части
substitution (t1 :@ t2) v t = (substitution t1 v t) :@ (substitution t2 v t)

-- если первый параметр абстрация, то подставляем в обе части. Как действовать в случае совпадения имен - открываем лекцию
substitution (Lam x lambdaTerm) v term
    | x == v = Lam x lambdaTerm                                       -- не будет лишний раз переименовывать, все равно смысл один и тот же
    | elem x (fv term) = substitution (Lam newX newLambdaTerm) v term -- если x есть в списке свободных -- переименовываем
    | otherwise = Lam x $ substitution lambdaTerm v term              -- иначе все ок, просто переименовываем абстракцию и подставляем внутрь
    where
        newX = x ++ "'"
        newLambdaTerm = substitution lambdaTerm x (Var newX)          -- пытаемся подставить переменную со штрихом

-- многошаговая бета-редукция. Редуцируем пока результат меняется        
beta :: Term -> Term
beta term
    | term == reduction term = term
    | otherwise = beta $ reduction term
         
-- для тестирования
-- компактно записанные переменные (чтобы обходиться без Var)
[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z] = map (Var . (:[])) "abcdefghijklmnopqrstuvwxyz"

-- комбинаторы
cI     = Lam "x" x
cK     = Lam "x" $ Lam "y" x
cK_ast = Lam "x" $ Lam "y" y
cB     = Lam "f" $ Lam "g" $ Lam "x" $ f :@ (g :@ x)
cS     = Lam "f" $ Lam "g" $ Lam "x" $ f :@ x :@ (g :@ x)
omega  = Lam "x" $ x :@ x
cY     = Lam "f" $ (Lam "z" $ f :@ (z :@ z)) :@ (Lam "z" $ f :@ (z :@ z)) 
omegaBig = omega :@ omega
-- Булевы значения
fls = Lam "t" $ Lam "f" f
tru = Lam "t" $ Lam "f" t
iif = Lam "b" $ Lam "x" $ Lam "y" $ b :@ x :@ y

not' = Lam "b" $ Lam "t" $ Lam "f" $  b :@ f :@ t
and' = Lam "x" $ Lam "y" $ x :@ y :@ fls
or'  = Lam "x" $ Lam "y" $ x :@ tru :@ y
-- пары
pair = Lam "x" $ Lam "y" $ Lam "f" $ f :@ x :@ y

fst' = Lam "p" $ p :@ tru
snd' = Lam "p" $ p :@ fls
-- числа Чёрча
zero  = Lam "s" $ Lam "z" z
one   = Lam "s" $ Lam "z" $ s :@ z
two   = Lam "s" $ Lam "z" $ s :@ (s :@ z)
three = Lam "s" $ Lam "z" $ s :@ (s :@ (s :@ z))
four  = Lam "s" $ Lam "z" $ s :@ (s :@ (s :@ (s :@ z)))
five  = Lam "s" $ Lam "z" $ s :@ (s :@ (s :@ (s :@ (s :@ z))))
six   = Lam "s" $ Lam "z" $ s :@ (s :@ (s :@ (s :@ (s :@ (s :@ z)))))
seven = Lam "s" $ Lam "z" $ s :@ (s :@ (s :@ (s :@ (s :@ (s :@ (s :@ z))))))
eight = Lam "s" $ Lam "z" $ s :@ (s :@ (s :@ (s :@ (s :@ (s :@ (s :@ (s :@ z)))))))
nine  = Lam "s" $ Lam "z" $ s :@ (s :@ (s :@ (s :@ (s :@ (s :@ (s :@ (s :@ (s :@ z))))))))
ten   = Lam "s" $ Lam "z" $ s :@ (s :@ (s :@ (s :@ (s :@ (s :@ (s :@ (s :@ (s :@ (s :@ z)))))))))

iszro = Lam "n" $ n :@ (Lam "x" fls) :@ tru
suc   = Lam "n" $ Lam "s" $ Lam "z" $  s :@ (n :@ s :@ z)
plus  = Lam "m" $ Lam "n" $ Lam "s" $ Lam "z" $ m :@ s :@ (n :@ s :@ z)
mult  = Lam "m" $ Lam "n" $ Lam "s" $ m :@ (n :@ s)
pow   = Lam "m" $ Lam "n" $ n :@ m


main :: IO ()
main = do
    putStrLn $ "тест переименований: "
    let r = Lam "x" $ Lam "y" $ Lam "z" $ z :@ y :@ x
    print $ beta $ r :@ y :@ x :@ cK_ast -- (\xyz.zyx) y x K* = K* x y = y 
    
    putStrLn $ "тест переименований-2: "
    let l1 = Lam "y" $ x :@ y
    let l2 = y :@ (Var "y'") :@ (Var "y''")
    print $ substitution l1 "x" l2
    print $ fv l2
    
    putStrLn $ "Проверка первого домашнего задания:"
    print $ beta $ cS :@ (cK :@ cS) :@ cK -- Да, правда, получается комбинатор B 
    print $ beta $ cS :@ cK :@ cK -- SKK = I (\x.x) Это и получилось
    
    putStrLn $ "Проверка на булевы типы: "
    print $ beta $ not' :@ tru -- F
    print $ beta $ iszro :@ one -- F
    print $ beta $ iszro :@ two -- F
    print $ beta $ iszro :@ zero -- T     
    
    putStrLn $ "Работа с числами: "
    print $ beta $ plus :@ one :@ two -- sss
    print $ beta $ pow :@ two :@ five -- 32s and z
    
    putStrLn $ "Работа с комбинаторами: "
    print $ beta $ omega :@ one -- \xy.xy == I
    --print $ beta $ omega :@ omega -- ωω             -- FAILED!  а если включить аппликативную стратегию, то сработает
    --print $ beta $ cK :@ cI :@ omegaBig -- KIΩ = I  -- FAILED!  а если включить аппликативную стратегию, то сработает