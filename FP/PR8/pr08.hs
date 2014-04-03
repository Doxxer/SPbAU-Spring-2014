module PR08 where
--import Control.Applicative

-- class Functor f => Applicative f where
--     pure :: a -> f a
--     (<*>) :: f (a -> b) -> f a -> f b
-- infixl 4 <*>
-- 
-- (<$>) :: Functor f => (a -> b) -> f a -> f b
-- (<$>) = fmap
-- 
-- instance Applicative ((->)e) where
--     pure = const
--     (<*>) f g x = f x (g x)
--     
-- instance Applicative (Either a) where
--     pure = Right
--     Right g <*> Right x = Right (g x)
--     Left g <*> Right x = Left g
--     Right g <*> Left x = Left x
--     Left g <*> Left x = Left g

main :: IO()
main = do
    --putStrLn $ "OK"
    print $ Right 2