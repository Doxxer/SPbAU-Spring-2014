module Main where
import Control.Monad.Error
import Data.Char

data ParseError = Err { location::Int, reason::String }
    deriving (Show)

instance Error ParseError where
    noMsg    = Err 0 "Parse Error"
    strMsg s = Err 0 s

type ParseMonad = Either ParseError

parseHex :: String -> ParseMonad Int
parseHex s = parse' s 1 0
    where
    parse' :: String -> Int -> Int -> ParseMonad Int
    parse' [] pos res = do
        return res
    parse' (x:xs) pos res = do
        if not (isHexDigit x)
        then throwError $ Err pos ([x] ++ ": invalid digit")
        else parse' xs (pos + 1) (res * 16 + (digitToInt x))

test s = str
    where
    Right str = action `catchError` printError
        where
        action = do {n <- parseHex s; return $ show n }
        printError :: ParseError -> ParseMonad String
        printError err = do
        return $ "At pos " ++ show (location err) ++ ": " ++ (reason err)

main :: IO()
main = do
    putStrLn $ test "0x102fc"
    putStrLn $ test "1"
    putStrLn $ test "0"
    putStrLn $ test "100"
    putStrLn $ test "FFAF"
    putStrLn $ test ""
    putStrLn $ test "DEADBEEF"
    putStrLn $ test "DEADMEAT"
    