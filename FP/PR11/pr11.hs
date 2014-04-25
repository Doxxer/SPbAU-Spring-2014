module PR11 where
import Control.Monad.Error

data ParseError = Err { location::Int, reason::String }
    deriving (Show)

instance Error ParseError where
  noMsg    = Err 0 "Parse Error"
  strMsg s = Err 0 s

type ParseMonad = Either ParseError

parseHex :: String -> ParseMonad Integer
parseHex s = do
    throwError $ Err 0 "asdasd"
    return 3
    
printError :: ParseError -> ParseMonad String
printError err = do
    return $ show err


test s = str where
    Right str = do 
        n <- parseHex s
        return $ show n
        `catchError` printError

main :: IO()
main = do
    print $ "------ 1 ----------"
    print $ test "2123"
    