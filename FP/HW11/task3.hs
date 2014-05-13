module Main where
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Error
import Control.Monad.IO.Class
import Control.Monad
import Data.Char
import Data.Maybe

data PasswordError = Err { reason::String }
    deriving (Show)

instance Error PasswordError where
    noMsg    = Err "Invalid password"
    strMsg s = Err s

type MyMonad = Either PasswordError String

isValid :: String -> ErrorT String (MaybeT IO) Bool
isValid s
    | not $ any isNumber s = throwError "must be at least one digit"
    | not $ any isPunctuation s = throwError "no punctuation"
    | length s < 8 = throwError "length must be at least 8"
    | otherwise = return True

check :: ErrorT String (MaybeT IO) Bool
check = do
    s <- liftIO $ getLine
    r <- isValid s
    return r
    
getValidPassword :: MaybeT IO String
getValidPassword = do
    r <- runErrorT check
    case r of
        (Left err) -> do {liftIO $ putStrLn err; guard False; return "ok"}
        (Right pass) -> return $ "ok"
        
askPassword :: MaybeT IO ()
askPassword = do
    liftIO $ putStrLn "Insert your new password:"
    value <- msum $ repeat getValidPassword
    liftIO $ putStrLn "Storing in database..."

--main :: IO ()
main = runMaybeT askPassword
