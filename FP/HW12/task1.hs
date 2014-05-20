module Main where
import Data.List
import Control.Monad.Error

data TIError = Err { getMessage::String }
    deriving (Show)

instance Error TIError where
    noMsg    = Err "Type inference error"
    strMsg s = Err s

type TIErrorMonad = Either TIError

type Sym = String

infixl 2 :@
data Expr = Var Sym | Expr :@ Expr | Lam Sym Expr
        deriving (Eq, Read, Show)

infixr 2 :→
data Type = TVar Sym | Type :→ Type
    deriving (Eq, Read)

instance Show Type where
    show (TVar a) = a
    show (a :→ b) = "(" ++ show a ++ " → " ++ show b ++ ")"

newtype Env = Env [(Sym, Type)]
    deriving (Show)
    
emptyEnv :: Env
emptyEnv = Env []

freeTVars :: Type -> [Sym]
freeTVars (TVar v) = [v]
freeTVars (a :→ b) = freeTVars a `union` freeTVars b

substitution :: Env -> Type -> Type
substitution (Env e) t@(TVar v) = case lookup v e of
        Just x -> x
        otherwise -> t
substitution e (a :→ b) = (substitution e a) :→ (substitution e b)

composition :: (TIErrorMonad Env) -> Env -> (TIErrorMonad Env)
composition (Right e1) e2 = return $ merge e1 (merge e2 (unionEnv e1 e2))
    where 
        merge :: Env -> Env -> Env
        merge e1 (Env e2) = Env $ map (\(v, t) -> (v, substitution e1 t)) e2        
        unionEnv :: Env -> Env -> Env
        unionEnv (Env x) (Env y) = Env $ map (\v -> (v, TVar v)) (map fst x `union` map fst y)        
composition (Left error) _ = throwError $ error

algoU :: Type -> Type -> TIErrorMonad Env
algoU (TVar a) (TVar b) | a == b = return emptyEnv
algoU (TVar a) tau
    | elem a (freeTVars tau) = throwError $ Err ("Type inference failed: '" ++ a ++ "' in FV of " ++ show tau)
    | otherwise = return $ Env $ [(a, tau)]
algoU (a :→ b) v@(TVar q) = algoU v (a :→ b)
algoU (a :→ b) (c :→ d) = calc (algoU b d)
    where
        calc :: (TIErrorMonad Env) -> (TIErrorMonad Env)
        calc (Left error) = throwError $ error
        calc (Right env) = composition (algoU (substitution env a) (substitution env c)) env

[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z] = map (TVar . (:[])) "abcdefghijklmnopqrstuvwxyz"

main :: IO()
main = do
    let sigma = b :→ b
    let tau = ((g :→ d) :→ e) :→ (a :→ d)
    let result = algoU (b :→ b) (((g :→ d) :→ e) :→ (a :→ d))   
    case result of
        Left error -> putStrLn $ getMessage error
        Right (Env a) -> putStrLn $ show a
    
    let result = algoU (b :→ b) (b)
    case result of
        Left error -> putStrLn $ getMessage error
        Right (Env a) -> putStrLn $ show a