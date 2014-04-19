module Main where
import Control.Monad.Identity
import System.Random
import System.IO
import Data.List
import Text.Printf

formatted :: Int -> String
formatted n = printf "%3d" n

getSample :: IO Int
getSample = do
    q <- (replicateM 1000 $ randomRIO (0, 1)) :: IO [Int]
    return $ 500 - sum q

runExperiment :: IO [Int]
runExperiment = replicateM 1000 $ getSample

ct :: (Ord a) => [a] -> [(a, Int)]
ct = map (\l -> (head l, length l)) . group . sort

clear file = do
    handle <- openFile file WriteMode
    hPutStr handle ""
    hClose handle

write [] file = do
    return ()

write (x:xs) file = do
    let title = fst x
    let count = snd x
    let str = formatted title ++ " " ++ (concat $ replicate count "x")
    hPutStrLn file str
    write xs file
    return ()

main :: IO ()
main = do
    xs <- runExperiment
    let file = "6.txt"
    clear file    
    handle <- openFile file AppendMode
    write (ct xs) handle
    hClose handle