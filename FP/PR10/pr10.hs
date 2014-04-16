module PR10 where
import Data.IORef
    
testIORef = do
    x <- newIORef 1
    val1 <- readIORef x
    writeIORef x 41
    val2 <- readIORef x
    modifyIORef x succ
    val3 <- readIORef x
    return [val1,val2,val3]


main :: IO()
main = do
    print $ "------ 1 ----------"
    print $ sequence [Just 1,Just 2,Just 3]
    print $ sequence [Just 1, Just 2, Nothing,Just 4]
    print $ sequence [[1,2,3],[10,20]]
    print $ mapM (\x -> [x+1,x*2]) [10,20]
    print $ sequence_ [[1,2,3],[10,20]]
    print $ mapM_ (\x -> [x+1,x*2]) [10,20]
    
    print $ "------ 2 ----------"
    let x = print "first" in print "second"
    let x = print "first" in x >> print "second"
    (\x -> print "first") (print "second")
    print "first" `seq` print "second"
    
    print $ "------ 3 ----------":t getStdGen