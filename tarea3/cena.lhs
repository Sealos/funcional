\begin{code}
import System.Random
import Control.Monad
import Data.Sequence as DS hiding (replicateM)
import Control.Concurrent
import Control.Concurrent.STM

type Semaphore = TVar Bool

newSem :: Bool -> IO Semaphore
newSem available = newTVarIO available

p :: Semaphore -> STM ()
p sem = do b <- readTVar sem
           if b
              then writeTVar sem False
              else retry

v :: Semaphore -> STM ()
v sem = writeTVar sem True

type Buffer a = TVar (DS.Seq a)

newBuffer :: IO (Buffer a)
newBuffer = newTVarIO DS.empty
 
put :: Buffer a -> a -> STM ()
put buffer item = do ls <- readTVar buffer
                     writeTVar buffer (ls |> item)
 
get :: Buffer a -> STM a
get buffer = do ls <- readTVar buffer
                case viewl ls of
                  EmptyL       -> retry
                  item :< rest -> do writeTVar buffer rest
                                     return item

simulation n = do chopsticks <- replicateM n (newSem True)
                  outputBuffer <- newBuffer
                  forM_ [0..n-1] $ \i -> 
                    forkIO (philosopher i outputBuffer
                            (chopsticks!!i) 
                            (chopsticks!!((i+1)`mod`n)))
                  output outputBuffer

output buffer = 
    do str <- atomically $ get buffer
       putStrLn str
       output buffer

philosopher :: Int -> Buffer String -> Semaphore -> Semaphore -> IO ()
philosopher n out chst1 chst2 = 
    do atomically $ put out ("Filosofo " ++ show n ++ " pensando.")
       randomDelay

       atomically $ p chst1 >> p chst2

       atomically $ put out ("Filosofo " ++ show n ++ " comiendo.")
       randomDelay

       atomically $ v chst1 >> v chst2

       philosopher n out chst1 chst2

randomDelay = do r <- randomRIO (100000,500000)
                 threadDelay r
\end{code}