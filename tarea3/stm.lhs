\begin{code}
import System.Random
import Control.Monad
import Data.Sequence as DS hiding (replicateM)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.MVar
import Data.Sequence as DS hiding (replicateM)
--import System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)

randomSeed :: Int
randomSeed = 42

type Buffer = TVar (DS.Seq String)
type Bowl = TVar Int	-- Cuantas empanadas hay en el bowl
type Total = TVar Int

newCounter :: IO (TVar Int)
newCounter = newTVarIO 0

newBuffer :: IO (Buffer)
newBuffer = newTVarIO DS.empty

randGen :: StdGen
randGen = mkStdGen randomSeed

delay :: RandomGen g => g -> Int -> Int -> (Int, g)
delay seed lo hi = randomR (lo, hi) seed

wait t = threadDelay t

put :: Buffer -> String -> STM ()
put buffer item = do ls <- readTVar buffer
                     writeTVar buffer (ls |> item)
 
get :: Buffer -> STM String
get buffer = do ls <- readTVar buffer
                case viewl ls of
                  EmptyL       -> retry
                  item :< rest -> do writeTVar buffer rest
                                     return item

inc x = x + 1
dec x = x - 1
incM m x = x + m

rafita :: Int -> STM ()
rafita = undefined

randomDelay lo hi = do
						r <- randomRIO (lo,hi)
						threadDelay r

--cocinar :: RandomGen g => g -> Int -> Bowl -> Total -> IO ()
cocinar m bowl total buffer = do
					b <- readTVar bowl
					if b > 0 then
						return ()
					else
						do 
							writeTVar bowl m
							put buffer "Rafita esta cocinando"
							modifyTVar' total (incM m)


agarrarEmpanada m bowl counter total buffer str = do
							b <- readTVar bowl
							if b > 0 then
								do
									writeTVar bowl $ b - 1
									modifyTVar' counter inc
									put buffer str
							else
								cocinar m bowl total buffer


-- Si un parroquiano tiene hambre, pero no hay empanadas, le avisa a Rafita para que prepare más.

--parroquiano :: Int -> Bowl -> STM ()
parroquiano m n bowl selfCounter total buffer = do
					atomically $ put buffer $ "Parroquiano " ++ (show n) ++ " tiene hambre"
					atomically $ agarrarEmpanada m bowl selfCounter total buffer $ "Parroquiano " ++ (show n) ++ " come empanada" 
					randomDelay 1000000 7000000
					atomically $ put buffer $ "Parroquiano " ++ (show n) ++ " bebe"
					parroquiano m n bowl selfCounter total buffer


infinite :: Int -> Int
infinite 0 = 0
infinite x = infinite (x - 1)

transactional :: Int -> Int -> IO ()
transactional m n = do
					counters <- replicateM n newCounter
					bowl <- newCounter
					total <- newCounter
					stall <- newCounter
					buffer <- newBuffer
					iszero <- atomically $ readTVar stall
					randomDelay 300000 500000
					forM_ [1..n] $ (\i ->
						forkIO $ parroquiano m i (counters !! (i - 1)) bowl total buffer)

					output buffer

-- Outputs the whole buffer to console
output buffer = 
    do str <- atomically $ get buffer
       putStrLn str
       output buffer
\end{code}