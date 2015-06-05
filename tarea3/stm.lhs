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
type Stall = TVar Bool

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

randomDelay lo hi = do
						r <- randomRIO (lo,hi)
						threadDelay r

rafita ::  Int -> Bowl -> Total -> Stall -> Buffer -> IO ()
rafita m bowl total stall buffer = do
						b <- readTVarIO bowl
						s <- readTVarIO stall
						if b > 0 || s then 
							rafita m bowl total stall buffer
						else
							do
								atomically $ writeTVar stall True >> put buffer "Rafita esta cocinando"
								randomDelay 3000000 5000000
								atomically $ do
											modifyTVar' total (incM m)
											writeTVar bowl m
											put buffer "Rafita sirvio las empanadas"
											writeTVar stall True
								rafita m bowl total stall buffer


-- Si un parroquiano tiene hambre, pero no hay empanadas, le avisa a Rafita para que prepare más.

--parroquiano :: Int -> Bowl -> STM ()
parroquiano i counter bowl buffer stall bool = do
					if bool then
						do
							atomically $ put buffer $ "Parroquiano " ++ (show i) ++ " tiene hambre "
							comer
					else
						comer
						
					where
						agarrarEmpanada = do
							b <- readTVar bowl
							if b > 0 then
								do
									writeTVar bowl $ b - 1
									modifyTVar' counter inc
									put buffer $ "Parroquiano " ++ (show i) ++ " come empanada"
							else
								return ()
						comer = do
							pre <- readTVarIO counter
							atomically $ agarrarEmpanada
							post <- readTVarIO counter
							if pre == post then
								do 
									atomically $ writeTVar stall False
									parroquiano i counter bowl buffer stall False
							else 
								do
									randomDelay 1000000 7000000
									parroquiano i counter bowl buffer stall True

transactional :: Int -> Int -> IO ()
transactional m n = do
					setStdGen $ mkStdGen randomSeed
					counters <- replicateM n newCounter
					bowl <- newCounter
					total <- newCounter
					stall <- newTVarIO True
					buffer <- newBuffer
					forM_ [1..n] $ (\i ->
						forkIO $ parroquiano i (counters !! (i - 1)) bowl buffer stall True)
					forkIO $ rafita m bowl total stall buffer
					output buffer

-- Outputs the whole buffer to console
output buffer = 
    do str <- atomically $ get buffer
       putStrLn str
       output buffer
\end{code}