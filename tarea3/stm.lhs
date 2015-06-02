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

put :: Buffer -> String -> STM String
put = undefined

get :: Buffer -> STM String
get buffer = undefined

rafita :: Int -> STM ()
rafita = undefined

randomDelay lo hi = do
						r <- randomRIO (lo,hi)
                 		threadDelay r

--cocinar :: RandomGen g => g -> Int -> Bowl -> Total -> IO ()
cocinar g m b tt = do
					let (t, s) = delay g 3000 5000
					wait t
					return () 


-- Si un parroquiano tiene hambre, pero no hay empanadas, le avisa a Rafita para que prepare más.

parroquiano :: Int -> Bowl -> STM ()
parroquiano n = undefined

transactional :: Int -> Int -> IO ()
transactional m n = do
					counters <- replicateM n newCounter
					bowl <- newCounter
					total <- newCounter
					print "rafita cocina"
					randomDelay 3000 5000


					return ()

-- Outputs the whole buffer to console
output :: Buffer -> IO ()
output buffer = undefined
\end{code}