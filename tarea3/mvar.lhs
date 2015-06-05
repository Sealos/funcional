\begin{code}
import System.Random
import Control.Monad
import Data.Sequence as DS hiding (replicateM)
import Control.Concurrent
import Control.Concurrent.MVar

randomSeed :: Int
randomSeed = 42

newCounter :: IO (MVar Int)
newCounter = newMVar 0

newBuffer :: IO (Buffer)
newBuffer = newTVarIO DS.empty

classic :: Int -> Int -> IO ()
classic m n = do
			counters <- replicateM n newCounter
			bowl <- newCounter
			total <- newCounter
			buffer <- newBuffer
			randomDelay 300000 500000
			forM_ [1..n] $ (\i ->
				forkIO $ parroquiano m i (counters !! (i - 1)) bowl total buffer)

			output buffer

randomDelay lo hi = do
				r <- randomRIO (lo,hi)
				threadDelay r

parroquiano m n bowl selfCounter total buffer = undefined


rafita m bowl total buffer = do
	b <- takeMVar bowl
	t <- takeMVar total
	buff <- takeMVar buffer
	if b > 0 then
		do
			putMVar buffer (buff |> "Rafita esta cocinando")
			randomDelay 300000 500000
			putMVar total $ t + m
			putMVar bowl $ m
	else
		do
			putMVar total $ t
			putMVar bowl $ b

\end{code}