\begin{code}
import System.Random
import Control.Monad
import Data.Sequence as DS hiding (replicateM)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.MVar

randomSeed :: Int
randomSeed = 42

classic :: Int -> Int -> IO ()
classic m n = undefined

transactional :: Int -> Int -> IO ()
transactional m n = undefined

\end{code}