\begin{code}
import System.Random
import Control.Monad
import Data.Sequence as DS hiding (replicateM, zip)
import Control.Concurrent
import Control.Concurrent.MVar
import System.Posix.Signals

randomSeed :: Int
randomSeed = 42

inc x = x + 1
dec x = x - 1
incM m x = x + m

printInfoC total counters rTID pTID mTID = do
  t <- readMVar total
  cs <- mapM readMVar counters
  forM_ pTID (\t -> killThread t)
  killThread rTID
  threadDelay 200000
  putStrLn $ "\n\nRafita preparo " ++ (show t) ++ " empanadas"
  let ps = zip cs [1..]
  forM ps (\(c, i)->
      putStrLn $ "Parroquiano " ++ (show i) ++ ":\t" ++ (show c)
    )
  putStrLn $ "Total: " ++ (show $ sum cs)

  killThread mTID

classic :: Int -> Int -> IO ()
classic m n = do
      setStdGen $ mkStdGen randomSeed
      counters <- replicateM n newMVar 0
      bowl <- newMVar 0
      total <- newMVar 0
      buffer <- newMVar DS.empty
      stall <- newMVar True
      mainTID <- myThreadId
      parroquianosTID <- forM [1..n] $ (\i ->
        forkIO $ parroquiano i (counters !! (i - 1)) bowl buffer stall True)
      rafitaTID <- forkIO $ rafitaC m bowl total stall buffer

      installHandler sigINT (Catch (printInfoC total counters rafitaTID parroquianosTID mainTID)) Nothing
      outputC buffer

outputC buffer = do
    str <- takeMVar buffer
    case viewl str of
      EmptyL        -> do
                    putMVar buffer str
                    threadDelay 1
                    outputC buffer
      item :< rest  -> do
                    putStrLn item
                    putMVar buffer rest
                    outputC buffer

randomDelay :: Int -> Int -> IO ()
randomDelay lo hi = do
        r <- randomRIO (lo,hi)
        threadDelay r

parroquiano :: Int -> Total -> Bowl -> Buffer -> Stall -> Bool -> IO b
parroquiano i counter bowl buffer stall bool = do
  if bool then
    do
      buff <- takeMVar buffer
      putMVar buffer (buff |> ("Parroquiano " ++ (show i) ++ " tiene hambre"))
      comer
  else
    comer

  where
    agarrarEmpanada = do
      b <- takeMVar bowl
      if b > 0 then
        do
          c <- takeMVar counter
          putMVar counter $ c + 1
          buff <- takeMVar buffer
          putMVar buffer (buff |> ("Parroquiano " ++ (show i) ++ " come empanada"))
          putMVar bowl $ b - 1
      else
        putMVar bowl 0

    comer = do 
      pre <- readMVar counter
      agarrarEmpanada
      post <- readMVar counter
      if pre == post then
        do
          -- Avisar que quiero comer
          takeMVar stall
          putMVar stall False
          parroquiano i counter bowl buffer stall False
      else
        do
          randomDelay 1000000 7000000
          parroquiano i counter bowl buffer stall True

rafitaC m bowl total stall buffer = forever $ do
  b <- takeMVar bowl
  s <- takeMVar stall
  if b > 0 || s then
    do
      putMVar stall s
      putMVar bowl b
  else
    do
      t <- takeMVar total
      buff <- takeMVar buffer
      putMVar buffer (buff |> "Rafita esta cocinando")
      randomDelay 3000000 5000000
      buff2 <- takeMVar buffer
      putMVar buffer (buff |> "Rafita sirvio las empanadas")
      putMVar total $ t + m
      putMVar stall True
      putMVar bowl m
    

\end{code}
