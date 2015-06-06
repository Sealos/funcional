\begin{code}
import System.Random
import Control.Monad
import Data.Sequence as DS hiding (replicateM, zip)
import Control.Concurrent
import Control.Concurrent.MVar
import System.Posix.Signals

randomSeed :: Int
randomSeed = 42

type Buffer = MVar (DS.Seq String)
type Bowl = MVar Int	-- Cuantas empanadas hay en el bowl
type Total = MVar Int
type Stall = MVar Bool

newCounter :: IO (MVar Int)
newCounter = newMVar 0

inc x = x + 1
dec x = x - 1
incM m x = x + m

newBuffer :: IO (Buffer)
newBuffer = newMVar DS.empty

printInfo total counters rTID pTID mTID = do
	buffer <- newBuffer
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
			counters <- replicateM n newCounter
			bowl <- newCounter
			total <- newCounter
			buffer <- newBuffer
			stall <- newMVar True
			mainTID <- myThreadId
			parroquianosTID <- forM [1..n] $ (\i ->
				forkIO $ parroquiano i (counters !! (i - 1)) bowl buffer stall True)
			rafitaTID <- forkIO $ rafita m bowl total stall buffer

			installHandler sigINT (Catch (printInfo total counters rafitaTID parroquianosTID mainTID)) Nothing
			output buffer


output :: Buffer -> IO b
output buffer = do
		str <- takeMVar buffer
		case viewl str of
			EmptyL				-> do
										putMVar buffer str
										threadDelay 1
										output buffer
			item :< rest	-> do
										putStrLn item
										putMVar buffer rest
										output buffer

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


rafita :: Int -> Bowl -> Total -> Stall -> Buffer -> IO b
rafita m bowl total stall buffer = forever $ do
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
