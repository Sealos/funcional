\begin{code}
import Control.Monad
import Control.Applicative
import qualified Data.Sequence as Seq
import qualified Data.Set as DS
import Data.Char
import Data.Functor
import Data.Either
import Test.QuickCheck
\end{code}
\begin{code}
data Otro a = Otro ((a -> Beta) -> Beta)

data Beta = Chamba (IO Beta)
          | Convive Beta Beta
          | Quieto

instance Show Beta where
   show (Chamba x)    = " chamba "
   show (Convive x y) = " convive(" ++ show x 
                                    ++ "," 
                                    ++ show y ++ ") "
   show Quieto        = " quieto "
\end{code}

\begin{code}
hacer :: Otro a -> Beta
hacer (Otro f) = Convive (\g -> f g) Quieto

quieto :: Otro a
quieto = Otro (\_ -> Quieto)

chambea :: IO a -> Otro a
chambea x = Otro (\f -> Chamba $ fmap f x)

convive :: Otro a -> Otro ()
convive = undefined

pana :: Otro a -> Otro a -> Otro a
pana (Otro f) (Otro g) = Otro (\b -> Convive (f b) (g b))

vaca :: [Beta] -> IO ()
vaca [] = putStrLn ""
vaca (Quieto:xs) = vaca xs
vaca ((Convive a b):xs) = vaca $ concat [xs, [b], [a]]
vaca (Chamba x:xs) = x >> vaca xs

\end{code}

g = (b -> Beta) -> Beta 
((a -> Beta) -> Beta) -> (a -> ((b -> Beta) -> Beta)) -> ((b -> Beta) -> Beta)
\begin{code}
instance Monad Otro where
  return x       = undefined
  (Otro f) >>= g = undefined

\end{code}

\begin{code}
cartel :: Otro ()
cartel = pana (dale (clavo 42)) (pana (dale (clavo 69)) (pana (dale (clavo 17)) (dale (clavo 23) >> chambea (putStrLn ""))))
              	
quedo :: Otro a -> IO ()
quedo x = vaca [hacer x]

clavo :: Int -> String
clavo 17 = "/nlmce"
clavo 23 = "/y./p6"
clavo 42 = "htptuc2"
clavo 69 = "t:irofr"

dale :: String -> Otro ()
dale xs = mapM_ (chambea . putChar) xs
\end{code}
