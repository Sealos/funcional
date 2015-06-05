\begin{code}
import Control.Monad
\end{code}
\begin{code}
data Otro a = Otro { fromOtro :: ((a -> Beta) -> Beta) }

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
-- Haces lo que tienes que hacer y sigues...
hacer :: Otro a -> Beta
hacer (Otro f) = f (const Quieto)

-- Lista
quieto :: Otro a
quieto = Otro (const Quieto)

-- Lista
chambea :: IO a -> Otro a
chambea x = Otro (\f -> Chamba $ fmap f x)

-- Compartir la fuerza, aunque al final quedas tablas
convive :: Otro a -> Otro ()
convive o =  Otro (\f -> Convive (hacer o) (f ()))

-- Cada quien busca por su lado...
pana :: Otro a -> Otro a -> Otro a
pana (Otro f) (Otro g) = Otro (\b -> Convive (f b) (g b))

vaca :: [Beta] -> IO ()
vaca [] = return ()
vaca (Quieto:bs)        = vaca bs
vaca ((Convive a b):bs) = vaca $ concat [bs, [a, b]]
vaca ((Chamba x):bs)    = x >>= (\f -> vaca (bs ++ [f]))

\end{code}

\begin{code}
instance Monad Otro where
  return x       = Otro (\f -> f x)
  (Otro f) >>= g = Otro (\j -> f (\x -> fromOtro (g x) j))

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
dale = mapM_ (chambea . putChar)
\end{code}
