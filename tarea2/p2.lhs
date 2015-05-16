\begin{code}

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
hacer = undefined

quieto :: Otro a
quieto = undefined

chambea :: IO a -> Otro a
chambea = undefined

convive :: Otro a -> Otro ()
convive = undefined

pana :: Otro a -> Otro a -> Otro a
pana = undefined

vaca :: [Beta] -> IO ()
vaca = undefined

\end{code}

\begin{code}
instance Monad Otro where
  return x       = undefined
  (Otro f) >>= g = undefined
\end{code}

\begin{code}
cartel :: Otro ()
cartel = pana (dale (clavo 42)) 
              (pana (dale (clavo 69))
                    (pana (dale (clavo 17)) 
                          (dale (clavo 23) >> chambea (putStrLn ""))))

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