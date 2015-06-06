\documentclass[11pt,fleqn]{article}

\usepackage{tikz}
\usepackage{multicol}
\usepackage{latexsym}
\usepackage{array}
\usepackage[english,spanish]{babel}
\usepackage{lmodern}
\usepackage{listings}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage[colorlinks=true,urlcolor=blue]{hyperref}
\usepackage{xcolor}
\usepackage{verbatim}
\usepackage{listings}
\usepackage{hyperref}
\long\def\ignore#1{}
\lstloadlanguages{Haskell}
\lstnewenvironment{code}
    {\lstset{}%
      \csname lst@SetFirstLabel\endcsname}
    {\csname lst@SaveFirstLabel\endcsname}
    \lstset{
      stepnumber=1,
      basicstyle=\small\ttfamily,
      flexiblecolumns=true,
      basewidth={0.45em,0.45em},
      literate={+}{{$+$}}1 {/}{{$/$}}1 {*}{{$*$}}1 {=}{{$=$}}1
               {>}{{$>$}}1 {<}{{$<$}}1 {\\}{{$\lambda$}}1
               {\\\\}{{\char`\\\char`\\}}1
               {->}{{$\rightarrow$}}2 {>=}{{$\geq$}}2 {<-}{{$\leftarrow$}}2
               {<=}{{$\leq$}}2 {=>}{{$\Rightarrow$}}2 
               {\ .}{{$\circ$}}2 {\ .\ }{{$\circ$}}2
               {>>}{{>>}}2 {>>=}{{>>=}}2
               {|}{{$\mid$}}1               
    }

\usepackage{algorithmic}
\usepackage{algorithm}

\usetikzlibrary{positioning,shapes,folding,positioning,shapes,trees}

\hypersetup{
  colorlinks=true,
  linkcolor=blue,
  urlcolor=blue
}

\definecolor{brown}{rgb}{0.7,0.2,0}
\definecolor{darkgreen}{rgb}{0,0.6,0.1}
\definecolor{darkgrey}{rgb}{0.4,0.4,0.4}
\definecolor{lightgrey}{rgb}{0.95,0.95,0.95}



\lstset{
   language=Haskell,
   gobble=0,
   frame=single,
   framerule=1pt,
   showstringspaces=false,
   basicstyle=\footnotesize\ttfamily,
   keywordstyle=\textbf,
   backgroundcolor=\color{lightgrey}
}

\long\def\ignore#1{}

\begin{document}


\title{CI4251 - Programación Funcional Avanzada \\ Tarea 3}
\author{Stefano De Colli\\09-10203}
\date{Junio 5, 2015}

\maketitle

\pagebreak
\section*{Clásico - MVars}

\ignore{
\begin{code}
import System.Random
import Control.Monad
import Data.Sequence as DS hiding (replicateM, zip)
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import System.Posix.Signals

-- Auxiliares
randomSeed :: Int
randomSeed = 42

-- Usadas en los modify de los TVars
inc :: Int -> Int
inc x = x + 1

incM :: Int -> Int -> Int
incM m x = x + m

-- Un generador en base de la semilla, asi puede ser repetible
randGen :: StdGen
randGen = mkStdGen randomSeed

-- Usado para un delay aleatorio
randomDelay :: Int -> Int -> IO ()
randomDelay lo hi = do
        r <- randomRIO (lo, hi)
        threadDelay r

-- Funciones auxiliares para el STM
put buffer item = do
  ls <- readTVar buffer
  writeTVar buffer (ls |> item)
 
get buffer = do
  ls <- readTVar buffer
  case viewl ls of
  EmptyL       -> retry
  item :< rest -> do
    writeTVar buffer rest
    return item


\end{code}
}

\noindent
En primera instancia tenemos a Rafita, para que ella empiece a cocinar se deben cumplir dos condiciones, primero que el colador donde sirve las empanadas esté vacío y que algún parroquiano esté hambriento, si esto se cumple empezará a cocinar \textit{m} empanadas, y luego de 3 a 5 segundos las pone en el colador.

\begin{code}
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

\pagebreak
\noindent
Luego tenemos al parroquiano, en principio el sólo dirá que tiene hambre una sola vez, luego de eso se quedará esperando hasta que hayan empanadas (Para eso es el último parámetro). Sí logró agarrar una empanada, entonces va a beber, tardando entre 1 a 7 segundos antes de que le dé hambre nuevamente. Si no logra agarrar empanada, intentará comer después.


\begin{code}
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
          takeMVar stall
          putMVar stall False
          parroquiano i counter bowl buffer stall False
      else
        do
          randomDelay 1000000 7000000
          parroquiano i counter bowl buffer stall True
\end{code}

\pagebreak
\noindent
Esta función imprimirá lo que exista en el buffer, si es que hay.
\begin{code}
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

\end{code}

\noindent
Y la función para inciar la simulación clasica, generamos los MVars para guardar la información, obtenemos los ThreadsID de Rafita y de cada parroquiano, y instalamos una función que se ejecutará al presionar Crtl-C
\begin{code}
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

      installHandler sigINT (Catch (
        printInfoC total counters rafitaTID parroquianosTID mainTID)) Nothing
      outputC buffer
\end{code}


\pagebreak
\noindent
La función que atrapa el Crtl-C hace lo siguiente, saca de los MVars el total de empanadas hechas por Radita y el contador de cada Parroquiano, una vez hecho esto, todos los hilos deberían estar bloqueados, así que matamos a todos los hilos, esperamos unos segundos\footnote{Esto se debe a que el hilo principal todavía está intentando imprimir, así que esperamos para que varios hilos no intenten imprimir en la consola simultaneamente} y luego imprimimos las estadisticas. Finalmente matamos al hilo principal (Que está imprimiendo del buffer compartido)\footnote{Si el programa fuese compilado y no ejecutado por GHCI, matar al hilo principal no sería necesario, pero esto es un problema ya que GHCI no mata los hilos al terminar la función, para más información, ver \href{https://ghc.haskell.org/trac/ghc/ticket/1399}{acá}}.
\begin{code}
printInfoC total counters rTID pTID mTID = do
  t <- readMVar total
  cs <- mapM readMVar counters
  forM_ pTID (\t -> killThread t)
  killThread rTID
  threadDelay 200000
  putStrLn $ "\n\nRafita preparo " ++ (show t) ++ " empanadas"
  let ps = zip cs [1..]
  forM ps (\(c, i)->
      putStrLn $ "Parroquiano " ++ (show i) ++ ":\t" ++ (show c))
  putStrLn $ "Total: " ++ (show $ sum cs)

  killThread mTID
\end{code}

\pagebreak
\section*{Transaccional - TVars}

\noindent
Similar al modelo clásico, sólo que en el transaccional no tenemos que reservar las variables en un orden particular y no usamos forever, mas bien recursión
\begin{code}
rafitaT m bowl total stall buffer = do
  b <- readTVarIO bowl
  s <- readTVarIO stall
  if b > 0 || s then 
    rafitaT m bowl total stall buffer
  else
    do
      atomically $ writeTVar stall True >> put buffer "Rafita esta cocinando"
      randomDelay 3000000 5000000
      atomically $ do
            modifyTVar' total (incM m)
            writeTVar bowl m
            put buffer "Rafita sirvio las empanadas"
            writeTVar stall True
      rafitaT m bowl total stall buffer
\end{code}

\pagebreak
\noindent
No hay mucho que explicar, sigue el mismo sentido que en el esquema clásico.
\begin{code}
parroquiano i counter bowl buffer stall bool = do
  if bool then
    do
      atomically $ put buffer $ "Parroquiano " ++ (show i) ++ " tiene hambre"
      comer
  else
    do
      comer
      threadDelay 1
    
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
\end{code}

\noindent
Para imprimir el buffer compartido, es muy sencillo.
\begin{code}
outputT buffer = 
    do str <- atomically $ get buffer
       putStrLn str
       outputT buffer
\end{code}

\pagebreak
\noindent
Y para iniciar la simulación es casi que lo mismo.
\begin{code}
transactional m n = do
  setStdGen $ mkStdGen randomSeed
  counters <- replicateM n newTVarIO 0
  bowl <- newTVarIO 0
  total <- newTVarIO 0
  stall <- newTVarIO True
  buffer <- newTVarIO DS.empty
  mainTID <- myThreadId
  parroquianosTID <- forM [1..n] $ (\i ->
    forkIO $ parroquiano i (counters !! (i - 1)) bowl buffer stall True)
  rafitaTID <- forkIO $ rafitaT m bowl total stall buffer
  installHandler sigINT (Catch (
    printInfoT total counters rafitaTID parroquianosTID mainTID)) Nothing
  outputT buffer
\end{code}

\noindent
Por último tenemos a la función encargada de atrapar al Crtl-C. El único cambio importante es que en el transaccional no hay que esperar por los otros hilos, esto se debe por el concepto de una transacción, si el hilo se destruye antes de que finalice, nada de los cambios tendrán efecto.
\begin{code}
printInfoT total counters rTID pTID mTID = do
  forM_ pTID (\t -> killThread t)
  killThread rTID
  buffer <- newTVarIO DS.empty
  t <- readTVarIO total
  cs <- mapM readTVarIO counters
  putStrLn $ "\n\nRafita preparo " ++ (show t) ++ " empanadas"
  let ps = zip cs [1..]
  forM ps (\(c, i)->
      putStrLn $ "Parroquiano " ++ (show i) ++ ":\t" ++ (show c)
    )
  putStrLn $ "Total: " ++ (show $ sum cs)

  killThread mTID
\end{code}

\pagebreak
\noindent


\end{document}