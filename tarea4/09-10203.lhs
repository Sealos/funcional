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
      literate={+}{{$+$}}1 {/}{{$/$}}1 {*}{{$*$}}1 {=}{{$=$ }}1
               {>}{{$>$}}1 {<}{{$<$}}1 {\\}{{$\lambda$}}1
               {\\\\}{{\char`\\\char`\\}}1
               {->}{{$\rightarrow$ }}2 {>=}{{$\geq$}}2 {<-}{{$\leftarrow$}}2
               {<=}{{$\leq$}}2 {=>}{{$\Rightarrow$ }}2 
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


\title{CI4251 - Programación Funcional Avanzada \\ Tarea 4}
\author{Stefano De Colli\\09-10203}
\date{Junio 18, 2015}

\maketitle

\pagebreak

\ignore{
\begin{code}
{-# LANGUAGE BangPatterns #-}
import Data.Word (Word8, Word32)
import Control.Parallel.Strategies as S
import Control.Monad.Par as P
import Prelude as P
import Control.Monad.Identity
import Data.Array.Repa as R
import System.Environment
import qualified Graphics.HGL as G
import Data.List
import Criterion
import Criterion.Main
import Data.List.Split
\end{code}}

\noindent
Usaremos un tipo para simplificar la abstracción
\begin{code}
type Complex = (Double, Double)
\end{code}

\noindent
Dado que solo nos interesa la región de -2 a 2, escalamos la entrada a esos valores, independientemente de la proporción del aspecto.
\begin{code}
scaleVal :: Double -> Double -> Double
scaleVal a v = (a - v') * 2 / v'
        where
          v' = v / 2

scale :: Complex -> Complex -> Complex
scale f (a, b) = (a', b')
      where
        (x, y) = f
        a' = scaleVal a x
        b' = scaleVal b y

toComplex :: Integral a => Complex -> (a, a) -> Complex
toComplex c (x, y) = scale c (f x, f y)
  where
    f = fromIntegral
\end{code}


\noindent
Para converger, usaremos una función recursiva de cola para optimizar la ejecución. Adem\'{a}s cada nos aprovechamos de los BangPatterns.
\begin{code}
converge :: Complex -> Word8
converge x = convergeIt 0 (0, 0) x

convergeIt :: Word8 -> Complex -> Complex -> Word8
convergeIt 255 _  _          =  255
convergeIt !n (a, b) (x, y)  = if a*a + b*b > 4 then
                                n
                              else
                                convergeIt (n + 1) (nr, ni) (x, y)
                              where 
                                nr = a*a - b*b + x
                                ni = 2*a*b + y
\end{code}

\noindent
Esta funci\'{o}n crea una 'matriz' donde cada posici\'{o}n representa un pixel.
\begin{code}
makeList :: (Enum b, Enum a, Num b, Num a) => a -> b -> [[(a, b)]]
makeList a b = foldl' (\l e -> (zip (repeat e) ys):l) [] xs
  where
    xs = [0..a]
    ys = [0..b]
\end{code}
\pagebreak

\noindent
La estrategia que se utiliz\'{o} fue a cada lista, evaluarla por completo con un s\'{o}lo spark, y para encontrar cada valor interno, generar un spark
\begin{code}
mandelStrat :: Word32 -> Word32 -> [[Word8]]
mandelStrat w h = S.parMap rdeepseq (S.parMap rpar converge) mv
  where
    l = makeList w h
    mv = S.parMap rdeepseq (S.parMap rpar f) l
    f = toComplex (fromIntegral w, fromIntegral h)
\end{code}

\noindent
En el caso del monad paralelo, se uso una idea similar, al principio se usa un hilo para calcular las listas, y luego se mapea paralelamente esas listas, y se le aplica la otro mapeo paralelo que calcula el valor final.
\begin{code}
mandelPar   :: Word32 -> Word32 -> [[Word8]]
mandelPar w h = runPar $ do
  fl <- spawn . return $ makeList w h
  l <- get fl
  P.parMapM (P.parMap calculate) l
  where
    calculate c = converge $ toComplex (fromIntegral w, fromIntegral h) c
\end{code}

En el caso de REPA, nos aprovechamos de los arreglos Delayed, y una vez definido el arreglo, lo llenamos de una vez con los valores. Esta respuesta hubiese sido la m\'{a}s r\'{a}pida si no se tuviese que picar en listas de listas, ya que chunksOf es costosa.
\begin{code}
mandelREPA  :: Word32 -> Word32 -> [[Word8]]
mandelREPA w h = chunksOf (fromIntegral w) $ R.toList d 
  where
    d = R.fromFunction shape fm
    fm (Z :. x :. y) =  let
                          v = toComplex (fromIntegral w, fromIntegral h) (x, y)
                        in
                          converge v
    shape = (Z :. ((fromIntegral w)::Int) :. ((fromIntegral h)::Int))

\end{code}

\pagebreak
\noindent
Para dibujar los puntos, agarramos el arreglo de arreglos y las mismas dimensiones y
se mapean para dibujarlos como . con el color resultante de converge.
\begin{code}

drawMandel :: Int -> Int -> [[Word8]] -> IO ()
drawMandel w h colors = do
  G.runGraphics $ do
    window <- G.openWindow "Mandelbrot" (w, h)
    G.drawInWindow window $ G.overGraphics $
      let
        coords = makeList w h
        mk = P.zipWith (,) (P.concat coords) (P.concat colors)
      in 
        P.map (\(xy, c) -> G.withTextColor (G.RGB c c c) (G.text xy ".")) mk

\end{code}

\noindent
El programa de Criterion usado para correr las pruebas. 
\begin{code}
main :: IO ()
main = defaultMain [
          bgroup "mandelbrot" [
            bench "strat" $ nf (mandelStrat 1280) 1024
            ,bench "par" $ nf (mandelPar 1280) 1024
            ,bench "repadelay" $ nf (mandelREPA 1280) 1024
          ]
        ]

\end{code}

\pagebreak

\section*{Resultados de 1280x1024}
\begin{center}
    \begin{tabular}{| l | l | l | l | l | }
    \hline
     & Time & Mean & STD Dev & Var \\ \hline
    Strat & 1.429 s & 1.445 s & 16.26 ms & 19\% (moderately inflated) \\ \hline
    Par  &  2.168 s & 2.167 s & 44.17 ms & 19\% (moderately inflated) \\ \hline
    REPA D  & 2.068 s & 1.991 s  & 64.47 ms & 19\% (moderately inflated) \\ \hline
    \end{tabular}
\end{center}
\begin{center}
	\includegraphics[scale=0.5]{file.png}
\end{center}


\end{document}