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


\title{CI4251 - Programación Funcional Avanzada \\ Tarea 5}
\author{Stefano De Colli\\09-10203}
\date{Junio 27, 2015}

\maketitle

\pagebreak

\ignore{
\begin{code}
import Text.ParserCombinators.Parsec
import Data.Array.ST
import Data.Array.Unboxed
import Control.Monad.ST
import Control.Monad
import Test.QuickCheck as Q
import Data.List
import Data.Word
import System.IO

data Symbol = SymTrue | SymFalse | SymAnd | SymOr | SymXor
  deriving (Show, Eq)
\end{code}}

\section*{Parser}

\noindent
Se implentó una función de palabras a símbolos, para no tener que implementar la instancia Read.
\begin{code}
stringToSym :: String -> Symbol
stringToSym "true"  = SymTrue
stringToSym "false" = SymFalse
stringToSym "and"   = SymAnd
stringToSym "or"    = SymOr
stringToSym "xor"   = SymXor
\end{code}

\noindent
Luego, el cada expresión está separada por \textbf{;} y dejamos al otro combinador que se encarge de que es una expresión.
\begin{code}
expresiones :: Parser [[Symbol]]
expresiones = do
              r <- expresion `endBy1` char ';'
              return $ map (map stringToSym) r

\end{code}

\noindent
Las expresiones y pueden estar separadas por espacios y final de linea, así que necesitamos un parser que ignore eso.
\begin{code}
sep :: String
sep = " \n\r"

garbage1 :: Parser ()
garbage1 = skipMany1 $ oneOf sep

garbage :: Parser ()
garbage = skipMany $ oneOf sep
\end{code}

\noindent
Para parsear los operadores no se necesita hacer backtracking, así que el operador de opción de Parsec es suficiente.
\begin{code}
value :: Parser String
value = string "true" <|> string "false"

operator :: Parser String
operator = string "and" <|> string "or" <|> string "xor"
\end{code}
\pagebreak

\noindent
Cada expresión puede tener basura al inicio, luego de la basura debería venir un valor True o False y por último cualquier cantidad de subexpresiones.
\begin{code}
expresion :: Parser [String]
expresion = do
          garbage
          val <- value
          garbage
          comb <- many subExp
          garbage
          return $ val:(concat comb)

\end{code}

\noindent
Cada expresión puede tener basura al inicio, luego de la basura debería venir un valor True o False y por último cualquier cantidad de subexpresiones.
\begin{code}
subExp :: Parser [String]
subExp = do
          op <- operator
          garbage1
          val <- value
          garbage
          return [op, val]
\end{code}

\pagebreak
\section*{Algoritmo dinámico}

\noindent
En memoria se guarda en memoria un arreglo de caracteres, por lo tanto debemos mappear lo que recibamos a caracteres.
\begin{code}
type Arr = UArray Int Char

symToChar :: Symbol -> Char
symToChar SymTrue  = 't'
symToChar SymFalse = 'f'
symToChar SymAnd   = '&'
symToChar SymOr    = '|'
symToChar SymXor   = '^'
\end{code}

\noindent
El algorimto separa los símbolos en valores booleanos y operadores, así que se hizo una función que separa las listas,
y dado que siempre las listas son de tamaño impar, entonces los operadores siempre terminarán en la primera posición de la tupla.
\begin{code}
splitList :: [a] -> ([a], [a])
splitList = foldl' (\(a1, a2) b -> (a2, b:a1)) ([], [])
\end{code}


Primero se mapean los Symbol a caracteres, y luego se separan con splitList,
y se construyen los arreglos contiguos en memoria partiendo de las listas de operadores y valores.
Finalmente se ejecuta el algoritmo dinámico. La solución esta en la posición (0, n), que significa de cuantas maneras se puede parentizar desde el principio hasta el final de la lista.
\begin{code}
trueWays :: [Symbol] -> Int
trueWays symbols = solve ! (0, n)
  where
    wsym = map symToChar symbols
    (op, sym) = splitList wsym
    n = length sym - 1
    sym' = array (0, n) $ [(i, x) | (i, x) <- zip [0..] (reverse sym)]
      :: Arr
    op' = array (0, (n - 1)) $ [(i, x) | (i, x) <- zip [0..] (reverse op)]
      :: Arr
    solve = solver sym' op' n
\end{code}
\pagebreak

\noindent
El algoritmo dinámico es básicamente una traducción del algoritmo iterativo con algunas diferencias.
Primero se crean los arrego \textbf{T} y \textbf{F} y se proceden a llenar las diagonales con los valores bases.
Luego en la iteración mas interna se leen todos los valores necesarios y se reescriben siguiendo el algoritmo iterativo.
\begin{code}
solver :: Arr -> Arr -> Int -> UArray (Int, Int) Int
solver sym op n = runSTUArray $ do
      t' <- newArray ((0, 0), (n, n)) 0 :: ST s (STUArray s (Int, Int) Int)
      f' <- newArray ((0, 0), (n, n)) 0 :: ST s (STUArray s (Int, Int) Int)
      forM_ [0..n] $ \i -> do
                          let n = sym ! i
                          writeArray t' (i, i) $ if n == 't' then 1 else 0
                          writeArray f' (i, i) $ if n == 'f' then 1 else 0
      forM_ [1..n] $ \gap -> do
        forM_ [gap..n] $ \j -> do
          let i = j - gap
          forM_ [0..(gap - 1)] $ \g -> do
            let k = i + g
            tt <- readArray t' (i, j)
            ff <- readArray f' (i, j)
            ttik <- readArray t' (i, k)
            ttkj <- readArray t' ((k + 1), j)
            ffik <- readArray f' (i, k)
            ffkj <- readArray f' ((k + 1), j)
            let cop = op ! k
                tik = ttik + ffik
                tkj = ttkj + ffkj
            
            if cop == '&' then
              do
                writeArray t' (i, j) (tt + ttik * ttkj) 
                writeArray f' (i, j) (ff + tik * tkj - ttik * ttkj)
            else
              if cop == '|' then
                do
                  writeArray t' (i, j) (tt + tik * tkj - ffik*ffkj)
                  writeArray f' (i, j) (ff + ffik * ffkj)
              else
                do
                  writeArray t' (i, j) (tt + ffik * ttkj + ttik * ffkj)
                  writeArray f' (i, j) (ff + ttik * ttkj + ffik * ffkj)
      return t'
\end{code}
\pagebreak


\section*{Arbitrary}

\noindent
Para no tener que escribir una instancia Random de Symbol, usaremos enteros y luego los mappeamos a Symbol.
\begin{code}
intToSym :: Int -> Symbol
intToSym 0 = SymTrue
intToSym 1 = SymFalse
intToSym 2 = SymAnd
intToSym 3 = SymOr
intToSym 4 = SymXor
\end{code}

Para la instancia de Arbitrary, creamos un nuevo tipo ya que arbitrary de por si instancia \textbf{[a]} y eso entra en conflicto con [Symbol].
\begin{code}
newtype Expression = Expression [Symbol]
\end{code}

\noindent
Para la instancia como tal, para un caso de tamaño \textbf{n} se generan \textbf{n + 1} valores booleanos y \textbf{n} operadores.
Luego se intercalan para generar una instancia válida. 
\begin{code}
instance Arbitrary Expression where
arbitrary = sized $ \n -> do
              values <- vectorOf (n + 1) $ choose (0, 1)
              ops <- vectorOf n $ choose (2, 4)
              let mvalues = map intToSym values
                  mops = map intToSym ops
              return $ Expression $ concat $ transpose [mvalues, mops]
\end{code}

\end{document}