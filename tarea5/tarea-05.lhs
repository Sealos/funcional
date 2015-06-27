\documentclass[11pt,fleqn]{article}

\usepackage{color}
\usepackage{tikz}
\usetikzlibrary{positioning,shapes}

\definecolor{brown}{rgb}{0.7,0.2,0}
\definecolor{darkgreen}{rgb}{0,0.6,0.1}
\definecolor{darkgrey}{rgb}{0.4,0.4,0.4}
\definecolor{lightgrey}{rgb}{0.95,0.95,0.95}


\usepackage[spanish]{babel}
\usepackage{lmodern}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage{listings}
\usepackage[colorlinks=true,urlcolor=blue]{hyperref}

\usepackage{mathrsfs}
\usepackage{amsmath}

\lstset{
   language=Haskell,
   gobble=2,
   frame=single,
   framerule=1pt,
   showstringspaces=false,
   basicstyle=\footnotesize\ttfamily,
   keywordstyle=\textbf,
   backgroundcolor=\color{lightgrey},
   literate={á}{{\'a}}1
            {é}{{\'e}}1
            {í}{{\'i}}1
            {ó}{{\'o}}1
            {ú}{{\'u}}1
            {ñ}{{\~n}}1
}

\begin{document}

\title{CI4251 - Programación Funcional Avanzada \\ Tareas 5}

\author{Ernesto Hernández-Novich\\
86-17791\\
\href{mailto:emhn@usb.ve}{<emhn@usb.ve>}}

\date{Junio 22, 2015}

\maketitle

\pagebreak

\section*{Un problema de programación dinámica\ldots}

Considere una expresión booleana compuesta por una
secuencia arbitraria de las palabras reservadas:

\begin{itemize}
\item
  \texttt{true}
\item
  \texttt{false}
\item
  \texttt{and}
\item 
  \texttt{or}
\item
  \texttt{xor}
\end{itemize}

\noindent
el problema consiste en determinar de \emph{cuántas} maneras se
pueden incorporar paréntesis \emph{explícitos} de modo que la
expresión tenga el valor \texttt{true}.

\noindent
Por ejemplo, si se nos proveyera le expresión
\begin{center}
  \begin{verbatim}
    true xor false and true
  \end{verbatim}
\end{center}

\noindent
el algoritmo debería contestar \texttt{2}, pues esa expresión sólo
se hace cierta si se incorporan los paréntesis

\begin{verbatim}
    ((true xor false) and true)
    (true xor (false and true))
\end{verbatim}

\noindent
Ud. debe implantar en Haskell una solución a este problema utilizando
técnicas de programación dinámica apoyadas en arreglos Haskell. En
este sentido, construiremos la solución comenzando con un tipo
de datos para representar los símbolos involucrados:

\begin{lstlisting}
> data Symbol = SymTrue | SymFalse | SymAnd | SymOr | SymXor
>             deriving (Show,Eq)
\end{lstlisting}

\noindent
Escriba un reconocedor \texttt{Parsec} que sea capaz de convertir una
expresión construida con los literales, y llevarla a una lista
de valores de nuestro tipo algebráico. En este sentido:

\begin{itemize}
\item
  Su reconocedor debe ser capaz de reconocer \emph{varias} expresiones,
  separadas entre sí por un punto y coma (\texttt{;}). Cada expresión
  puede ocupar una o más líneas, e incluso podría haber más de una
  expresión en una línea. Pero \emph{todas} terminan con punto y coma.
\item
  Puede haber una cantidad arbitraria de espacios en blanco antes del
  comienzo de la expresión, entre los literales, antes del punto y coma,
  y después del punto y coma. Deben ser ignorados.
\item
  Su reconocedor debe rechazar expresiones sintácticamente incorrectas.
  No es necesario que se recupere de ese error.
\end{itemize}

\noindent
Así, la función principal del reconocedor sería

\begin{lstlisting}
> expresiones :: Parser [[Symbol]]
\end{lstlisting}

\noindent
Escriba entonces la función
\begin{lstlisting}
> trueWays :: [Symbol] -> Int
\end{lstlisting}

\noindent
que calcule la cantidad de parentizaciones que hacen \texttt{true} la
expresión.

\noindent
El programa principal debe recibir un nombre de archivo como
argumento de línea de comandos, y si existe, aplicar el reconocedor
sobre los contenidos de ese archivo e indicar la cantidad de
parentizaciones para cada expresión. Sólo debe mostrar la expresión
y la cantidad de parentizaciones, pero \emph{no} necesita mostrar las
parentizaciones específicas.

\noindent
La solución para este algoritmo es directa y emplea técnicas de
programación dinámica sobre arreglos \emph{mutables}. Ud. puede
presentar una solución utilizando arreglos mutables sobre el
monad \texttt{ST}, pero sepa que es perfectamente posible hacerlo
con arreglos \emph{inmutables} si Ud. escribe \emph{thunks} de
manera astuta.

\section*{Aprovechando \texttt{Arbitrary}}
  
\noindent
¿Puede escribir una instancia \texttt{Arbitrary} que le ayude a generar
casos de prueba interesantes?

\begin{thebibliography}{1}

  \bibitem{Geeks4Geeks}
       
    \href{http://www.geeksforgeeks.org/dynamic-programming-set-37-boolean-parenthesization-problem/}
       {Descripción del problema y solución imperativa}

  \bibitem{Dean}

    \href{http://people.cs.clemson.edu/~bcdean/dp_practice/dp_9.swf}
         {Explicación del problema por Brian Dean} (requiere Flash)
\end{thebibliography}

\end{document}
