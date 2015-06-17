\documentclass[11pt,fleqn]{article}

\usepackage{color}
\usepackage{tikz}
\usetikzlibrary{positioning,shapes}

\definecolor{brown}{rgb}{0.7,0.2,0}
\definecolor{darkgreen}{rgb}{0,0.6,0.1}
\definecolor{darkgrey}{rgb}{0.4,0.4,0.4}
\definecolor{lightgrey}{rgb}{0.95,0.95,0.95}

\usepackage{amsmath}
\usepackage[spanish]{babel}
\usepackage{lmodern}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage{listings}
\usepackage[colorlinks=true,urlcolor=blue]{hyperref}

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

\title{CI4251 - Programación Funcional Avanzada \\ Tarea 4}

\author{Ernesto Hernández-Novich\\
86-17791\\
\href{mailto:emhn@usb.ve}{<emhn@usb.ve>}}

\date{Junio 9, 2015}

\maketitle

\pagebreak

\section*{Conjunto de Mandelbrot}

\noindent
Un Conjunto de Mandelbrot es un conjunto de puntos en el plano
complejo que son cuasi-estables cuando se calculan iterando
una función. Usualmente se emplea la función

$$z_{k+1} = z^2_k+c$$

\noindent
donde $z_{k+1}$ es la $(k+1)-$ésima iteración del número complejo
$z=a+bi$, $z_k$ es la $k-$ésima iteración, y $c$ es el número
complejo que expresa la posición del punto en el plano complejo.

\noindent
El valor inicial para $z$ es cero, y las iteraciones deben repetirse
hasta que la magnitud de $z$ sea mayor que 2 (indicando que $z$
tendría magnitud infinita eventualmente) o se llega a una cantidad
arbitraria de iteraciones sin que esto ocurra.

\noindent
La magnitud de un número complejo $z=a+bi$ se calcula como
$$|z| = \sqrt{a^2+b^2}$$

\noindent
Así mismo, calcular la función compleja $z_{k+1} = z^2_k+c$ es muy
simple si se simplifica la expresión hasta llegar

\begin{align*}
        z'_{real} &= z_{real}^2 - z_{imaginary}^2 + c_{real} \\
        z'_{imaginary} &= 2 \cdot z_{real} \cdot z_{imaginary} +
        c_{imaginary}
\end{align*}

\noindent
Considerando que el punto con coordenadas $(x,y)$ corresponde al
número complejo $z = x+yi$, implante la función

\begin{lstlisting}
> converge :: (Double,Double) -> Word8
\end{lstlisting}


\noindent
que itere la función compleja sobre el número complejo hasta converger
o por un máximo de 255 iteraciones. La función debe retornar el número
de iteraciones efectivamente completadas.
\\

\noindent
Para visualizar el Conjunto de Mandelbrot sobre un conjunto
arbitrario en el plano complejo, presentaremos cada punto $(x,y)$
con una tonalidad de gris proporcional a la cantidad de iteraciones
completadas por la función anterior. Esto es, si \texttt{converge (x,y)}
produce $n$ como resultado, el pixel $(x,y)$ tendrá el ``color''
\texttt{RGB n n n} que corresponde al $n-$ésimo gris.
\\

\noindent
Las partes interesantes del Conjunto de Mandelbrot están en un
círculo de radio 2 alrededor del centro del Plano Complejo. Esto
quiere decir que basta variar la parte real en el intervalo
$[-2,2]$ y simultáneamente se hace variar la parte imaginaria en
el intervalo $[-2,2]$, analizando los números complejos allí
presentes.

\noindent
El nivel de detalle observable dependerá del tamaño de la ventana
con la cual se presente el conjunto, pues la cantidad de pixels
horizontales y verticales establece cuántos puntos calcular. Para
los que no han cursado Computación Gráfica, si se desea una ventana
con $w$ pixels de ancho y $h$ pixels de alto, Ud. puede calcular

\begin{align*}
  step_{real} &= 4.0 / w \\
  step_{imaginary} &= 4.0 / h 
\end{align*}

\noindent
que le permitirán recorrer el intervalo real y el intervalo imaginario
paso a paso, i.e. si $0\leq x < w \wedge 0 \leq y < h$, entonces el
pixel $(x,y)$ correspondería al número complejo
$(-2.0 + step_{real}*x,-2.0 + step_{imaginary}*y)$
\\

\noindent
Provea tres implantaciones del cálculo de la parte interesante del
Conjunto de Mandelbrot sobre una ``ventana'' de visualización

\begin{lstlisting}
> mandelStrat :: Word32 -> Word32 -> [[Word8]]
> mandelPar   :: Word32 -> Word32 -> [[Word8]]
> mandelREPA  :: Word32 -> Word32 -> [[Word8]]
\end{lstlisting}

\noindent
que aprovechen estrategias paralelas, el Monad \texttt{Par} y la librería
de vectorización REPA, respectivamente.
\\

\noindent
Asegúrese que el desempeño de su implantación sea bueno en ``ventanas''
de hasta 1280x1024. No haga ninguna suposición sobre la cantidad de
núcleos disponibles para el cómputo; si lo desea, utilice las funciones
ofrecidas por GHC para determinar cuántos hay disponibles, pero
intente encontrar un particionado dinámico razonable. Finalmente,
escriba un programa principal usando \texttt{Criterion} que permita
comparar la velocidad de ejecución de las tres implantaciones.

\end{document}
