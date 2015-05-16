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
   gobble=2,
   frame=single,
   framerule=1pt,
   showstringspaces=false,
   basicstyle=\footnotesize\ttfamily,
   keywordstyle=\textbf,
   backgroundcolor=\color{lightgrey}
}

\long\def\ignore#1{}

\begin{document}

\title{CI4251 - Programación Funcional Avanzada \\ Tarea 2}

\author{Ernesto Hernández-Novich\\
86-17791\\
\href{mailto:emhn@usb.ve}{<emhn@usb.ve>}}

\date{Mayo 8, 2015}

\maketitle

\pagebreak

\section*{Autómatas Finitos No Determinísticos}

\begin{lstlisting}

> import Control.Monad
> import Control.Monad.RWS
> import Control.Monad.Error
> import qualified Data.Sequence as Seq
> import qualified Data.Set as DS
> import Data.Char
> import Data.Either
> import Test.QuickCheck 

\end{lstlisting}

\noindent
Considere el siguiente conjunto de Tipos Abstractos de Datos
diseñados para representar Autómatas Finitos No-Determinísticos
con $\lambda-$Transiciones ($\lambda-$NFA).
\\


\noindent
Los estados de un $\lambda-$NFA serán representados con
un \texttt{newtype}. Esto tiene el doble propósito de tener
enteros diferenciados, para un desempeño aceptable, y poder
disponer de una instancia \texttt{Show} separada con la cual
mantener la tradición de que los estados sean denotados
\texttt{q0}, \texttt{q1}, \ldots

\begin{lstlisting}

> newtype NFANode = Node Int 
>                 deriving (Eq,Ord)
>
> instance Show NFANode where
>    show (Node i) = "q" ++ show i

\end{lstlisting}

\noindent
Luego, representaremos las transiciones de un $\lambda-$NFA con un
tipo completo que incluye alternativas para transiciones que
consumen un símbolo de entrada y para $\lambda-$transiciones.
Con el tipo completo, además de la conveniencia de funciones
de acceso a los campos provista por la notación de registros,
también podemos tener una instancia \texttt{Show} separada que
muestre las transiciones de manera más atractiva.

\begin{lstlisting}

> data Transition = Move   { from, to :: NFANode, sym :: Char }
>                 | Lambda { from, to :: NFANode }
>                 deriving (Eq,Ord)
>
> instance Show Transition where
>   show (Move f t i) = show f ++ 
>                       " -" ++ show i ++ "-> " ++
>                       show t
>   show (Lambda f t) = show f ++ " ---> " ++ show t
>

\end{lstlisting}

\noindent
Finalmente, aprovechando la librería \texttt{Data.Set} para
representar conjuntos, podemos representar un $\lambda-$NFA
de manera directa. 

\begin{lstlisting}

> data NFA = NFA { 
>                   sigma   :: (DS.Set Char),
>                   states  :: (DS.Set NFANode),
>                   moves   :: (DS.Set Transition),
>                   initial :: NFANode,
>                   final   :: (DS.Set NFANode)
>                }
>          deriving (Eq,Show)

\end{lstlisting}

\noindent
En esta definición:

\begin{itemize}
\item
  \texttt{sigma} es el conjunto no vacío de caracteres del alfabeto.
\item 
  \texttt{states} es el conjunto no vacío de estados; siempre debe
  incluir al menos al estado inicial.
\item
  \texttt{moves} es el conjunto de transiciones.
\item
  \texttt{initial} es el estado final que \emph{siempre} sera
  \texttt{q0} (o sea \texttt{(Node 0)}).
\item
  \texttt{final} es el conjunto de estados finales.
\end{itemize}

\noindent
Con estas definiciones podemos construir expresiones que denoten
$\lambda-$NFA

\begin{lstlisting}

> nfa0 = NFA {
>              sigma  = DS.fromList "ab",
>              states = DS.fromList $ fmap Node [0..3],
>              moves  = DS.fromList [
>                Move { from = Node 0, to = Node 0, sym = 'a' },
>                Move { from = Node 0, to = Node 0, sym = 'a' },
>                Move { from = Node 0, to = Node 1, sym = 'a' },
>                Move { from = Node 1, to = Node 2, sym = 'b' },
>                Move { from = Node 2, to = Node 3, sym = 'b' }
>              ],
>              initial = Node 0,
>              final = DS.fromList [ Node 3 ]
>            }

\end{lstlisting}

\pagebreak

\section*{Generando $\lambda-$NFAs}

\noindent
La primera parte de este ejercicio requiere que Ud. escriba
las instancias \texttt{Arbitrary} que permitan generar nodos
y $\lambda-$NFAs. En este sentido, queremos que la generación
arbitraria sea de valores correctos.

\begin{lstlisting}

> instance Arbitrary NFANode where
>   arbitrary = undefined

\end{lstlisting}

\noindent
En el caso de \texttt{NFANode} queremos que los estados siempre
sean identificados con enteros \emph{positivos}. La librería
\texttt{Test.QuickCheck} incluye una forma de generar enteros
positivos que Ud. debe aprovechar. Como el estado inicial
\texttt{(Node 0)} \emph{siempre} debe estar presente, no hace
falta generarlo sino incluirlo manualmente en los $\lambda-$NFA.

\begin{lstlisting}

> instance Arbitrary NFA where
>   arbitrary = undefined

\end{lstlisting}

\noindent
En el caso de \texttt{NFA} queremos que el generador sólo produzca
$\lambda-$NFA con estructura consistente. En este sentido, la
instancia debe \emph{garantizar}:

\begin{itemize}
\item
  Que el alfabeto sea no vacío sobre letras minúsculas.
\item
  Que el conjunto de estados sea de tamaño arbitrario pero
  que \emph{siempre} incluya a \texttt{(Node 0)}.
\item
  Que tenga una cantidad arbitraria de transiciones. Todas las
  transiciones tienen que tener sentido: entre estados que están
  en el conjunto de estados y con un símbolo del alfabeto. Se
  desea que haya una $\lambda-$transición por cada cinco
  transiciones convencionales.
\item
  El estado inicial siempre debe ser \texttt{(Node 0)}.
\item
  El estado final debe ser un subconjunto del conjunto de estados,
  posiblemente vacío.
\end{itemize}

\noindent
Es inaceptable que el generador produzca algo con la forma de
un $\lambda-$NFA y luego se verifique la estructura.
Lo que se quiere es que el generador, mientras hace su trabajo,
\emph{garantice} que la estructura es correcta.
\footnote{Como sugerencia para el desarrollo, escriba una
función \texttt{isValid :: NFA -> Bool} que verifique la estructura
de un \texttt{NFA} cualquiera, y apóyese en ella para refinar la
calidad de su generador. La función \texttt{isValid} \textbf{no}
forma parte de la entrega.}

\section*{Simulador de $\lambda-$NFA}

\noindent
Recordará de CI-3725 que los $\lambda-$NFA son equivalentes a
los Autómatas Determinísticos, en virtud del algoritmo que simula
el paso \emph{simultáneo} por todos los estados válidos para cada
transición. Le sugiero buscar el Sudkamp y sus notas de clase para
reforzar el tema, aún cuando iremos desarrollando la solución paso
a paso.
\\

\noindent
En primer lugar, es necesario identificar si un movimiento es
convencional o es una $\lambda-$transición, así que debe proveer
las funciones
\footnote{Note que una es la negación de la otra. Implante la que
le resulte ``más obvia'' y exprese la otra como negación de la
primera en estilo \emph{point-free} -- elegancia y categoría.}

\begin{lstlisting}

> isMove, isLambda :: Transition -> Bool
> isMove   = undefined
> isLambda = undefined

\end{lstlisting}

\noindent
En el algoritmo de simulación, la manera de desplazarse a partir
de un estado particular consiste en considerar la $\lambda-$clausura
del estado. Luego, para cada uno de los estados, determinar a su vez
aquellos estados a los cuales es posible moverse consumiendo
exactamente un símbolo de entrada. Finalmente, considerar la
$\lambda-$clausura de esos estados, obteniéndose así los estados
destino.
\\

\noindent
Para resolver ese problema, Ud. deberá implantar varias funciones
auxiliares:

\begin{itemize}
\item 
  Dado un $\lambda-$NFA y un estado, calcular el conjunto
  de estados a los cuales se puede alcanzar con \emph{una}
  $\lambda-$transición.

  \begin{lstlisting}

> lambdaMoves :: NFA -> NFANode -> DS.Set NFANode
> lambdaMoves = undefined

  \end{lstlisting}

\item
  Dado un $\lambda-$NFA, un caracter del alfabeto y un estado,
  calcular el conjunto de estados a los cuales se puede alcanzar
  con una transición que consuma el caracter de entrada.

  \begin{lstlisting}

> normalMoves :: NFA -> Char -> NFANode -> DS.Set NFANode
> normalMoves = undefined

  \end{lstlisting}

\item
  Dado un $\lambda-$NFA, un caracter del alfabeto y un estado,
  calcular el conjunto de estados a los cuales se puede alcanzar
  consumiendo el caracter de entrada. Esta es la función que
  debe calcular la $\lambda-$clausura del estado inicial, los
  desplazamientos desde ese conjunto ahora consumiendo la entrada,
  y luego la $\lambda-$clausura final.

  \begin{lstlisting}

> destinations :: NFA -> Char -> NFANode -> DS.Set NFANode
> destinations = undefined

  \end{lstlisting}

\item
  Recordará que el cálculo de la $\lambda-$clausura es un algoritmo
  de la familia de Algoritmos de Punto Fijo. En estos algoritmos
  se cuenta con un conjunto inicial, una función aplicable sobre
  elementos del conjunto que produce nuevos conjuntos, los cuales
  deben ser unidos para ampliar el conjunto, hasta que no se pueda
  ampliar más. Escriba la función

  \begin{lstlisting}

> fixSet :: Ord a => (a -> DS.Set a) -> DS.Set a -> DS.Set a
> fixSet f s = undefined

  \end{lstlisting}

  Que es capaz de calcular el punto fijo de aplicar \texttt{f}
  sobre el conjunto \texttt{s}. La función \texttt{fixSet} será
  necesaria para poder implantar \texttt{destinations}.
\end{itemize}

\noindent
Una vez implantadas estas funciones, estará en posición de implantar
el simulador monádico de $\lambda-$NFA poniendo en práctica monads
y transformadores.
\\

\noindent
La función principal de este simulador será

\begin{lstlisting}

> runNFA :: NFA -> [Char] -> IO ()
> runNFA nfa word = undefined

\end{lstlisting}

\noindent
que para un \texttt{nfa} particular simulará el procesamiento de la
palabra de entrada \texttt{word}. El comportamiento de la función
dependerá de lo que ocurra en la simulación. Por ejemplo:

\begin{itemize}
\item
  Si la palabra es \emph{aceptada}, debe \emph{imprimir} en
  pantalla la secuencia de conjuntos de estados por la cual
  pasó el $\lambda-$NFA.
  \begin{verbatim}
ghci> runNFA nfa0 "abb"
fromList [fromList [q0],fromList [q0,q1],
          fromList [q2],fromList [q3]] \end{verbatim}
  Aquí hay \emph{dos} usos diferentes de \texttt{fromList}: el más
  externo es de un \texttt{Data.Sequence} y los más internos son
  de \texttt{Data.Set}.
\item
  Si la palabra es \emph{rechazada} porque se consumió la entrada
  pero el $\lambda-$NFA quedó en un estado no final, debe
  \emph{imprimir} en pantalla un mensaje indicando que rechazó
  y en cuales estados se encontraba la simulación.
  \begin{verbatim}
ghci> runNFA nfa0 "ab"
Reject (fromList [q2]) \end{verbatim}
\item
  Si la palabra es \emph{rechazada} porque no hay transiciones
  posibles sobre el siguiente símbolo de entrada, debe
  \emph{imprimir} en pantalla un mensaje indicando que rechazó
  indicando la entrada restante y el estado de estancamiento.
  \begin{verbatim}
ghci> runNFA nfa0 "abbbb"
Stuck (fromList [q3]) "bb" \end{verbatim}
\end{itemize}

\noindent
Para escribir el simulador Ud. necesitará combinar correctamente
los monads \texttt{Reader}, \texttt{Writer}, \texttt{State} y
\texttt{Error}.
\footnote{Puede usar el monad \texttt{RWS} o el transformador
\texttt{RWST} si lo desea. No es ni más fácil, ni más difícil:
sólo tiene que escribir menos firmas y paréntesis.}
Necesitará el monad \texttt{Reader} para llevar
el NFA que se está simulando, el monad \texttt{Writer} para ir
conservando los conjuntos de estados por los cuales vaya
avanzando, el monad \texttt{State} para mantener la entrada
restante y el conjunto de estados actuales, y el monad
\texttt{Error} para poder emitir las excepciones necesarias
para el rechazo.
\\

\noindent
Ambos rechazos serán manejados como excepciones, así que necesitará

\begin{lstlisting}

> data NFAReject = Stuck (DS.Set NFANode) String
>                | Reject (DS.Set NFANode)
>                deriving (Show)
>
> instance Error NFAReject

\end{lstlisting}

\noindent
Necesitará un tipo de datos que se aprovechará en el Monad 
\texttt{State}

\begin{lstlisting}

> data NFARun = NFARun { w :: String, qs :: DS.Set NFANode }
>             deriving (Show,Eq)

\end{lstlisting}

\noindent
Como comprenderá, no puedo darle la firma de las funciones
monádicas que necesitará. Después de todo, de eso se trata esta
evaluación. No obstante, le sugiero estructurar su código
alrededor de las siguientes funciones:

\begin{itemize}
\item
  Una función para preparar el estado inicial de la simulación, con
  la palabra de entrada y fijando el estado actual en
  \texttt{(Node 0)}.
  \begin{lstlisting}

> initialState :: String -> NFARun
> initialState word = undefined

  \end{lstlisting}
\item
  Una función para determinar si en un conjunto de estados hay uno
  o más que sean estados finales de un NFA.
  \begin{lstlisting}

> accepting :: NFA -> DS.Set NFANode -> Bool
> accepting = undefined

  \end{lstlisting}
\item
  Una función monádica \texttt{start} que comienza la simulación
  a partir del estado inicial.
\item
  Una función monádica \texttt{flow} que completa la simulación.
  Esta función es la que debe operar en el monad combinado y
  hacer avanzar el $\lambda-$NFA consumiendo la entrada. Si
  detecta que debe rechazar la palabra, lanza la excepción
  adecuada; si logra procesar todo la entrada y aceptar, debe
  permitir acceso al historial de movimientos.
\item
  La función \texttt{runNFA} mencionada más arriba, debe
  aprovechar las funciones monádicas \texttt{start} y \texttt{flow}
  en el contexto de los Monads adecuados para correr la
  simulación, obtener los resultados e imprimir.
\end{itemize}

\noindent
Hay muchas formas de implantar la simulación y las funciones auxiliares,
sin embargo aplican las recomendaciones que ya hemos discutido en el
pasado: aprovechar funciones de orden superior y aprovechar funciones
de las librerías.
\\

Una vez que su simulación esté operando correctamente, escriba dos
propiedades QuickCheck y aproveche la instancia \texttt{Arbitrary} para
comprobar:

\begin{itemize}
\item
  Todo $\lambda-$NFA que tenga un estado final en la $\lambda-$clausura
  de su estado inicial acepta la palabra vacía.
  \begin{lstlisting}

> prop_acceptsemptyword :: NFA -> Property
> prop_acceptsemptyword nfa = undefined

  \end{lstlisting}
\item
  Cuando un $\lambda-$NFA acepta una palabra de longitud $n$, el camino
  recorrido tiene longitud $n+1$.
  \begin{lstlisting}

> prop_acceptancelength :: NFA -> String -> Property
> prop_acceptancelength nfa w = undefined

  \end{lstlisting}
\end{itemize}

\newpage
\section*{Otro Beta}

\noindent
La vida es dura. Todos los días hay que salir a la calle,
buscando la fuerza para alcanzar otro beta y echar pa'lante.

\begin{lstlisting}

> data Otro a = Otro ((a -> Beta) -> Beta)

\end{lstlisting}

\noindent
Y se hace más difícil, porque el \texttt{Beta} está en una chamba o en
hacer llave con un convive, así hasta que te toque el quieto.

\begin{lstlisting}

> data Beta = Chamba (IO Beta)
>           | Convive Beta Beta
>           | Quieto

\end{lstlisting}

\noindent
Se complica ver el \texttt{Beta}, porque \texttt{IO} esconde lo que
tiene. Hay que inventarse una ahí para tener idea\ldots

\begin{lstlisting}

> instance Show Beta where
>    show (Chamba x)    = " chamba "
>    show (Convive x y) = " convive(" ++ show x 
>                                     ++ "," 
>                                     ++ show y ++ ") "
>    show Quieto        = " quieto "

\end{lstlisting}

\noindent
A veces hay suerte, y uno encuentra algo que hacer. Uno llega
con fuerza, hace lo que tiene que hacer, y allí sigues buscando
otro Beta
\begin{lstlisting}

> hacer :: Otro a -> Beta
> hacer = undefined

\end{lstlisting}

\noindent
pero es triste ver cuando simplemente es un quieto. No importa
si traes fuerza, te quedas quieto.

\begin{lstlisting}

> quieto :: Otro a
> quieto = undefined

\end{lstlisting}

\noindent
Pero hay que ser positivo. Hay que pensar que si uno encuentra
un oficio, uno chambea. Sólo hay que darle a la chamba y de
allí uno saca fuerza

\begin{lstlisting}

> chambea :: IO a -> Otro a
> chambea = undefined

\end{lstlisting}

\noindent
y si el trabajo se complica, lo mejor es encontrar un convive
para compartir la fuerza, aunque al final quedes tablas

\begin{lstlisting}

> convive :: Otro a -> Otro ()
> convive = undefined

\end{lstlisting}

\noindent
Para llegar lejos, es mejor cuadrar con un pana. Cada uno
busca por su lado, y luego se juntan.

\begin{lstlisting}

> pana :: Otro a -> Otro a -> Otro a
> pana = undefined

\end{lstlisting}

\noindent
y así al final, cuando se junten los panas, hacen una vaca
y se la vacilan

\begin{lstlisting}

> vaca :: [Beta] -> IO ()
> vaca = undefined

\end{lstlisting}

\noindent
Me pasaron el dato, que buscar el beta es como un perol, que
con cada mano que le metes, siempre te hace echar pa'lante. Que
consulte al chamo de sistemas, pa'que me muestre como hacer. Porque
a esos peroles con cosas, que paso a paso avanzan, dizque los
mentan ``Monads''. A mi no me dá el güiro, pero espero que ti
si, menor.

\begin{lstlisting}

> instance Monad Otro where
>   return x       = undefined
>   (Otro f) >>= g = undefined

\end{lstlisting}

\noindent
\textbf{Nota:} el propósito de este ejercicio es que noten que
son los \emph{tipos} los que deben describir el comportamiento.
Hay sólo una manera correcta de escribir todas las funciones aquí
solicitades, para lo cual lo único necesario es considerar el
tipo a producir y los tipos de los argumentos. Así mismo, para
escribir la instancia \texttt{Monad}, sólo es necesario respetar
las firmas.

\noindent
Para saber si su código funciona, primero debe compilar todo
sin errores. Luego, ejecute

\begin{verbatim}
ghci> quedo cartel
\end{verbatim}

\noindent
y el contenido le hará entender si lo logró.

\ignore{
\begin{lstlisting}

> cartel :: Otro ()
> cartel = pana (dale (clavo 42)) 
>               (pana (dale (clavo 69))
>                     (pana (dale (clavo 17)) 
>                           (dale (clavo 23) >> chambea (putStrLn ""))))
> 
> quedo :: Otro a -> IO ()
> quedo x = vaca [hacer x]
> 
> clavo :: Int -> String
> clavo 17 = "/nlmce"
> clavo 23 = "/y./p6"
> clavo 42 = "htptuc2"
> clavo 69 = "t:irofr"
> 
> dale :: String -> Otro ()
> dale xs = mapM_ (chambea . putChar) xs

\end{lstlisting}
}

\end{document}