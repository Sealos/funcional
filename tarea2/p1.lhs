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
\long\def\ignore#1{}
\lstloadlanguages{Haskell}
\lstnewenvironment{code}
    {\lstset{}%
      \csname lst@SetFirstLabel\endcsname}
    {\csname lst@SaveFirstLabel\endcsname}
    \lstset{
      basicstyle=\small\ttfamily,
      flexiblecolumns=false,
      basewidth={0.6em,0.6em},
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
\author{Stefano De Colli\\09-10203}
\date{Mayo 22, 2015}

\maketitle

\pagebreak

\ignore{
\begin{code}
import Control.Monad
import Control.Monad.RWS
import Control.Monad.Trans.Error
import qualified Data.Sequence as Seq
import qualified Data.Set as DS
import Data.Char
import Data.Either
import Test.QuickCheck 

newtype NFANode = Node Int 
                deriving (Eq,Ord)

instance Show NFANode where
   show (Node i) = "q" ++ show i

data Transition = Move   { from, to :: NFANode, sym :: Char }
                | Lambda { from, to :: NFANode }
                deriving (Eq,Ord)

type RWSNFA = ErrorT NFAReject (RWS NFA NFALog NFARun)

instance Show Transition where
  show (Move f t i) = show f ++ 
                      " -" ++ show i ++ "-> " ++
                      show t
  show (Lambda f t) = show f ++ " ---> " ++ show t

data NFA = NFA { 
                  sigma   :: (DS.Set Char),
                  states  :: (DS.Set NFANode),
                  moves   :: (DS.Set Transition),
                  initial :: NFANode,
                  final   :: (DS.Set NFANode)
               }
         deriving (Eq,Show)

data NFAReject = Stuck (DS.Set NFANode) String  -- Sin transiciones
               | Reject (DS.Set NFANode)        -- Palabra vacia
               deriving (Show)
instance Error NFAReject

-- Monad State
data NFARun = NFARun { w :: String, qs :: DS.Set NFANode }
            deriving (Show,Eq)
            
type NFALog = Seq.Seq String
\end{code}
}

\noindent
Se aprovechó del tipo de dato Positive de QuickCheck para solo generar
nodos de enteros positivos
\begin{code}
instance Arbitrary NFANode where
  arbitrary = return . Node . getPositive =<< arbitrary
\end{code}

Esta funcion auxiliar devuelve un generador de transiciones dado una lista
de caracteres y una lista de estados.
\begin{code}
transitionGen :: [Char] -> [NFANode] -> Gen Transition
transitionGen sigma states = frequency [
                              (1, genLambda),
                              (5, genMove)]
            where
              genMove = do
                          from  <- elements states
                          to    <- elements states
                          sigma <- elements sigma
                          return $ Move from to sigma
              genLambda   = do
                          from  <- elements states
                          to    <- elements states
                          return $ Lambda from to
\end{code}
\noindent
Para hacer las instancias arbitrarias del NFA, primero se selecciona el
alfabeto y los estados, y luego se construyen las transiciones y los
estados final partiendo de los valores generados.
\begin{code}
instance Arbitrary NFA where
  arbitrary = sized $ \n -> do
                sigma       <- listOf1 (choose ('a', 'z'))
                states      <- listOf1 (arbitrary::Gen NFANode)
                final       <- listOf $ elements (n0:states)
                transitions <- resize (3*n) $ listOf $ transitionGen sigma (n0:states)
                return $ NFA (DS.fromList sigma) (DS.fromList (n0:states)) (DS.fromList transitions) n0 (DS.fromList final)
            where
              n0 = Node 0
\end{code}
\noindent
Para reconocer si una transición es lambda o no.
\begin{code}
isMove :: Transition -> Bool
isMove = not . isLambda

isLambda :: Transition -> Bool
isLambda (Lambda _ _) = True
isLambda _            = False
\end{code}

\begin{code}
lambdaMoves :: NFA -> NFANode -> DS.Set NFANode
lambdaMoves nfa n = DS.map to validLambda
  where validLambda = DS.filter (\s -> isLambda s && n == from s) $ moves nfa

normalMoves :: NFA -> Char -> NFANode -> DS.Set NFANode
normalMoves nfa c n = DS.map to validTransitions
  where validTransitions = DS.filter (\s -> isMove s && n == from s && c == sym s) m
        m = moves nfa

fixSet :: Ord a => (a -> DS.Set a) -> DS.Set a -> DS.Set a
fixSet f s =  let
                newElems  = DS.unions $ DS.foldl' (\a e -> (f e):a) [] s
                newSet    = DS.union s newElems
              in
                case newSet == s of
                  True  -> s
                  False -> fixSet f newSet
\end{code}
\noindent
Esta funcion calcula los posibles estados a los cuales puede alcanzar un NFA. 
Primero se calculan las lambda transiciones del nodo principal, luego
se consume el simbolo, y al final se vuelve a calcular las lambda transiciones.
\begin{code}

lambdaDest :: NFA -> NFANode -> DS.Set NFANode
lambdaDest nfa n = fixSet (lambdaMoves nfa0) (DS.singleton (n))

destinations :: NFA -> Char -> NFANode -> DS.Set NFANode
destinations nfa char n = sClosure
  where
    set      = lambdaMoves nfa n
    fClosure = DS.unions . DS.toList $ DS.map (normalMoves nfa char) set
    sClosure = fixSet (lambdaDest nfa) fClosure

\end{code}
\begin{code}
destFromNodes :: NFA -> Char -> DS.Set NFANode -> DS.Set NFANode
destFromNodes nfa char ns = DS.unions . DS.toList $ DS.map (destinations nfa char) ns

runNFA :: NFA -> [Char] -> IO ()
runNFA nfa word = do
                   let (err, state, log) = start nfa word flow
                   print err
                   print state
                   print log

start :: NFA -> [Char] -> RWSNFA a -> (Either NFAReject a, NFARun, NFALog)
start nfa sym = ((flip $ (flip runRWS) nfa) state) . runErrorT
  where state = initialState sym

logNodes :: DS.Set NFANode -> Seq.Seq String
logNodes nodes = Seq.singleton $ show nodes

\end{code}
\begin{code}
n0 = Node 0
nfa0 = NFA {
             sigma  = DS.fromList "a",
             states = DS.fromList $ fmap Node [0..1],
             moves  = DS.fromList [
               --Move { from = Node 0, to = Node 0, sym = 'a' },
               Lambda { from = Node 0, to = Node 1 },
               --Move { from = Node 0, to = Node 0, sym = 'a' },
               --Move { from = Node 0, to = Node 1, sym = 'b' }
               --Lambda { from = Node 1, to = Node 2 },
               --Lambda { from = Node 4, to = Node 5 },
               --Lambda { from = Node 5, to = Node 6 },
               --Lambda { from = Node 3, to = Node 2 },
               --Lambda { from = Node 0, to = Node 1 }
               Move { from = Node 1, to = Node 0, sym = 'a' }
               --Move { from = Node 2, to = Node 3, sym = 'a' },
               --Move { from = Node 3, to = Node 4, sym = 'b' }
               --Move { from = Node 3, to = Node 2, sym = 'b' },
               --Move { from = Node 2, to = Node 3, sym = 'b' }
             ],
             initial = Node 0,
             final = DS.fromList [ Node 1 ]
           }

flow :: RWSNFA ()
flow = do
      nfa <- ask
      s <- get
      let cw = w s
          cs = qs s
      if cw == "" then
        do
          -- Hacemos esto por si estamos en el estado inicial
          let ls = fixSet (lambdaDest nfa) cs
          if accepting nfa ls then
            return ()
          else
            throwError $ Reject ls
      else
        do
          let nw = tail cw
              ns = destFromNodes nfa (head cw) cs
              con = fixSet (normalMoves nfa (head cw)) lon
              lon = fixSet (lambdaDest nfa) cs
          -- Si no es vacia y no hay estados..
          if con == DS.empty then
            throwError $ Stuck cs cw
          else
            do
              tell $ logNodes cs
              put $ s { qs = ns, w = nw }
              flow

\end{code}

\noindent
El monad aprovecha varias funciones auxiliares
\begin{code}
initialState :: String -> NFARun
initialState word = NFARun word (DS.singleton (Node 0))

accepting :: NFA -> DS.Set NFANode -> Bool
accepting nfa = DS.foldl' isFinal False
  where
    fs      = final nfa
    isFinal b n = b || DS.member n fs

\end{code}

\begin{code}
prop_acceptsemptyword :: NFA -> Property
prop_acceptsemptyword nfa = undefined

prop_acceptancelength :: NFA -> String -> Property
prop_acceptancelength nfa w = undefined
\end{code}
\end{document}
