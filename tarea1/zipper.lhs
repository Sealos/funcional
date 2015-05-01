\begin{code}

import Data.List
import Data.Functor
import Data.Monoid
import Data.Foldable (foldMap)
import Data.Tree
import Data.Maybe
import Control.Monad


a = Directory 3 [File 4, Directory 5 [File 5, Directory 3 [File 1]]]  

data Filesystem a = File a | Directory a [Filesystem a] deriving (Show)

-- Valor, elementos a la izquierda, elementos a la derecha
data Crumb a = Crumb {val :: a, ez :: [Filesystem a],  ed :: [Filesystem a] } deriving (Show)
type Breadcrumbs a = [Crumb a]

type Zipper a = (Filesystem a, Breadcrumbs a)
\end{code}

En la funcion goDown, se devuelve Nothing en el caso de que se intente
bajar por un Fileo un Directory que no tenga elementos,
en caso contrario, creamos un crumb que contenga los elementos
como hijos derechos y guardamos el elemento
\begin{code}
goDown :: Zipper a -> Maybe (Zipper a)
goDown (File _, _)                  = Nothing
goDown (Directory _ [], _)          = Nothing
goDown (Directory n (fs:fss), bcs)  = Just (fs, (Crumb n [] fss):bcs)
\end{code}

Si no hay crumbs o los hijos derechos estan vacios,
entonces no me deberia poder mover, en caso contrario
agarro el primer elemento de la lista de ed y lo coloco
como el nuevo foco, luego se guarda el foco viejo en la
lista de ez
\begin{code}
goRight :: Zipper a -> Maybe (Zipper a)
goRight (_, [])                         = Nothing
goRight (_, (Crumb _ _ []):bcs)         = Nothing 
goRight (fs, (Crumb e ez (ed:eds)):bcs) = Just (ed, (Crumb e (fs:ez) eds):bcs)
\end{code}

Analogo de goRight, solo que con el elemento izquierdo 
\begin{code}
goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (_, [])                          = Nothing
goLeft (_, (Crumb _ [] _):bcs)          = Nothing
goLeft (fs, (Crumb e (ez:ezs) ed):bcs)  = Just (ez, (Crumb e ezs (fs:ed)):bcs)
\end{code}

Si no hay donde subir, Nothing, en caso contrario
se agarra el ultimo Crumb y se reconstruye el camino
primero colocando los elementos a la derecha y luego
los de la izquierda. Se usa un flip de Cons ya que
el foldl concatenaria List:Elem y asi no se usa foldr
\begin{code}
goBack :: Zipper a -> Maybe (Zipper a)
goBack (_, [])        = Nothing
goBack (fs, (bc:bcs)) = Just (armar fs bc, bcs)
  where
    armar n (Crumb e ez ed) = Directory e (foldl' (flip (:)) (n:ed) ez)
\end{code}

Sube al principio del filesystem, asumiendo que no era Nothing,
entonces debe existir un camino hacia atras
\begin{code}
tothetop :: Zipper a -> Maybe (Zipper a)
tothetop (fs, []) = Just (fs, [])
tothetop x = tothetop $ (fromJust . goBack) x
\end{code}

Modify cambia el elemento guardado en el foco actual del zipper
con la funcion f
\begin{code}
modify :: (a -> a) -> Zipper a -> Zipper a
modify f (File a, bcs)          = (File (f a), bcs)
modify f (Directory a fs, bcs)  = (Directory (f a) fs, bcs) 
\end{code}

focus crea un zipper de un Filesystem
\begin{code}
focus :: Filesystem a -> Zipper a
focus f = (f, [])
\end{code}

defocus devuelve el Filesystem almacenado
\begin{code}
defocus :: Zipper a -> Filesystem a
defocus (f, _) = f
\end{code}

