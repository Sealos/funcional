\begin{code}

import Data.List
import Data.Functor
import Data.Monoid
import Data.Foldable (foldMap)
import Data.Tree
import Data.Maybe

data Sample a = Sample { x :: [a], y :: a } deriving (Show)
data Hypothesis a = Hypothesis { c :: [a] } deriving (Show)

training = [
  Sample { x = [  0.1300098690745405, -0.2236751871685913 ], y = 399900 },
  Sample { x = [ -0.5041898382231769, -0.2236751871685913 ], y = 329900 },
  Sample { x = [  0.502476363836692, -0.2236751871685913 ], y = 369000 },
  Sample { x = [ -0.7357230646969468, -1.537766911784067 ], y = 232000 },
  Sample { x = [  1.257476015381594, 1.090416537446884 ], y = 539900 },
  Sample { x = [ -0.01973172848186497, 1.090416537446884 ], y = 299900 },
  Sample { x = [ -0.5872397998931161, -0.2236751871685913 ], y = 314900 },
  Sample { x = [ -0.7218814044186236, -0.2236751871685913 ], y = 198999 },
  Sample { x = [ -0.7810230437896409, -0.2236751871685913 ], y = 212000 },
  Sample { x = [ -0.6375731099961096, -0.2236751871685913 ], y = 242500 },
  Sample { x = [ -0.07635670234773261, 1.090416537446884 ], y = 239999 },
  Sample { x = [ -0.0008567371932424295, -0.2236751871685913 ], y = 347000 },
  Sample { x = [ -0.1392733399764744, -0.2236751871685913 ], y = 329999 },
  Sample { x = [  3.117291823687202,   2.40450826206236 ], y = 699900 },
  Sample { x = [ -0.9219563120780225, -0.2236751871685913 ], y = 259900 },
  Sample { x = [  0.3766430885792084,  1.090416537446884 ], y = 449900 },
  Sample { x = [ -0.856523008944131,  -1.537766911784067 ], y = 299900 },
  Sample { x = [ -0.9622229601604173, -0.2236751871685913 ], y = 199900 },
  Sample { x = [  0.7654679091248329,  1.090416537446884 ], y = 499998 },
  Sample { x = [  1.296484330711414,   1.090416537446884 ], y = 599000 },
  Sample { x = [ -0.2940482685431793, -0.2236751871685913 ], y = 252900 },
  Sample { x = [ -0.1417900054816241, -1.537766911784067 ], y = 255000 },
  Sample { x = [ -0.4991565072128776, -0.2236751871685913 ], y = 242900 },
  Sample { x = [ -0.04867338179108621, 1.090416537446884 ], y = 259900 },
  Sample { x = [  2.377392165173198,  -0.2236751871685913 ], y = 573900 },
  Sample { x = [ -1.133356214510595,  -0.2236751871685913 ], y = 249900 },
  Sample { x = [ -0.6828730890888036, -0.2236751871685913 ], y = 464500 },
  Sample { x = [  0.6610262906611214, -0.2236751871685913 ], y = 469000 },
  Sample { x = [  0.2508098133217248, -0.2236751871685913 ], y = 475000 },
  Sample { x = [  0.8007012261969283, -0.2236751871685913 ], y = 299900 },
  Sample { x = [ -0.2034483103577911, -1.537766911784067 ], y = 349900 },
  Sample { x = [ -1.259189489768079,  -2.851858636399542 ], y = 169900 },
  Sample { x = [  0.04947657290975102, 1.090416537446884 ], y = 314900 },
  Sample { x = [  1.429867602484346,  -0.2236751871685913 ], y = 579900 },
  Sample { x = [ -0.2386816274298865,  1.090416537446884 ], y = 285900 },
  Sample { x = [ -0.7092980768928753, -0.2236751871685913 ], y = 249900 },
  Sample { x = [ -0.9584479619026928, -0.2236751871685913 ], y = 229900 },
  Sample { x = [  0.1652431861466359,  1.090416537446884 ], y = 345000 },
  Sample { x = [  2.78635030976002,    1.090416537446884 ], y = 549000 },
  Sample { x = [  0.202993168723881,   1.090416537446884 ], y = 287000 },
  Sample { x = [ -0.4236565420583874, -1.537766911784067 ], y = 368500 },
  Sample { x = [  0.2986264579195686, -0.2236751871685913 ], y = 329900 },
  Sample { x = [  0.7126179335166897,  1.090416537446884 ], y = 314000 },
  Sample { x = [ -1.007522939253111,  -0.2236751871685913 ], y = 299000 },
  Sample { x = [ -1.445422737149154,  -1.537766911784067 ], y = 179900 },
  Sample { x = [ -0.1870899845743182,  1.090416537446884 ], y = 299900 },
  Sample { x = [ -1.003747940995387,  -0.2236751871685913 ], y = 239500 } ]
\end{code}

Valores iniciales
\begin{code}
alpha :: Double
alpha = 0.03

epsilon :: Double
epsilon = 0.0000001

guess :: Hypothesis Double
guess = Hypothesis { c = [0.0, 0.0, 0.0] }
\end{code}

Usamos el valor absoluto
\begin{code}
veryClose :: Double -> Double -> Bool
veryClose v0 v1 = abs(v0 - v1) < epsilon
\end{code}

Le agrega unos a todos las muestras
\begin{code}
addOnes :: [Sample Double] -> [Sample Double]
addOnes = foldl' addOne []
  where
    addOne a (Sample x y) =  (Sample (1:x) y):a
\end{code}

Se calcula el producto punto de los valores de la hipotesis
y una muestra
\begin{code}
theta :: Hypothesis Double -> Sample Double -> Double
theta (Hypothesis os) (Sample xs _) = foldl' dotProduct 0 $ zip os xs
  where
    dotProduct accumulator (a, b) = accumulator + a * b
\end{code}

Calcula la aproximacion de la muestra usando la hipotesis
Usa un foldl' con un contador que lleva el numero de elementos
y el total y luego aplica la formula
\begin{code}
square :: Num a => a -> a
square x = x * x

err :: Hypothesis Double -> Sample Double -> Double
err h (Sample os y) =  (theta h (Sample os y)) - y 

cost :: Hypothesis Double -> [Sample Double] -> Double
cost h ss = formula $ aggregate
  where
    pack = zip ss $ repeat h
    aggregate = foldl' (\ (count, sum) (ss, h) -> (count + 1, (+) sum (square $ err h ss))) (0, 0) $ pack 
    formula (count, sum) = sum / (count * 2)
\end{code}

Similar al anterior, se generan tuplas de (posicion,valor) y
con la posicion se puede usar la formula, contando la cantidad de
muestras y el valor resultante
\begin{code}
descend :: Double -> Hypothesis Double -> [Sample Double] -> Hypothesis Double
descend alpha h ss = build
  where
    build = Hypothesis . reverse $ foldl' (\ c (value, position) -> (:) (value - (applyFormula(samplesError position))) c) [] packLength
    applyFormula (count, value) = alpha * value / count
    packLength = zip (c h) [0..]
    samplesError position = foldl' (\ (count, sum) sample -> (count + 1, (+) sum $ innerFormula sample position)) (0, 0) ss
    innerFormula sample position = (*) (err h sample) $ (x sample) !! position
\end{code}

Se usa un unfold que para cuando la diferencia de las hipotesis es aceptada por
la funcion veryClose
\begin{code}
gd :: Double -> Hypothesis Double -> [Sample Double] -> [(Integer, Hypothesis Double, Double)]
gd alpha h s = unfoldr generateHypothesis (0, h, cost h ss)
  where
    ss = addOnes s
    generateHypothesis (count, oldH, oldY) = 
      case (compareHypothesis oldH (newH oldH)) of
        False -> Just ((count, oldH, oldY), (count + 1, newH oldH, newY (newH oldH)))
        True -> Nothing
    newH oldH = descend alpha oldH ss
    newY newH = cost newH ss
    compareHypothesis h1 h2 = veryClose (cost h1 ss) (cost h2 ss)
\end{code}

En el monoide, se delcara un newtype Max, que es el monoide
que la operacion binaria devuelve siempre el maximo entre dos
elementos y como elemento neutro Nothing
\begin{code}
newtype Max a = Max { getMax :: Maybe a }
  deriving ( Eq, Ord, Read, Show )

gMax :: Ord a => Max a -> Max a -> Max a
gMax (Max Nothing) x = x
gMax x (Max Nothing) = x
gMax (Max (Just x)) (Max (Just y)) = Max $ Just $ max x y

instance Ord a => Monoid (Max a) where
  mempty = Max Nothing
  mappend = (gMax)
\end{code}

Zipper
\begin{code}
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
