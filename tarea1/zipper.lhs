\begin{code}

import Data.List
import Data.Functor
import Data.Monoid
import Data.Foldable (foldMap)
import Data.Tree

type Files a = [Filesystem a]
data Filesystem a = File a | Directory a (Files a)

data Crumbs a =
 
data Breadcrumbs a = [Crumbs a]
type Zipper a = (Filesystem a, Breadcrumbs a)

goDown :: Zipper a -> Zipper a
goDown x = undefined

goRight :: Zipper a -> Zipper a
goRight x = undefined

goLeft :: Zipper a -> Zipper a
goLeft x = undefined

goBack :: Zipper a -> Zipper a
goBack x = undefined

tothetop :: Zipper a -> Zipper a
tothetop x = undefined

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (fs, z) = (f fs, z)

focus :: Filesystem a -> Zipper a
focus f = (f, [])

defocus :: Zipper a -> Filesystem a
defocus (f, _) = f

\end{code}
