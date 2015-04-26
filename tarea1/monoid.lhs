\begin{code}

import Data.List
import Data.Functor
import Data.Monoid
import Data.Foldable (foldMap)
import Data.Tree

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

