\begin{code}
import Control.Monad
import Control.Monad.RWS
import Control.Monad.Error
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


nfa0 = NFA {
             sigma  = DS.fromList "ab",
             states = DS.fromList $ fmap Node [0..3],
             moves  = DS.fromList [
               Move { from = Node 0, to = Node 0, sym = 'a' },
               Move { from = Node 0, to = Node 0, sym = 'a' },
               Move { from = Node 0, to = Node 1, sym = 'a' },
               Move { from = Node 1, to = Node 2, sym = 'b' },
               Move { from = Node 2, to = Node 3, sym = 'b' }
             ],
             initial = Node 0,
             final = DS.fromList [ Node 3 ]
           }

instance Arbitrary NFANode where
  arbitrary = undefined

instance Arbitrary NFA where
  arbitrary = undefined


isMove, isLambda :: Transition -> Bool
isMove   = undefined
isLambda = undefined

lambdaMoves :: NFA -> NFANode -> DS.Set NFANode
lambdaMoves = undefined

normalMoves :: NFA -> Char -> NFANode -> DS.Set NFANode
normalMoves = undefined

destinations :: NFA -> Char -> NFANode -> DS.Set NFANode
destinations = undefined

fixSet :: Ord a => (a -> DS.Set a) -> DS.Set a -> DS.Set a
fixSet f s = undefined

runNFA :: NFA -> [Char] -> IO ()
runNFA nfa word = undefined

data NFAReject = Stuck (DS.Set NFANode) String
               | Reject (DS.Set NFANode)
               deriving (Show)
instance Error NFAReject

data NFARun = NFARun { w :: String, qs :: DS.Set NFANode }
            deriving (Show,Eq)


initialState :: String -> NFARun
initialState word = undefined

accepting :: NFA -> DS.Set NFANode -> Bool
accepting = undefined

prop_acceptsemptyword :: NFA -> Property
prop_acceptsemptyword nfa = undefined

prop_acceptancelength :: NFA -> String -> Property
prop_acceptancelength nfa w = undefined

\end{code}