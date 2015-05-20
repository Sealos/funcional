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
               Lambda { from = Node 0, to = Node 3 },
               Lambda { from = Node 0, to = Node 1 },
               Move { from = Node 0, to = Node 0, sym = 'a' },
               Move { from = Node 0, to = Node 1, sym = 'a' },
               Move { from = Node 1, to = Node 2, sym = 'b' },
               Move { from = Node 2, to = Node 3, sym = 'b' }
             ],
             initial = Node 0,
             final = DS.fromList [ Node 3 ]
           }

instance Arbitrary NFANode where
  arbitrary = return . Node . getPositive =<< arbitrary

genToSet :: (Arbitrary a, Ord a) => Gen a -> Gen (DS.Set a)
genToSet gen = sized (makeGen gen)

makeGen :: (Arbitrary a, Ord a) => Gen a -> Int -> Gen (DS.Set a)
makeGen gen n
        | n <= 0    = do
                      a <- gen
                      return $ DS.singleton a
        | otherwise = do
                      l <- makeGen gen (n - 1) 
                      r <- makeGen gen (n - 1)
                      return $ DS.union l r

makeMoves :: Int -> DS.Set Char -> DS.Set NFANode -> DS.Set Transition
makeMoves n sigma states = DS.empty

makeFinal :: DS.Set NFANode -> DS.Set NFANode
makeFinal nodes = fst $ DS.foldl' (\(a, b) e -> (union b (singleton e), a)) (empty, empty) nodes 
  where
    empty     = DS.empty
    union     = DS.union
    singleton = DS.singleton

instance Arbitrary NFA where
  arbitrary = sized $ \n -> do
                sigma <- resize (n `div` 2) (genToSet (choose ('a', 'z')))
                states <- resize n (genToSet arbitrary)
                return $ NFA sigma (DS.union states (DS.singleton n0)) (makeMoves n sigma states) n0 (makeFinal states)
            where
              n0 = Node 0

isValid :: NFA -> Bool
isValid = undefined

isMove :: Transition -> Bool
isMove = not . isLambda

isLambda :: Transition -> Bool
isLambda (Lambda _ _) = True
isLambda _            = False

lambdaMoves :: NFA -> NFANode -> DS.Set NFANode
lambdaMoves nfa n = DS.map to validLambda
  where validLambda = DS.filter (\s -> isLambda s && n == from s) $ moves nfa

normalMoves :: NFA -> Char -> NFANode -> DS.Set NFANode
normalMoves nfa c n = DS.map to validTransitions
  where validTransitions = DS.filter (\s -> isMove s && n == from s && c == sym s) m
        m = moves nfa

fixSet :: Ord a => (a -> DS.Set a) -> DS.Set a -> DS.Set a
fixSet f s =  let
                newElems  = s
                newSet    = DS.union s newElems
              in
                case newSet == s of
                  True  -> s
                  False -> fixSet f newSet

destinations :: NFA -> Char -> NFANode -> DS.Set NFANode
destinations = undefined

data NFAReject = Stuck (DS.Set NFANode) String
               | Reject (DS.Set NFANode)
               deriving (Show)
instance Error NFAReject

-- Monad State
data NFARun = NFARun { w :: String, qs :: DS.Set NFANode }
            deriving (Show,Eq)
            
type Log = Seq.Seq String
type RWS1 = RWS NFA Log NFARun
type RWSNFA = ErrorT NFAReject RWS1

runNFA :: NFA -> [Char] -> IO ()
runNFA nfa word = undefined

initialState :: String -> NFARun
initialState word = NFARun word (Node 0)

accepting :: NFA -> DS.Set NFANode -> Bool
accepting nfa = DS.foldl' isFinal False
  where
    fs      = final nfa
    isFinal b n = b || DS.member n fs

prop_acceptsemptyword :: NFA -> Property
prop_acceptsemptyword nfa = undefined

prop_acceptancelength :: NFA -> String -> Property
prop_acceptancelength nfa w = undefined

\end{code}