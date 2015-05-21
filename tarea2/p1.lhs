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
               Lambda { from = Node 3, to = Node 2 },
               Lambda { from = Node 0, to = Node 1 },
               Move { from = Node 0, to = Node 0, sym = 'a' },
               Move { from = Node 0, to = Node 1, sym = 'a' },
               Move { from = Node 1, to = Node 2, sym = 'b' },
               Move { from = Node 2, to = Node 3, sym = 'b' }
             ],
             initial = Node 0,
             final = DS.fromList [ Node 3 ]
           }

transitionGen :: [Char] -> [NFANode] -> Gen Transition
transitionGen sigma states = frequency [
                              (1, genLambda),
                              (5, genMove)
                            ]
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

instance Arbitrary NFANode where
  arbitrary = return . Node . getPositive =<< arbitrary

instance Arbitrary NFA where
  arbitrary = sized $ \n -> do
                sigma       <- listOf1 (choose ('a', 'z'))
                states      <- listOf1 (arbitrary::Gen NFANode)
                final       <- listOf $ elements (n0:states)
                transitions <- resize (3*n) $ listOf $ transitionGen sigma (n0:states)
                return $ NFA (DS.fromList sigma) (DS.fromList (n0:states)) (DS.fromList transitions) n0 (DS.fromList final)
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
                newElems  = DS.unions $ DS.foldl' (\a e -> (f e):a) [] s
                newSet    = DS.union s newElems
              in
                case newSet == s of
                  True  -> s
                  False -> fixSet f newSet

destinations :: NFA -> Char -> NFANode -> DS.Set NFANode
destinations nfa char n = fixSet expand $ DS.union moveConsume $ fixSet expand $ DS.singleton n
  where
    lambdaConsume node  = DS.filter (\e -> isLambda e && from e == node) transitions
    transitions         = moves nfa
    expand node         = DS.map to $ lambdaConsume node
    moveConsume         = DS.map to $ DS.filter (\e-> isMove e && sym e == char && from e == n) transitions


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
initialState word = NFARun word (DS.singleton (Node 0))

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