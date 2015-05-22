import Data.Char
import Data.Maybe
import Data.Either
import qualified Data.Map      as DM
import Data.Sequence as DS
import Data.Foldable as DF
import qualified Data.List as DL
import Control.Monad.Trans.Reader
import Control.Monad
import Control.Monad.Trans.Error
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Control.Monad.Trans.IdentityT

data Exp = Const Int
         | Var String
         | Add Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
         | Div Exp Exp
         deriving (Eq,Show)

type Env = DM.Map String Int

env :: Env
env = DM.fromList [ ("foo",42), ("bar",69), ("baz",27), ("qux",0) ]

eval (Const i) = i
eval (Var n)   = fromJust (DM.lookup n env)
eval (Add l r) = (eval l) + (eval r)
eval (Sub l r) = (eval l) - (eval r)
eval (Mul l r) = (eval l) * (eval r)
eval (Div l r) = (eval l) `div` (eval r)

ex1 = (Const 21)
ex2 = (Var "foo")
ex3 = (Mul (Const 2) ex1)
ex4 = (Add (Mul ex2 (Const 5)) (ex3))
ex5 = (Add (Div (Const 42) (Const 2)) (Mul (Const 3) (Const 7)))

type Eval1 a = IdentityT a

--eval1 :: Env -> Exp -> Eval1 Int
eval1 env (Const i) = return i
eval1 env (Var n)   = return $ fromJust $ DM.lookup n env
eval1 env (Add l r) = do i1 <- eval1 env l
                         i2 <- eval1 env r
                         return $ i1 + i2
eval1 env (Sub l r) = do i1 <- eval1 env l
                         i2 <- eval1 env r
                         return $ i1 - i2
eval1 env (Mul l r) = do i1 <- eval1 env l
                         i2 <- eval1 env r
                         return $ i1 * i2
eval1 env (Div l r) = do i1 <- eval1 env l
                         i2 <- eval1 env r
                         return $ i1 `div` i2

evalM1 :: Eval1 a -> a
evalM1 = runIdentityTT

data ExpError = DivisionPorCero
              | NumeroDeMalaSuerte
              | VariableNoExiste String
              deriving (Show)

instance Error ExpError

type Eval2 a = ErrorT ExpError IdentityT a

eval2 :: Env -> Exp -> Eval2 Int
eval2 env (Const i) = checkForThirteen i
eval2 env (Var n)   = maybe (throwError $ VariableNoExiste n) 
                            checkForThirteen
                            (DM.lookup n env)
eval2 env (Add l r) = checkMath env (+) l r
eval2 env (Sub l r) = checkMath env (-) l r
eval2 env (Mul l r) = checkMath env (*) l r
eval2 env (Div l r) = do
  vl <- eval2 env l
  vr <- eval2 env r
  if vr == 0 then throwError DivisionPorCero
             else checkForThirteen $ vl `div` vr

checkMath e op l r = liftM2 (op) (eval2 e l) (eval2 e r) >>= checkForThirteen

checkForThirteen i = if i == 13 then throwError NumeroDeMalaSuerte
                                else return i


evalM2 :: Eval2 a -> Either ExpError a
evalM2 = runIdentityTT . runErrorT 

type Eval3 a = ReaderT Env (ErrorT ExpError IdentityT) a

eval3 :: Exp -> Eval3 Int
eval3 (Const i) = checkForThirteen i
eval3 (Var n)   = do env <- ask
                     maybe (throwError $ VariableNoExiste n) 
                           checkForThirteen
                           (DM.lookup n env)
eval3 (Add l r) = checkMath' (+) l r
eval3 (Sub l r) = checkMath' (-) l r
eval3 (Mul l r) = checkMath' (*) l r
eval3 (Div l r) = do
  vl <- eval3 l
  vr <- eval3 r
  if vr == 0 then throwError DivisionPorCero
             else checkForThirteen $ vl `div` vr

checkMath' op l r = liftM2 (op) (eval3 l) (eval3 r) >>= checkForThirteen


evalM3 :: Env -> Eval3 a -> Either ExpError a
evalM3 env = runIdentityTT . runErrorT . (flip runReaderT) env

data EvalState = EvalState { adds, subs, muls, divs, vars, tabs :: Int }
               deriving (Show)

initialState = EvalState { 
                           adds = 0,
                           subs = 0,
                           muls = 0,
                           divs = 0,
                           vars = 0,
                           tabs = 0
                         }

type Eval4 a = ReaderT Env (ErrorT ExpError (StateT EvalState IdentityT)) a

eval4 :: Exp -> Eval4 Int
eval4 (Const i) = checkForThirteen i
eval4 (Var n)   = do s <- get
                     env <- ask
                     case DM.lookup n env of
                       Nothing -> throwError $ VariableNoExiste n
                       Just v  -> do put $ s { vars = vars s + 1}
                                     checkForThirteen v
eval4 (Add l r) = do
  vl <- eval4 l
  vr <- eval4 r
  s  <- get
  put $ s { adds = adds s + 1 }
  checkForThirteen $ vl + vr
eval4 (Sub l r) = do
  vl <- eval4 l
  vr <- eval4 r
  s  <- get
  put $ s { subs = subs s + 1 }
  checkForThirteen $ vl - vr
eval4 (Mul l r) = do
  vl <- eval4 l
  vr <- eval4 r
  s  <- get
  put $ s { muls = muls s + 1 }
  checkForThirteen $ vl * vr
eval4 (Div l r) = do
  vl <- eval4 l
  vr <- eval4 r
  s  <- get
  if vr == 0 then throwError DivisionPorCero
             else do put $ s { divs = divs s + 1 }
                     checkForThirteen $ vl `div` vr

evalM4 :: Env -> EvalState -> Eval4 a -> (Either ExpError a,EvalState)
evalM4 env init = runIdentityTT . (flip runStateT init) . runErrorT .
                  (flip runReaderT) env

type ExpLog = DS.Seq String

type Eval5 a = ReaderT Env (ErrorT ExpError 
                           (WriterT ExpLog (StateT EvalState IdentityT))) a

eval5 :: Exp -> Eval5 Int
eval5 t@(Const i) = do
  tell $ logExp t i
  checkForThirteen i
eval5 t@(Var n)   = do s <- get
                       env <- ask
                       case DM.lookup n env of
                         Nothing -> throwError $ VariableNoExiste n
                         Just v  -> do put $ s { vars = vars s + 1}
                                       tell $ logExp t v
                                       checkForThirteen v
eval5 t@(Add l r) = do
  vl <- eval5 l
  vr <- eval5 r
  s  <- get
  let v = vl + vr
  put $ s { adds = adds s + 1 }
  tell $ logExp t v
  checkForThirteen v
eval5 t@(Sub l r) = do
  vl <- eval5 l
  vr <- eval5 r
  s  <- get
  let v = vl - vr
  put $ s { subs = subs s + 1 }
  tell $ logExp t v
  checkForThirteen v
eval5 t@(Mul l r) = do
  vl <- eval5 l
  vr <- eval5 r
  s  <- get
  let v = vl * vr
  put $ s { muls = muls s + 1 }
  tell $ logExp t v
  checkForThirteen v
eval5 t@(Div l r) = do
  vl <- eval5 l
  vr <- eval5 r
  s  <- get
  if vr == 0 then throwError DivisionPorCero
             else do let v = vl `div` vr
                     put $ s { divs = divs s + 1 }
                     tell $ logExp t v
                     checkForThirteen v

logExp :: Exp -> Int -> Seq String
logExp e v = singleton $ "Exp: " ++ show e ++ " -> Val: " ++ show v

evalM5 :: Env -> EvalState -> Eval5 a -> ((Either ExpError a,ExpLog),EvalState)
evalM5 env init = runIdentityTT . (flip runStateT init) . runWriterT .
                  runErrorT .  (flip runReaderT) env

type Eval6 a = ReaderT Env (ErrorT ExpError (WriterT ExpLog (StateT EvalState IO))) a

evalM6 :: Env -> EvalState -> Eval6 a -> IO ((Either ExpError a,ExpLog),EvalState)
evalM6 env init = (flip runStateT init) . runWriterT .
                  runErrorT .  (flip runReaderT) env
--evalM6 env initialState $ eval6 ex5
eval6 :: Exp -> Eval6 Int
eval6 t@(Const i) = do
  s <- get
  tell $ logExp t i
  lift $ putStrLn $ indent (tabs s) (show i)
  checkForThirteen i
eval6 t@(Var n)   = do s <- get
                       env <- ask
                       case DM.lookup n env of
                         Nothing -> throwError $ VariableNoExiste n
                         Just v  -> do put $ s { vars = vars s + 1}
                                       tell $ logExp t v
                                       lift $ putStrLn $ indent (tabs s) 
                                                                  (show v)
                                       checkForThirteen v
eval6 t@(Add l r) = do
  p <- get
  lift $ putStrLn $ indent (tabs p) "+"
  put $ p { tabs = tabs p + 2 }
  vl <- eval6 l
  vr <- eval6 r
  s  <- get
  let v = vl + vr
  put $ s { adds = adds s + 1, tabs = tabs s - 2 }
  tell $ logExp t v
  checkForThirteen v
eval6 t@(Sub l r) = do
  p <- get
  lift $ putStrLn $ indent (tabs p) "-"
  put $ p { tabs = tabs p + 2 }
  vl <- eval6 l
  vr <- eval6 r
  s  <- get
  let v = vl - vr
  put $ s { subs = subs s + 1, tabs = tabs s - 2 }
  tell $ logExp t v
  checkForThirteen v
eval6 t@(Mul l r) = do
  p <- get
  lift $ putStrLn $ indent (tabs p) "*"
  put $ p { tabs = tabs p + 2 }
  vl <- eval6 l
  vr <- eval6 r
  s  <- get
  let v = vl * vr
  put $ s { muls = muls s + 1, tabs = tabs s - 2 }
  tell $ logExp t v
  checkForThirteen v
eval6 t@(Div l r) = do
  p <- get
  lift $ putStrLn $ indent (tabs p) "/"
  put $ p { tabs = tabs p + 2 }
  vl <- eval6 l
  vr <- eval6 r
  s  <- get
  if vr == 0 then throwError DivisionPorCero
             else do let v = vl `div` vr
                     put $ s { divs = divs s + 1, tabs = tabs s - 2 }
                     tell $ logExp t v
                     checkForThirteen v

indent n s = DL.replicate n ' ' ++ s
