module Eval1
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Prelude                 hiding ( fst
                                                , snd
                                                )
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado
newtype State a = State { runState :: Env -> Pair a Env }

instance Monad State where
  return x = State (\s -> (x :!: s))
  m >>= f = State (\s -> let (v :!: s') = runState m s in runState (f v) s')

-- Para calmar al GHC
instance Functor State where
  fmap = liftM

instance Applicative State where
  pure  = return
  (<*>) = ap

instance MonadState State where
  lookfor v = State (\s -> (lookfor' v s :!: s))
    where lookfor' v s = fromJust $ M.lookup v s
  update v i = State (\s -> (() :!: update' v i s)) where update' = M.insert

-- Ejercicio 1.b: Implementar el evaluador utilizando la monada State

-- Evalua un programa en el estado nulo
eval :: Comm -> Env
eval p = snd (runState (stepCommStar p) initEnv)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: MonadState m => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: MonadState m => Comm -> m Comm
stepComm Skip = return Skip

stepComm (Seq Skip c2) = return c2
stepComm (Seq c1 c2) = do
                        e1 <- stepComm c1
                        return (Seq e1 c2)
stepComm (Let r e) = do
                      v <- evalExp e
                      update r v
                      return Skip
stepComm (IfThenElse e c1 c2) = do
                              v  <- evalExp e        
                              if v
                                then return c1
                                else return c2
stepComm (While e1 c1 ) = do
                            v1 <- evalExp e1
                            case v1 of
                              True -> return (Seq c1 (While e1 c1) )
                              False -> return Skip       


-- Evalua una expresion
evalExp :: MonadState m => Exp a -> m a
evalExp (Const a) = return a
evalExp (Var r) = lookfor r
evalExp (UMinus e1 ) = do 
                        val <- evalExp e1
                        return (-val)
evalExp (Plus e1 e2) = do
                          v1 <- evalExp e1
                          v2 <- evalExp e2
                          return (v1+v2)
evalExp (Minus e1 e2) = do 
                           v1 <- evalExp e1
                           v2 <- evalExp e2
                           return (v1-v2)

evalExp (Times e1 e2) = do 
                          v1 <- evalExp e1
                          v2 <- evalExp e2
                          return (v1*v2)


evalExp (Div e1 e2) = do v1 <- evalExp e1
                         v2 <- evalExp e2
                         return (div v1 v2)

evalExp BTrue = return True
evalExp BFalse = return False
evalExp (Lt e1 e2) = do 
                        v1 <- evalExp e1
                        v2 <- evalExp e2
                        return (v1<v2)
evalExp (Gt e1 e2) = do 
                        v1 <- evalExp e1
                        v2 <- evalExp e2
                        return (v1>v2)
evalExp (And e1 e2) = do 
                        v1 <- evalExp e1
                        v2 <- evalExp e2
                        return (v1 && v2)
evalExp (Or e1 e2) = do 
                        v1 <- evalExp e1
                        v2 <- evalExp e2
                        return (v1 || v2)
evalExp (Not e1) = do 
                        v1 <- evalExp e1
                        return ( not v1)

evalExp (Eq e1 e2) = do
                      v1 <- evalExp e1
                      v2 <- evalExp e2
                      return (v1 == v2)
evalExp (NEq e1 e2) = do
                      v1 <- evalExp e1
                      v2 <- evalExp e2
                      return (v1 /= v2)
evalExp (EAssgn r e) = do
                        v1 <- evalExp e
                        update r v1
                        return v1
evalExp (ESeq e1 e2) = do
                        evalExp e1
                        evalExp e2
