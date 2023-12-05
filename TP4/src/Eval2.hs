module Eval2
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado, con manejo de errores
newtype StateError a =
  StateError { runStateError :: Env -> Either Error ( Pair a Env) }

-- Para calmar al GHC
instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure  = return
  (<*>) = ap

-- Ejercicio 2.a: Dar una instancia de Monad para StateError:
instance Monad StateError where
  return x = StateError (\s -> Right (x :!: s) )
  m >>= f = StateError (\s -> case (runStateError m s) of
                              Left error    -> Left error 
                              Right (v :!: s') -> (runStateError  (f v)) s' )

-- Ejercicio 2.b: Dar una instancia de MonadError para StateError:
-- COMPLETAR
instance MonadError StateError where
  throw err = StateError (\s -> Left err )

-- Ejercicio 2.c: Dar una instancia de MonadState para StateError:
-- COMPLETAR
instance MonadState StateError where
    -- Busca el valor de una variable
    -- lookfor :: Variable -> m Int
    lookfor var = StateError (\s -> case (M.lookup var s) of
                                    Just x -> Right (x :!: s)
                                    Nothing -> Left UndefVar )
    -- Cambia el valor de una variable
    -- update :: Variable -> Int -> m ()
    update var val = StateError (\s -> Right (() :!: M.insert var val s))

-- Ejercicio 2.d: Implementar el evaluador utilizando la monada StateError.
-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error Env
eval c1 =  case (runStateError (stepCommStar c1) initEnv) of 
            Left err          -> Left err
            Right (a :!: env) -> Right env

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadState m, MonadError m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m) => Comm -> m Comm
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
evalExp :: (MonadState m, MonadError m) => Exp a -> m a
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
                         case v2 of
                          0 -> throw DivByZero
                          x -> return (div v1 v2)
                         
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
