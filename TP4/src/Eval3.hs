module Eval3
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
import           PPLis
import           Text.PrettyPrint

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Ejercicio 3.a: Proponer una nueva m\'onada que  
-- lleve una traza de ejecución (además de manejar errores y estado).
-- y dar su instancia de mónada. Llamarla |StateErrorTrace|. 
-- COMPLETAR


newtype StateErrorTrace a =
  StateErrorTrace { runStateErrorTrace :: Env -> (Either Error a, Env, Trace) }

instance Monad StateErrorTrace where
  return x = StateErrorTrace(\s -> (Right x, s, empty ) )
  m >>= f = StateErrorTrace (\s -> case ( ( runStateErrorTrace m ) s) of
                                    (Left err, s', tr) -> (Left err, s', tr )
                                    (Right x, s', tr) ->  case ( (runStateErrorTrace (f x)) s') of
                                                  (x', s'', tr') -> (x', s'', tr <+> tr')
                                     )

-- Recuerde agregar las siguientes instancias para calmar al GHC:
instance Functor StateErrorTrace where
  fmap = liftM

instance Applicative StateErrorTrace where
  pure  = return
  (<*>) = ap

-- Ejercicio 3.b: Resolver en Monad.hs


-- Ejercicio 3.c: Dar una instancia de MonadTrace para StateErrorTrace.
instance MonadTrace StateErrorTrace where
  write tr = StateErrorTrace (\s -> (Right (), s, tr))

-- Ejercicio 3.d: Dar una instancia de MonadError para StateErrorTrace.
-- COMPLETAR
instance MonadError StateErrorTrace where
  throw err = StateErrorTrace (\s -> ( Left err , s , empty ) )

-- Ejercicio 3.e: Dar una instancia de MonadState para StateErrorTrace.
-- COMPLETAR

instance MonadState StateErrorTrace where
  update var val = StateErrorTrace (\s -> ( Right () , M.insert var val s , empty ) )
  lookfor var = StateErrorTrace (\s ->  case (M.lookup var s) of 
                                          Nothing -> (Left UndefVar, s, empty)
                                          Just x -> ( Right x , s , empty) )

-- Ejercicio 3.f: Implementar el evaluador utilizando la monada StateErrorTrace.
-- Evalua un programa en el estado nulo

eval :: Comm -> Doc
eval c = let (x, s, tr) = runStateErrorTrace (stepCommStar c) initEnv
          in 
            tr $+$ text "Errores" <+> (text (show (either Just (const Nothing) x)) ) $+$ text "Estado" <+>  (text  (show s) )
            
--( either Just (const Nothing) x , s , tr)

--( either Just (const Nothing) x , s , tr)


--case runStateErrorTrace (stepCommStar c) initEnv of
--          (Left err, s, tr) -> (Just err, s, tr)
--          (Right x, s, tr) ->  (Nothing, s, tr)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadTrace m, MonadError m, MonadState m) => Comm -> m Comm
stepCommStar Skip = return Skip
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m, MonadTrace m) => Comm -> m Comm
stepComm Skip = return Skip

stepComm (Seq Skip c2) = return c2
stepComm (Seq c1 c2) = do
                        e1 <- stepComm c1
                        return (Seq e1 c2)
stepComm (Let r e) = do
                      v <- evalExp e
                      update r v
                      write ( text "Let " <+> text r <+> text " = " <+> text (show v) <+> text "\n")
                      return Skip
stepComm (IfThenElse e c1 c2) = do
                              v  <- evalExp e        
                              write ( text "## Eval " <+> text (renderExp e) <+>  text" ~> " <+> text (show v) <+> text "\n")
                              if v
                                then return c1
                                else return c2
stepComm (While e1 c1) = do
                            v1 <- evalExp e1
                            write ( text "## Eval " <+> text (renderExp e1) <+>  text" ~> " <+> text (show v1) <+> text "\n")
                            case v1 of
                              True -> return (Seq c1 (While e1 c1) )
                              False -> return Skip       


-- Evalua una expresion
evalExp :: (MonadState m, MonadError m, MonadTrace m) => Exp a -> m a
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
                        write ( text "Let " <+> text r <+> text " = " <+> text (show v1) <+> text "\n")
                        return v1
evalExp (ESeq e1 e2) = do
                        evalExp e1
                        evalExp e2