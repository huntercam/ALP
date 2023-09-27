module Untyped where

import           Control.Monad
import           Data.List
import           Data.Maybe

import           Common

----------------------------------------------
-- Seccón 2  
-- Ejercicio 2: Conversión a términos localmente sin nombres
----------------------------------------------

conversion :: LamTerm -> Term
conversion l = conversion' l []

conversion' (LVar var) nameEnv = case (elemIndex var nameEnv) of
                                Nothing -> Free (Global var)
                                Just i -> Bound i
conversion' (App l1 l2) nameEnv = (conversion' l1 nameEnv) :@: (conversion' l2 nameEnv)
conversion' (Abs var l) nameEnv = Lam (conversion' l var:nameEnv)

-------------------------------
-- Sección 3
-------------------------------

vapp :: Value -> Value -> Value
vapp (Vlam f) v = f v
vapp (Vneutral n) v = Vneutral ( Napp n v)

eval :: NameEnv Value -> Term -> Value
eval e t = eval' t (e, [])

eval' :: Term -> (NameEnv Value, [Value]) -> Value
eval' (Bound ii)  (_, lEnv) = lEnv !! ii
eval' (Free name) (nameEnv, _) = case (buscar name nameEnv) of
                                Nothing -> VNeutral (NFree name)
                                Just v -> v
eval' (Lam t) (nameEnv, lEnv)     = VLam (\x -> eval' t (nameEnv, x:lEnv))

eval' (l1 :@: l2) (nameEnv, lEnv) = case l1 of
                        Lam m -> eval' m (nameEnv, l2:lEnv)
                        _     -> let
                                    a = eval' l1 (nameEnv, lEnv)
                                    b = eval' l2 (nameEnv, lEnv)
                                in case a of
                                    VLam f -> f b
                                    Vneutral n -> 

                                    b = eval' 
                         a = eval' l1 state
                                 let b = eval' l2 state
                                 in 

-- (\y.y) w

[]
    z (\x.x) id
->  z id
[x, y, z]
[x, x, z]
[id, id, 3]
(\x \x \z x z) 3 id id
[1, 8, 3]
--------------------------------
-- Sección 4 - Mostrando Valores
--------------------------------

quote :: Value -> Term
quote = undefined






