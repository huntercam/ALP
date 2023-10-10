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

conversion' (LVar var) lEnv = case (elemIndex var lEnv) of
                                Nothing -> Free (Global var)
                                Just i -> Bound i
conversion' (App l1 l2) lEnv = (conversion' l1 lEnv) :@: (conversion' l2 lEnv)
conversion' (Abs var l) lEnv = Lam (conversion' l (var:lEnv))

-------------------------------
-- Sección 3
-------------------------------

vapp :: Value -> Value -> Value
vapp (VLam f) v = f v
vapp (VNeutral n) v = VNeutral ( NApp n v)

eval :: NameEnv Value -> Term -> Value
eval e t = eval' t (e, [])

buscar [] name2 = Nothing
buscar ( (name,value):xs) name2 = if name == name2 then (Just value) else ( buscar xs name2 )

eval' :: Term -> (NameEnv Value, [Value]) -> Value
eval' (Bound ii)  (_, lEnv) = lEnv !! ii
eval' (Free name) (nameEnv, _) = case (buscar nameEnv name) of
                                Nothing -> VNeutral (NFree name)
                                Just v -> v
eval' (Lam t) (nameEnv, lEnv)     = VLam (\x -> eval' t (nameEnv, x:lEnv))
eval' (l1 :@: l2) s = let
                        v1 = eval' l1 s
                        v2 = eval' l2 s
                        in
                        vapp v1 v2

--------------------------------
-- Sección 4 - Mostrando Valores
--------------------------------

quote :: Value -> Term
quote v = quote' v 0
-- quote = undefined

quote' :: Value -> Int -> Term
quote' (VLam f) i  = Lam (quote' (f (VNeutral(NFree (Quote i) ) ) ) (i+1) )  
quote' (VNeutral vn) i = case vn of 
                    NFree name -> case name of
                                Global s -> (Free name)
                                Quote k -> (Bound (i-k-1) )
                    NApp nn v -> (quote' (VNeutral nn) i ) :@: (quote' v i)





