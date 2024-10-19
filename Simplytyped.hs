module Simplytyped
  ( conversion
  ,    -- conversion a terminos localmente sin nombre
    eval
  ,          -- evaluador
    infer
  ,         -- inferidor de tipos
    quote          -- valores -> terminos
  )
where

import           Data.List
import           Data.Maybe
import           Prelude                 hiding ( (>>=) )
import           Text.PrettyPrint.HughesPJ      ( render )
import           PrettyPrinter
import           Common

-----------------------
-- conversion
-----------------------

-- conversion a términos localmente sin nombres
conversion :: LamTerm -> Term
conversion = conv []

conv :: [String] -> LamTerm -> Term
conv list (LVar x)     = if (i == -1 ) then Free (Global x) else Bound i where i = inList x list
conv list (LApp l1 l2) = (conv list l1) :@: (conv list l2)
conv list (LAbs x t l) = Lam t (conv (x:list) l)
conv list (LLet x u v) = Let (conv list u) (conv (x:list) v)

-- función que devuelve el índice del elemento en la lista, o -1 si no se encuentra en la misma.
inList :: String -> [String] -> Int
inList x list = case findIndex (== x) list of
  Nothing -> -1
  Just v -> v
  
----------------------------
--- evaluador de términos
----------------------------

-- substituye una variable por un término en otro término
sub :: Int -> Term -> Term -> Term
sub i t (Bound j) | i == j    = t
sub _ _ (Bound j) | otherwise = Bound j
sub _ _ (Free n   )           = Free n
sub i t (u   :@: v)           = sub i t u :@: sub i t v
sub i t (Lam t'  u)           = Lam t' (sub (i + 1) t u)

-- convierte un valor en el término equivalente
quote :: Value -> Term
quote (VLam t f) = Lam t f

termToVal :: Term -> Value
termToVal (Lam t f) = VLam t f

searchEnv :: Name -> NameEnv Value Type -> Term
searchEnv n env = quote v where (Just (v,t)) = lookup n env

-- evalúa un término en un entorno dado
-- preguntar que hacer con Bound
eval :: NameEnv Value Type -> Term -> Value 
eval env (Let (Lam t e) t2)      = eval env (sub 0 t2 (Lam t e))                    -- E-LetV
eval env (Let t1 t2)             = eval env (Let (quote (eval env t1)) t2)           -- E-Let
eval env ((Free e)       :@: t2) = eval env (searchEnv e env :@: t2)                 -- E-App1
eval env ((Let u v)      :@: t2) = eval env (quote (eval env (Let u v)) :@: t2)      -- E-App1
eval env ((t1' :@: t1'') :@: t2) = eval env (quote (eval env (t1' :@: t1'')) :@: t2) -- E-App1
eval env (t1@(Lam t e)   :@: t2) = case t2 of                                        
  (Free e')      -> eval env (t1 :@: (searchEnv e' env))                             -- E-App2
  (Lam t' e')    -> eval env (sub 0 t2 e)                                           -- E-AppAbs
  _ -> eval env (t1 :@: quote (eval env t2))                                         -- E-App2
eval env (Free x) = eval env (searchEnv x env)
eval env t1@(Lam t e) = termToVal t1
-- eval env (Bound i) = error

----------------------
--- type checker
-----------------------

-- infiere el tipo de un término
infer :: NameEnv Value Type -> Term -> Either String Type
infer = infer' []

-- definiciones auxiliares
ret :: Type -> Either String Type
ret = Right

err :: String -> Either String Type
err = Left

(>>=)
  :: Either String Type -> (Type -> Either String Type) -> Either String Type
(>>=) v f = either Left f v
-- fcs. de error

matchError :: Type -> Type -> Either String Type
matchError t1 t2 =
  err
    $  "se esperaba "
    ++ render (printType t1)
    ++ ", pero "
    ++ render (printType t2)
    ++ " fue inferido."

notfunError :: Type -> Either String Type
notfunError t1 = err $ render (printType t1) ++ " no puede ser aplicado."

notfoundError :: Name -> Either String Type
notfoundError n = err $ show n ++ " no está definida."

-- infiere el tipo de un término a partir de un entorno local de variables y un entorno global
infer' :: Context -> NameEnv Value Type -> Term -> Either String Type
infer' c _ (Bound i) = ret (c !! i) -- c[i]. significa que el tipo del bound esta guardado en el contexto local
infer' _ e (Free  n) = case lookup n e of -- el tipo del free está guardado en el contexto global
  Nothing     -> notfoundError n
  Just (_, t) -> ret t
infer' c e (t :@: u) = infer' c e t >>= \tt -> infer' c e u >>= \tu -> -- >>= es para concatenar operaciones monadicas
  case tt of
    FunT t1 t2 -> if (tu == t1) then ret t2 else matchError t1 tu -- el tipo de t debe ser una funcion
    _          -> notfunError tt
infer' c e (Lam t u) = infer' (t : c) e u >>= \tu -> ret $ FunT t tu
infer' c e (Let u v) = infer' c e u >>= \tu -> infer' (tu:c) e v 