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
conv list (LVar x)      = if (i == -1 ) then Free (Global x) else Bound i where i = inList x list
conv list (LApp l1 l2)  = (conv list l1) :@: (conv list l2)
conv list (LAbs x t l)  = Lam t (conv (x:list) l)
conv list (LLet x u v)  = Let (conv list u) (conv (x:list) v)
conv list LZero         = Zero 
conv list (LSuc l)      = Suc (conv list l)
conv list (LRec x u v)  = Rec (conv list x) (conv list u) (conv list v)
conv list LNil          = Nil
conv list (LCons u v)   = Cons (conv list u) (conv list v)
conv list (LRecL x u v) = RecL (conv list x) (conv list u) (conv list v)

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
sub i t (Let u   v)           = Let (sub i t u) (sub (i + 1) t v)
sub i t Zero                  = Zero
sub i t (Suc n)               = Suc (sub i t n)
sub i t (Rec x u v)           = Rec (sub i t x) (sub i t u) (sub i t v)
sub i t Nil                   = Nil
sub i t (Cons n lv)           = Cons (sub i t n) (sub i t lv)
sub i t (RecL x u v)          = RecL (sub i t x) (sub i t u) (sub i t v)

-- convierte un valor en el término equivalente
quote :: Value -> Term
quote (VLam t f)           = Lam t f
quote (VNum NZero)         = Zero
quote (VNum (NSuc n))      = Suc (quote (VNum n))
quote (VList VNil)         = Nil
quote (VList (VCons n lv)) = Cons (quote (VNum n)) (quote (VList lv))

-- convierte un término en el valor equivalente
termToVal :: Term -> Value
termToVal (Lam t f)   = VLam t f
termToVal Zero        = (VNum NZero)        
termToVal (Suc n)     = (VNum (NSuc a)) where (VNum a) = (termToVal n)       
termToVal Nil         = (VList VNil)        
termToVal (Cons n lv) = (VList (VCons a b)) where ((VNum a), (VList b)) = ((termToVal n), (termToVal lv))

searchEnv :: Name -> NameEnv Value Type -> Term
searchEnv n env = quote v where (Just (v,t)) = lookup n env

-- evalúa un término en un entorno dado
eval :: NameEnv Value Type -> Term -> Value 
eval env (Free x)           = eval env (searchEnv x env)                                        
eval env (Bound i)          = error "mal tipo en bound"
eval env t1@(Lam t e)       = termToVal t1                                                                                         
eval env (Let (Lam t e) t2) = eval env (sub 0 t2 (Lam t e))                           -- E-LetV
eval env (Let t1 t2)        = eval env (Let (quote (eval env t1)) t2)                 -- E-Let
eval env (t1@(Lam t e) :@: t2) = case t2 of
  (Lam t' e') -> eval env (sub 0 t2 e)                                                -- E-AppAbs
  Zero        -> eval env (sub 0 t2 e)                                                -- E-AppAbs
  (Suc n)     -> eval env (sub 0 t2 e)                                                -- E-AppAbs
  Nil         -> eval env (sub 0 t2 e)                                                -- E-AppAbs
  (Cons n l)  -> eval env (sub 0 t2 e)                                                -- E-AppAbs
  _           -> eval env (t1 :@: (quote (eval env t2)))                              -- E-App2
eval env (t1 :@: t2)           = eval env (quote (eval env t1) :@: t2)                -- E-App1
eval env Zero                  = VNum NZero
eval env Nil                   = VList VNil
eval env (Cons n lv)           = case (a,b) of
  (VNum numval, VList listval) -> VList (VCons numval listval)                        -- E-Cons1 E-Cons2
  _                            -> error "mal tipo en cons"
  where (a,b) = (eval env n, eval env lv)
eval env (Suc e)               = case a of
  VNum numval -> VNum (NSuc numval)
  _           -> error "mal tipo en suc"
  where a = eval env e
eval env (Rec e1 e2 e3)        = case e3 of      
  Zero        -> eval env e1                                                          -- E-RZero                       
  (Suc e)     -> eval env (e2 :@: (Rec e1 e2 e) :@: e)                                -- E-RSucc
  _           -> eval env (Rec e1 e2 (quote (eval env e3)))                           -- E-R
eval env (RecL e1 e2 e3)        = case e3 of
  Nil         -> eval env e1                                                          -- E-RNil
  (Cons n lv) -> eval env (e2 :@: n :@: lv :@: (RecL e1 e2 lv))                       -- E-RCons
  _           -> eval env (RecL e1 e2 (quote (eval env e3)))                          -- E-RL

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
infer' c e Zero      = ret NatT
infer' c e Nil        = ret ListT
infer' c e (Suc u)   = case infer' c e u of 
  Right NatT -> ret NatT
  Right t    -> matchError NatT t
  Left e     -> Left e
infer' c e (Cons u v) = infer' c e u >>= \t1 -> infer' c e v >>= \t2 ->
  case t1 of
    NatT -> case t2 of
              ListT -> ret ListT
              _     -> matchError ListT t2
    _    -> matchError NatT t1
infer' c e (Rec e1 e2 e3) = infer' c e e1 >>= \t1 -> infer' c e e2 >>= \t2 -> infer' c e e3 >>= \t3 ->
  case t2 of 
    FunT tu (FunT NatT tv)              -> if (tu == t1 && tu == tv) then (if (t3 == NatT) then ret t1 else matchError NatT t3) else matchError t1 tu
    _                                   -> notfunError t2
infer' c e (RecL e1 e2 e3) = infer' c e e1 >>= \t1 -> infer' c e e2 >>= \t2 -> infer' c e e3 >>= \t3 ->
  case t2 of 
    FunT NatT (FunT ListT (FunT tu tv)) -> if (tu == t1 && tu == tv) then (if (t3 == ListT) then ret t1 else matchError ListT t3) else matchError t1 tu
    _                                   -> notfunError t2