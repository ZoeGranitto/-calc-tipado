module PrettyPrinter
  ( printTerm  ,     -- pretty printer para terminos
    printType        -- pretty printer para tipos
  )
where

import  Common
import  Text.PrettyPrint.HughesPJ
import  Prelude hiding ((<>))

-- lista de posibles nombres para variables
vars :: [String]
vars =
  [ c : n
  | n <- "" : map show [(1 :: Integer) ..]
  , c <- ['x', 'y', 'z'] ++ ['a' .. 'w']
  ]

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

-- pretty-printer de términos

pp :: Int -> [String] -> Term -> Doc
pp ii vs (Bound k         ) = text (vs !! (ii - k - 1))
pp _  _  (Free  (Global s)) = text s

pp ii vs (i :@: c         ) = sep
  [ parensIf (isLam i) (pp ii vs i)
  , nest 1 (parensIf (isLam c || isApp c) (pp ii vs c))
  ]
pp ii vs (Lam t c) =
  text "\\"
    <> text (vs !! ii)
    <> text ":"
    <> printType t
    <> text ". "
    <> pp (ii + 1) vs c
pp ii vs (Let u v) = 
  text "let " 
    <> text (vs !! ii)
    <> text " = "
    <> pp ii vs u
    <> text " in "
    <> pp (ii + 1) vs v
pp ii vs (Zero ) = text "0"
pp ii vs (Nil  ) = text "nil"
pp ii vs (Suc u) = 
  parensIf True (
    text "suc "
    <> (pp ii vs u))
pp ii vs (Rec u v w) = 
  text "R "
    <> pp ii vs u
    <> text " "
    <> parensIf True (pp ii vs v)
    <> text " "
    <> pp ii vs w
pp ii vs (RecL u v w) = 
  text "RL "
    <> pp ii vs u
    <> text " "
    <> parensIf True (pp ii vs v)
    <> text " "
    <> pp ii vs w
pp ii vs (Cons u v) = 
  text "cons "
    <> pp ii vs u 
    <> text " "
    <> parensIf (isNotNil v) (pp ii vs v)

isLam :: Term -> Bool
isLam (Lam _ _) = True
isLam _         = False

isApp :: Term -> Bool
isApp (_ :@: _) = True
isApp _         = False

isNotNil :: Term -> Bool
isNotNil Nil = False
isNotNil _    = True

-- pretty-printer de tipos
printType :: Type -> Doc
printType EmptyT = text "E"
printType (FunT t1 t2) =
  sep [parensIf (isFun t1) (printType t1), text "->", printType t2]
printType NatT = text "Nat"
printType ListT = text "List Nat"

isFun :: Type -> Bool
isFun (FunT _ _) = True
isFun _          = False

fv :: Term -> [String]
fv (Free  (Global n)) = [n]
fv (t   :@: u       ) = fv t ++ fv u
fv (Lam _   u       ) = fv u
fv (Let u v         ) = fv u ++ fv v
fv (Suc u           ) = fv u 
fv (Cons u v        ) = fv u ++ fv v
fv (Rec u v w       ) = fv u ++ fv v ++ fv w 
fv (RecL u v w      ) = fv u ++ fv v ++ fv w 
fv  _                 = [] -- Bound, Zero, Nil
---
printTerm :: Term -> Doc
printTerm t = pp 0 (filter (\v -> not $ elem v (fv t)) vars) t

