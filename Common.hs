module Common where

  -- Comandos interactivos o de archivos
  data Stmt i = Def String i           --  Declarar un nuevo identificador x, let x = t
              | Eval i                 --  Evaluar el término
    deriving (Show)
  
  instance Functor Stmt where
    fmap f (Def s i) = Def s (f i)
    fmap f (Eval i)  = Eval (f i)

  -- Tipos de los nombres
  data Name =  Global  String
    deriving (Show, Eq)

  -- Entornos
  type NameEnv v t = [(Name, (v, t))]

  -- Tipo de los tipos
  data Type = EmptyT 
            | FunT Type Type
            -- Sección 8
            | NatT
            | ListT
            deriving (Show, Eq)
  
  -- Términos con nombres
  data LamTerm  =  LVar String
                |  LAbs String Type LamTerm
                |  LApp LamTerm LamTerm
                 -- Sección 8
                |  LLet String LamTerm LamTerm
                -- naturales 
                |  LZero
                |  LSuc LamTerm
                |  LRec LamTerm LamTerm LamTerm
                -- listas
                |  LNil
                |  LCons LamTerm LamTerm 
                deriving (Show, Eq)


  -- Let x = \y.y in \a.xa 
  -- LLet "x" (LAbs "y" t (LVar "y")) (LAbs "a" t' (LApp (LVar "x") (LVar "a")))
  -- conversion []    t1 --> Lam t (Bound 0)
  -- conversion ["x"] t2 --> Lam t' (conversion' ["a", "x"] (LApp (LVar "x") (LVar "a")))
  -- Let (Lam t (Bound 0)) (Lam t' ((Bound 1) :@: (Bound 0)))
  -- 


  -- Términos localmente sin nombres
  data Term  = Bound Int
             | Free Name 
             | Term :@: Term
             | Lam Type Term
              -- Sección 8
             | Let Term Term
             -- naturales
             | Zero
             | Suc Term
             | Rec Term Term Term
             -- listas
             | Nil
             | Cons Term Term 
          deriving (Show, Eq)

  -- Valores
  data Value = VLam Type Term 
             -- Sección 8
             | VNum NumVal
             | VList ListVal
             
           deriving (Show, Eq)

  -- Valores Numericos
  data NumVal = NZero | NSuc NumVal deriving (Show, Eq)

  -- Listas de números
  data ListVal = VNil | VCons NumVal ListVal deriving (Show, Eq)
   
  -- Contextos del tipado
  type Context = [Type]
