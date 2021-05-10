module Types where

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Expr
  | Let Name Expr Expr
  | Lit Lit
  | If Expr Expr Expr
  | Fix Expr
  | Op Binop Expr Expr
  | Pair Expr Expr
  | Fst Expr
  | Snd Expr
  | Null
  deriving (Eq, Ord, Read, Show)


newtype TVar = TV String
  deriving (Show, Eq, Ord, Read)

data Type
  = TVar  TVar
  | TCon  String
  | TArr  Type Type
  | TProd Type Type
  deriving (Show, Eq, Ord, Read)

data Lit
  = LInt    Integer
  | LBool   Bool
  | LString String
  | LFloat  Double
  deriving (Show, Eq, Ord, Read)

data Binop = Add | Sub | Mul | Div | Mod | Eql
  deriving (Eq, Ord, Show, Read)


data Program = Program [Decl] Expr deriving (Eq, Show, Read)

type Decl = (Name, Expr)

data Scheme = Base Type | Forall TVar Scheme

type Name = String





typeInt, typeBool, typeFloat, typeString :: Type
typeInt    = TCon "Int"
typeBool   = TCon "Bool"
typeFloat  = TCon "Float"
typeString = TCon "String"
