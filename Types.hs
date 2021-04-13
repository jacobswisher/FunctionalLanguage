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
  deriving (Show, Eq, Ord)

newtype TVar = TV String
  deriving (Show, Eq, Ord)

data Type
  = TVar TVar
  | TCon String
  | TArr Type Type
  deriving (Show, Eq, Ord)

data Lit
  = LInt Integer
  | LBool Bool
  deriving (Show, Eq, Ord)

data Binop = Add | Sub | Mul | Eql
  deriving (Eq, Ord, Show)



data Value = VInt Integer | VBool Bool | VClosure Expr Env deriving Show

data Program = Program [Decl] Expr deriving Eq

type Decl = (String, Expr)

data Scheme = Base Type | Forall TVar Scheme

type Name = String

newtype TypeEnv = TypeEnv [(Name, Scheme)]
newtype Env = Env [(Name, Value)] deriving Show



typeInt, typeBool, typeFloat, typeString :: Type
typeInt    = TCon "Int"
typeBool   = TCon "Bool"
typeFloat  = TCon "Float"
typeString = TCon "String"
