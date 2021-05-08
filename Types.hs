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
  | FCall Name
  deriving (Eq, Ord, Read, Show)

-- instance Show Expr where
--   show Null = "Null"
--   show (Lit x) = show x
--   show x =

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

data Binop = Add | Sub | Mul | Eql
  deriving (Eq, Ord, Show, Read)

-- instance Show Lit where
--   show (LInt i)    = show i
--   show (LBool i)   = show i
--   show (LString i) = show i
--   show (LFloat i)  = show i

-- data Value = VInt Integer | VBool Bool | VFloat Double | VString String
--   | VClosure Expr Env | VPair Value Value | VExpr Expr deriving Show

data Program = Program [Decl] Expr deriving Eq

type Decl = (Name, Expr)

data Scheme = Base Type | Forall TVar Scheme

type Name = String

-- newtype TypeEnv = TypeEnv [(Name, Scheme)]
-- newtype Env = Env [(Name, Value)] deriving Show




typeInt, typeBool, typeFloat, typeString :: Type
typeInt    = TCon "Int"
typeBool   = TCon "Bool"
typeFloat  = TCon "Float"
typeString = TCon "String"
