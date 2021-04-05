module Testlang where


import Data.List


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

data Lit
  = LInt Integer
  | LBool Bool
  deriving (Show, Eq, Ord)

data Binop = Add | Sub | Mul | Eql
  deriving (Eq, Ord, Show)

data Program = Program [Decl] Expr deriving Eq

type Decl = (String, Expr)

typeInt, typeBool, typeFloat, typeString :: Type
typeInt    = TCon "Int"
typeBool   = TCon "Bool"
typeFloat  = TCon "Float"
typeString = TCon "String"

data Scheme = Forall [TVar] Type

scheme1 :: Scheme
scheme1 = Forall [TV "t"] $ TArr (TArr t t) (TArr t t)
  where t = TVar $ TV "t"

-- mapmap :: Var -> Scheme -> String
-- type EVar = String

type Name = String

--newtype TypeEnv = TypeEnv (Map.Map Var Scheme)
newtype TypeEnv = TypeEnv [(Name, Scheme)]

extend :: TypeEnv -> (Name, Scheme) -> TypeEnv
extend (TypeEnv cxt) decl = TypeEnv (decl:cxt)

fv :: Expr -> [TVar]
fv (Var name)       = [(TV name)]
fv (App e1 e2)      = nub $ (fv e1) ++ (fv e2)
fv (Lit _)          = []
fv (If e1 e2 e3)    = nub $ (fv e1) ++ (fv e2) ++ (fv e3)
fv (Fix expr)       = fv expr
fv (Op binop e1 e2) = nub $ (fv e1) ++ (fv e2)
fv (Lam name expr)  = filter p (fv expr)
  where p (TV name') = name' /= name
fv (Let name e1 e2) = nub $ (fv e1) ++ (filter p (fv e2))
  where p (TV name') = name' /= name

ftv :: Type -> [TVar]
ftv (TVar tvar)  = [(tvar)]
ftv (TCon _)     = []
ftv (TArr t1 t2) = nub $ (ftv t1) ++ (ftv t2)

newtype TVar = TV String
  deriving (Show, Eq, Ord)

data Type
  = TVar TVar
  | TCon String
  | TArr Type Type
  deriving (Show, Eq, Ord)

fresh :: Name -> [Name] -> Name
fresh name names | not $ name `elem` names = name
                 | otherwise               = fresh (name++'\'') names

--TODO read about unification (algorithm)
--TODO all subst's
substExpr :: (Name, Expr) -> Expr -> Expr
substExpr (name, e1) (Var name') | name == name' = e1
                                 | otherwise     = Var name'
substExpr p (App e1 e2) = App (substExpr p e1) (substExpr p e2)
substExpr p@(n1, e1) (Lam n2 e2) | n2 /= n1 && not $ n2 `elem` fv e1 = Lam n2 (substExpr p e2)
                                 | n2 == n1  = Lam n2 e2
                                 | otherwise = Lam n3 (substExpr p (substExpr (n2, Var n3) e2))
                                where n3 = fresh n2 (fv e1)
substExpr _ (Lit lit) = Lit lit
substExpr p@(n1, e1) (If e2 e3 e4) = e2
substExpr p (Fix e) = substExpr p e
substExpr p@(n1, e1) (Op binop e1 e2) = if binop == Add || binop == Sub then substExpr p e1 else substExpr p e2
substExpr p (Let n1 e1 e2)  | e2 /= e1 && not $ e2 `elem` fv e1 = Let n3 e2 (substExpr p e2)
                            | e2 == e1  = Let n1 e1 e2
                            | otherwise = Let n3 (substExpr p (substExpr (n1, Var n3) e2)) e2
                                where n3 = fresh n1 (fv e1)



substType :: (TVar, Type) -> Type -> Type
substType ((TVar t1), tp) (TVar t2) = | t1 == t2  = tp
                                      | otherwise = TVar t2
substType p (TArr t1 t2) = TArr (substType p t1) (substType p t2)
substType _ (TCon name) = TCon name


--the forall quantifier should be treated like lambda
substScheme :: (TVar, Type) -> Scheme -> Scheme
substScheme p1@(TVar name, t1) p2@(Forall vars t2) = Forall vars $ substScheme p' p2
                                where p' = (NEEDS SOMETHING RIGHT HERE)
                                  filter f vars
                                      f (TVar name') = name' /= name


substTypeList :: (TVar, Type) -> [Type] -> [Type]
substTypeList = fmap . substTypeList


substTypeEnv :: (TVar, Type) -> TypeEnv -> TypeEnv
substTypeEnv s (TypeEnv env) = TypeEnv $ map (substTypeEnv s) env
--        uses the map.map thing right here ^











--comment
