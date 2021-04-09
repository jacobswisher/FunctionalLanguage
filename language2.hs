module Language where


import Data.List

--data types
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


fact5 :: Expr
fact5 = Let "fact5"
            (Fix (Lam "f"
                      (Lam "x"
                           (If  (Op Eql
                                    (Var "x")
                                    (Lit (LInt 0)))
                                (Lit (LInt 1))
                                (Op Mul
                                    (Var "x")
                                    (App (Var "fact5")
                                         (Op Sub
                                             (Var "x")
                                             (Lit (LInt 1)))
                                    )
                                )
                            )
                      )
            ))
            (App
                (Var "fact5")
                (Lit (LInt 5)))


data Value = VInt Integer | VBool Bool | VClosure Expr Env

data Program = Program [Decl] Expr deriving Eq

type Decl = (String, Expr)

typeInt, typeBool, typeFloat, typeString :: Type
typeInt    = TCon "Int"
typeBool   = TCon "Bool"
typeFloat  = TCon "Float"
typeString = TCon "String"

data Scheme = Base Type | Forall TVar Scheme

type Name = String

newtype TypeEnv = TypeEnv [(Name, Scheme)]
newtype Env = Env [(Name, Value)]

eval :: Env -> Expr -> Maybe Value
eval (Env xs) (Var name)   = lookup name xs
eval env (App e1 e2)       = error("")
eval env (Lam n1 e1)       = error("")
eval env (Let n1 e1 e2)    = error("")
eval env (Lit (LInt val))  = Just $ VInt  val
eval env (Lit (LBool val)) = Just $ VBool val
eval env (If e1 e2 e3)     = case eval env e1 of
                               Just (VBool True)  -> eval env e2
                               Just (VBool False) -> eval env e3
                               _                  -> Nothing
eval env (Fix e1)          = eval env (App e1 (Fix e1))
eval env (Op b1 e1 e2)     = do v1 <- eval env e1
                                v2 <- eval env e2
                                binop b1 v1 v2
-- eval env (Op b1 e1 e2)     = case (eval env e1, eval env e2) of
--                               (Just v1, Just v2) -> binop b1 v1 v2
--                               _                  -> Nothing

binop :: Binop -> Value -> Value -> Maybe Value
binop Add (VInt val1) (VInt val2)   = Just . VInt $ val1 + val2
binop Sub (VInt val1) (VInt val2)   = Just . VInt $ val1 - val2
binop Mul (VInt val1) (VInt val2)   = Just . VInt $ val1 * val2
binop Eql (VInt val1) (VInt val2)   = Just . VBool $ val1 == val2
binop Eql (VBool val1) (VBool val2) = Just . VBool $ val1 == val2
binop _ _ _                         = Nothing

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

ftvs :: Scheme -> [TVar]
ftvs (Base t) = ftv t
ftvs (Forall (TV var) scheme) = filter p (ftvs scheme)
                    where p (TV name) = name /= var




fresh :: String -> [String] -> String
fresh name names | not $ name `elem` names = name
                 | otherwise               = fresh ('\'':name) names

freshTV :: TVar -> [TVar] -> TVar
freshTV (TV name) vs = TV $ fresh name (map unTV vs)

unTV :: TVar -> String
unTV (TV name) = name

--TODO all subst's
substExpr :: (Name, Expr) -> Expr -> Expr
substExpr (name, e1) (Var name') | name == name' = e1
                                 | otherwise     = Var name'
substExpr p (App e1 e2) = App (substExpr p e1) (substExpr p e2)
substExpr p@(n1, e1) (Lam n2 e2) | n2 /= n1 && (not $ (TV n2) `elem` (fv e1)) = Lam n2 (substExpr p e2)
                                 | n2 == n1  = Lam n2 e2
                                 | otherwise = Lam n3 (substExpr p (substExpr (n2, Var n3) e2))
                                where n3 = fresh n2 (map unTV (fv e1 ++ fv e2))
substExpr _ (Lit lit) = Lit lit
substExpr p (If e2 e3 e4) = If (substExpr p e2) (substExpr p e3) (substExpr p e4)
substExpr p (Fix e) = Fix $ substExpr p e
substExpr p@(n1, e1) (Op binop e2 e3) = Op binop (substExpr p e2) (substExpr p e3)
substExpr p@(n1,e1) (Let n2 e2 e3)  | n2 /= n1 && (not $ (TV n2) `elem` (fv e1)) = Let n2 (substExpr p e2) (substExpr p e3)
                                    | n2 == n1  = Let n2 (substExpr p e2) e3
                                    | otherwise = Let n3 (substExpr p e2) (substExpr p (substExpr (n2, Var n3) e3))
                                       where n3 = fresh n1 (map unTV (fv e1 ++ fv e2))



substType :: (TVar, Type) -> Type -> Type
substType (t1, tp) (TVar t2) | t1 == t2  = tp
                                  | otherwise = TVar t2
substType p (TArr t1 t2) = TArr (substType p t1) (substType p t2)
substType _ (TCon name) = TCon name


--the forall quantifier should be treated like lambda
substScheme :: (TVar, Type) -> Scheme -> Scheme
substScheme p1@(_, t1) p2@(Base t) = Base $ substType p1 t
substScheme p1@(name, t1) p2@(Forall var scheme) | name /= var && (not $ var `elem` (ftv t1)) = Forall var (substScheme p1 scheme)
                                                 | name == var = Forall var scheme
                                                 | otherwise   = Forall n (substScheme p1 (substScheme (var, TVar n) scheme))
                                                       where n = freshTV name (ftv t1 ++ ftvs scheme)

--data Scheme = Base Type | Forall TVar Scheme

substTypeList :: (TVar, Type) -> [Type] -> [Type]
substTypeList = fmap . substType


substTypeEnv :: (TVar, Type) -> TypeEnv -> TypeEnv
substTypeEnv s (TypeEnv env) = TypeEnv $ map f env
                            where f (x,t) = (x, substScheme s t)











--comment
