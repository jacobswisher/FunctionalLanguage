module Language where

import Types
import Data.List
import Data.Maybe
import Fact




cbn :: Env -> Expr -> Maybe Value
cbn (Env xs) (Var name)   = lookup name xs
-- cbn env (App (Lam n1 e1) e2) =  cbn env (substExpr (n1, e2) e1)
cbn env (App e1 e2) = case cbn env e1 of
  Just (VClosure (Lam n1 e3) env2) -> cbn env2 (substExpr (n1, e2) e3)
  _                                -> Nothing
cbn env lam@(Lam n1 e1) = Just $ VClosure lam env
cbn env@(Env xs) (Let n1 e1 e2)  = cbn env (substExpr (n1, e1) e2)
cbn env (Lit (LInt val))    = Just $ VInt    val
cbn env (Lit (LBool val))   = Just $ VBool   val
cbn env (Lit (LString val)) = Just $ VString val
cbn env (Lit (LFloat val))  = Just $ VFloat  val
cbn env (If e1 e2 e3)       = case cbn env e1 of
                                 Just (VBool True)  -> cbn env e2
                                 Just (VBool False) -> cbn env e3
                                 _                  -> Nothing
cbn env (Fix e1)          = cbn env (App e1 (Fix e1))
cbn env (Op b1 e1 e2)     = do v1 <- cbn env e1
                               v2 <- cbn env e2
                               binop b1 v1 v2
cbn env (Pair e1 e2) = do v1 <- cbn env e1
                          v2 <- cbn env e2
                          Just $ VPair v1 v2
cbn env (Fst e) = case cbn env e of
                    Just (VPair e1 e2) -> Just e1
                    _                  -> Nothing
cbn env (Snd e) = case cbn env e of
                    Just (VPair e1 e2) -> Just e2
                    _                  -> Nothing

--unsafe must check for nothing first
unMaybe :: Maybe a -> a
unMaybe (Just x) = x


declToEnv xs = Env $ declToEnv' xs

declToEnv' :: [Decl] -> [(Name, Value)]
declToEnv' [] = []
declToEnv' ((name, Lit (LInt i)):xs) = ((name, VInt i):(declToEnv' xs))
declToEnv' ((name, Lit (LBool i)):xs) = ((name, VBool i):(declToEnv' xs))
declToEnv' ((name, Lit (LString i)):xs) = ((name, VString i):(declToEnv' xs))
declToEnv' ((name, Lit (LFloat i)):xs) = ((name, VFloat i):(declToEnv' xs))
declToEnv' ((name, Lam n e):xs) = ((name, VClosure e (Env [])):(declToEnv' xs))
declToEnv' ((name, Pair i1 i2):xs) = ((name, VPair (exprToValue i1) (exprToValue i2)):(declToEnv' xs))

exprToValue :: Expr -> Value
exprToValue (Lit (LInt i)) = VInt i
exprToValue (Lit (LBool i)) = VBool i
exprToValue (Lit (LString i)) = VString i
exprToValue (Lit (LFloat i)) = VFloat i
exprToValue (Pair e1 e2)  = VPair (exprToValue e1) (exprToValue e2)







binop :: Binop -> Value -> Value -> Maybe Value
binop Add (VInt val1) (VInt val2) = Just . VInt  $ val1 + val2
binop Sub (VInt val1) (VInt val2) = Just . VInt  $ val1 - val2
binop Mul (VInt val1) (VInt val2) = Just . VInt  $ val1 * val2
binop Eql (VInt val1) (VInt val2) = Just . VBool $ val1 == val2
binop Add (VFloat val1) (VFloat val2) = Just . VFloat  $ val1 + val2
binop Sub (VFloat val1) (VFloat val2) = Just . VFloat  $ val1 - val2
binop Mul (VFloat val1) (VFloat val2) = Just . VFloat  $ val1 * val2
binop Eql (VFloat val1) (VFloat val2) = Just . VBool $ val1 == val2
binop Eql (VBool val1) (VBool val2) = Just . VBool $ val1 == val2
binop _ _ _                         = Nothing

extend :: TypeEnv -> (Name, Scheme) -> TypeEnv
extend (TypeEnv cxt) decl = TypeEnv (decl:cxt)

fv :: Expr -> [TVar]
fv (Var name)        = [(TV name)]
fv (App e1 e2)       = nub $ (fv e1) ++ (fv e2)
fv (Lit _)           = []
fv (If e1 e2 e3)     = nub $ (fv e1) ++ (fv e2) ++ (fv e3)
fv (Fix expr)        = fv expr
fv (Op binop e1 e2)  = nub $ (fv e1) ++ (fv e2)
fv (Lam name expr)   = filter p (fv expr)
  where p (TV name') = name' /= name
fv (Let name e1 e2)  = nub $ (fv e1) ++ (filter p (fv e2))
  where p (TV name') = name' /= name
fv (Pair e1 e2)      = nub $ (fv e1) ++ (fv e2)
fv (Fst e)           = fv e
fv (Snd e)           = fv e

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


substExpr :: (Name, Expr) -> Expr -> Expr
substExpr (name, e1) (Var name') | name == name' = e1
                                 | otherwise     = Var name'
substExpr p (App e1 e2) = App (substExpr p e1) (substExpr p e2)
substExpr p@(n1, e1) (Lam n2 e2) | n2 /= n1 && (not $ (TV n2) `elem` (fv e1)) = Lam n2 (substExpr p e2)
                                 | n2 == n1  = Lam n2 e2
                                 | otherwise = Lam n3 (substExpr p (substExpr (n2, Var n3) e2))
                                    where n3 = fresh n2 (map unTV (fv e1 ++ fv e2))
substExpr _ (Lit lit)     = Lit lit
substExpr p (If e2 e3 e4) = If (substExpr p e2) (substExpr p e3) (substExpr p e4)
substExpr p (Fix e)       = Fix $ substExpr p e
substExpr p@(n1, e1) (Op binop e2 e3) = Op binop (substExpr p e2) (substExpr p e3)
substExpr p@(n1,e1) (Let n2 e2 e3)  | n2 /= n1 && (not $ (TV n2) `elem` (fv e1)) = Let n2 (substExpr p e2) (substExpr p e3)
                                    | n2 == n1  = Let n2 (substExpr p e2) e3
                                    | otherwise = Let n3 (substExpr p e2) (substExpr p (substExpr (n2, Var n3) e3))
                                       where n3 = fresh n1 (map unTV (fv e1 ++ fv e2))
substExpr p (Pair e1 e2) = Pair (substExpr p e1) (substExpr p e2)
substExpr p (Fst e)      = Fst  $ (substExpr p e)
substExpr p (Snd e)      = Snd  $ (substExpr p e)

substType :: (TVar, Type) -> Type -> Type
substType (t1, tp) (TVar t2) | t1 == t2  = tp
                             | otherwise = TVar t2
substType p (TArr t1 t2) = TArr (substType p t1) (substType p t2)
substType _ (TCon name)  = TCon name



substScheme :: (TVar, Type) -> Scheme -> Scheme
substScheme p1@(_, t1) p2@(Base t) = Base $ substType p1 t
substScheme p1@(name, t1) p2@(Forall var scheme) | name /= var && (not $ var `elem` (ftv t1)) = Forall var (substScheme p1 scheme)
                                                 | name == var = Forall var scheme
                                                 | otherwise   = Forall n (substScheme p1 (substScheme (var, TVar n) scheme))
                                                       where n = freshTV name (ftv t1 ++ ftvs scheme)


substTypeList :: (TVar, Type) -> [Type] -> [Type]
substTypeList = fmap . substType


substTypeEnv :: (TVar, Type) -> TypeEnv -> TypeEnv
substTypeEnv s (TypeEnv env) = TypeEnv $ map f env
                            where f (x,t) = (x, substScheme s t)



--comment
