module Language (cbn) where

import Types
import Data.List
import Data.Maybe




cbn :: [Decl] -> Expr -> Expr
cbn ds (Var name) = case lookup name ds of
  Just e           -> cbn ds e
  _                -> Null
cbn ds (App e1 e2) = case cbn ds e1 of
  Null        -> Null
  (Lit _)     -> Null
  (Pair _ _)  -> Null
  (Lam n1 e3) -> cbn ds (substExpr (n1, e2) e3)
  e           -> cbn ds (App e e2)

cbn ds lam@(Lam n1 e1)     = lam
cbn ds (Let n1 e1 e2)      = cbn ds (substExpr (n1, e1) e2)
cbn ds lit@(Lit _)         = lit
cbn ds (If e1 e2 e3)       = case cbn ds e1 of
                                 Lit (LBool True)  -> cbn ds e2
                                 Lit (LBool False) -> cbn ds e3
                                 _                 -> Null
cbn ds (Fix e1)           = cbn ds (App e1 $ Fix e1)
cbn ds (Op b1 e1 e2)      = binop b1 (cbn ds e1) (cbn ds e2)
cbn ds (List (Last e))    = List (Last (cbn ds e))
cbn ds (List (Data e l))  = List (Data (cbn ds e) (unList (cbn ds (List l))))
cbn ds (Pair e1 e2) = Pair (cbn ds e1) (cbn ds e2)
cbn ds (Fst e) = case cbn ds e of
                    Pair e1 e2      -> cbn ds e1
                    List (Last e)   -> cbn ds e
                    List (Data e l) -> cbn ds e
                    _               -> Null
cbn ds (Lst e) = case cbn ds e of
                    Pair e1 e2 -> cbn ds e2
                    List (Last e)   -> cbn ds e
                    List (Data e l) -> cbn ds (Lst (List l))
                    _               -> Null



binop :: Binop -> Expr -> Expr -> Expr
binop Add (Lit (LInt val1))    (Lit (LInt val2))    = Lit . LInt    $ val1 + val2
binop Add (Lit (LFloat val1))  (Lit (LFloat val2))  = Lit . LFloat  $ val1 + val2
binop Add (Lit (LInt val1))    (Lit (LFloat val2))  = Lit . LFloat  $ (fromInteger val1) + val2
binop Add (Lit (LFloat val1))  (Lit (LInt val2))    = Lit . LFloat  $ val1 + (fromInteger val2)
binop Add (Lit (LString val1)) (Lit (LString val2)) = Lit . LString $ val1 ++ val2
binop Sub (Lit (LInt val1))    (Lit (LInt val2))    = Lit . LInt    $ val1 - val2
binop Sub (Lit (LFloat val1))  (Lit (LFloat val2))  = Lit . LFloat  $ val1 - val2
binop Sub (Lit (LInt val1))    (Lit (LFloat val2))  = Lit . LFloat  $ (fromInteger val1) - val2
binop Sub (Lit (LFloat val1))  (Lit (LInt val2))    = Lit . LFloat  $ val1 - (fromInteger val2)
binop Mul (Lit (LInt val1))    (Lit (LInt val2))    = Lit . LInt    $ val1 * val2
binop Mul (Lit (LFloat val1))  (Lit (LFloat val2))  = Lit . LFloat  $ val1 * val2
binop Mul (Lit (LInt val1))    (Lit (LFloat val2))  = Lit . LFloat  $ (fromInteger val1) * val2
binop Mul (Lit (LFloat val1))  (Lit (LInt val2))    = Lit . LFloat  $ val1 * (fromInteger val2)
binop Mul (Lit (LString val1)) (Lit (LInt val2))    = Lit . LString $ concat $ replicate (fromIntegral val2) val1
binop Div (Lit (LFloat val1))  (Lit (LFloat val2))  = Lit . LFloat  $ val1 / val2
binop Div (Lit (LInt val1))    (Lit (LFloat val2))  = Lit . LFloat  $ (fromInteger val1) / val2
binop Div (Lit (LFloat val1))  (Lit (LInt val2))    = Lit . LFloat  $ val1 / (fromInteger val2)
binop Div (Lit (LInt val1))    (Lit (LInt val2))    = Lit . LInt    $ val1 `div` val2
binop Mod (Lit (LInt val1))    (Lit (LInt val2))    = Lit . LInt    $ val1 `mod` val2
binop Eql (Lit (LInt val1))    (Lit (LInt val2))    = Lit . LBool $ val1 == val2
binop Eql (Lit (LFloat val1))  (Lit (LFloat val2))  = Lit . LBool $ val1 == val2
binop Eql (Lit (LInt val1))    (Lit (LFloat val2))  = Lit . LBool $ (fromInteger val1) == val2
binop Eql (Lit (LFloat val1))  (Lit (LInt val2))    = Lit . LBool $ val1 == (fromInteger val2)
binop Eql (Lit (LBool val1))   (Lit (LBool val2))   = Lit . LBool $ val1 == val2
binop Eql (Lit (LString val1)) (Lit (LString val2)) = Lit . LBool $ val1 == val2
binop Eql e1 e2                                     = Lit . LBool $ e1 == e2
binop _ _ _                                         = Null

unList :: Expr -> List
unList (List l) = l

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
fv (Lst e)           = fv e

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
substExpr p (Lst e)      = Lst  $ (substExpr p e)

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


--comment
