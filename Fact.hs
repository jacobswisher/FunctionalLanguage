module Fact where

import Types

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

lamTest :: Expr
lamTest = App (App (Lam "x"
                        (Lam "y"
                             (Op Add
                                 (Var "x")
                                 (Var "y")
                             )
                        )
                    )
                        (Lit (LInt 1))
              )
              (Lit (LInt 5))
