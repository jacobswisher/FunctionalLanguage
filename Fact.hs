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
                                    (App (Var "f")
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

fact5' = App (Fix (Lam "f"
          (Lam "x"
               (If  (Op Eql
                        (Var "x")
                        (Lit (LInt 0)))
                    (Lit (LInt 1))
                    (Op Mul
                        (Var "x")
                        (App (Var "f")
                             (Op Sub
                                 (Var "x")
                                 (Lit (LInt 1)))
                        )
                    )
                )
          )))
          (Lit (LInt 5))

letTest = Let "x" (Lit (LInt 5)) (Op Mul (Var "x") (Var "x"))

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


-- Just (VClosure (Lam "x"
--                     (If (Op Eql (Var "x")(Lit (LInt 0)))
--                         (Lit (LInt 1))
--                         (Op Mul (Var "x") (App (App (Var "fact5")
--                                                (Lit (LInt 5)))
--                                                (Op Sub (Var "x") (Lit (LInt 1)))
--
--
--
--                                                )))) (Env []))
--
