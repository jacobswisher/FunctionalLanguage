module Parser where

import Types
import Control.Monad (when)

main = do
    contents <- readFile "in.txt"
    let newContents = contents
    when (length newContents > 0) $
        writeFile "out.sudo" newContents


data Token = String | Expr

token :: String -> [Expr]
token x = tokenize $ words x

tokenize :: [String] -> [Expr]
tokenize []         = []
tokenize ("var":xs) = [var xs]
tokenize ("op":xs)  = [op xs]

var :: [String] -> Expr
var (x:"is":["True"])  = Let x (Lit (LBool True))    (Var x)
var (x:"is":["False"]) = Let x (Lit (LBool False))   (Var x)
var (x:"is":[q])       = Let x (Lit (LInt (read q))) (Var x)
var x                  = error(unwords x)

op :: [String] -> Expr
op (x:"is":q1:"add":[q2]) = Let x (Op Add (Var q1) (Var q2)) (Var x)
op x                      = error(unwords x)

op a is 1 add 1
a = a+b


"var c is 0" -> ["var", "c", "is", "0"]

var c is 0
Let "c" (Lit (LInt 0)) (Var "c")

var b is True
Let "b" (LBool Bool) (Var "b")

var a is b + c
Let "a" (Op Add (Var "b") (Var "c")) (Var "a")

var a is "hello"
