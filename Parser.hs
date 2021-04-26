module Parser where

import Types
import Control.Monad (when)
import Data.Char

main = do
    contents <- readFile "in.txt"
    let newContents = parser $ token (addSpaces (removeComments$ lines contents))
    -- let newContents = (addSpaces (removeComments$ lines contents))
    putStrLn $ show (newContents)
    -- when (length newContents > 0) $
    --     writeFile "out.sudo" newContents


data Token = VSym String | LSym Lit | TSym Type | OpSym Binop
          | LBrace | RBrace | LPar | RPar | Colon | Comma | Period
          | Keyword String | ParsedExpr Expr | Decl Name Type | Error
          | Func Expr | Defn Name Expr deriving Show



token :: String -> [Token]
token s = map classify (words s)

parser :: [Token] -> [Decl]
parser s  = sr [] s

sr :: [Token] -> [Token] -> [Decl]
sr ((LSym lit):xs) i                                      = sr ((ParsedExpr (Lit lit)):xs) i
sr (RPar:ParsedExpr e:LPar:xs) i                          = sr (ParsedExpr e:xs) i
sr ((ParsedExpr e1):(OpSym binop):(ParsedExpr e2):xs) i   = sr (ParsedExpr (Op binop e2 e1):xs) i
sr (RPar:(ParsedExpr e1):Comma:(ParsedExpr e2):LPar:xs) i = sr (ParsedExpr (Pair e2 e1):xs) i
sr ((ParsedExpr e1):(Keyword "fst"):xs) i                 = sr (ParsedExpr (Fst e1):xs) i
sr ((ParsedExpr e1):(Keyword "snd"):xs) i                 = sr (ParsedExpr (Snd e1):xs) i
sr ((ParsedExpr e1):(Keyword "else"):(ParsedExpr e2):(Keyword "then"):(ParsedExpr e3):(Keyword "if"):xs) i = sr (ParsedExpr (If e3 e2 e1):xs) i
sr (RBrace:(ParsedExpr e):LBrace:xs) i                    = sr (Func e:xs) i
sr ((Func func):RPar:(ParsedExpr (Var var)):(TSym _):Comma:xs) i = sr (Func (Lam var func):RPar:xs) i
sr ((Func func):RPar:(ParsedExpr (Var var)):(TSym _):LPar: xs) i = sr (Func (Lam var func):RPar:xs) i
sr ((Func func):RPar:LPar:Colon:(VSym fname):xs) i        = sr (Defn fname func:xs) i
sr ((Func func):RPar:Colon:(VSym fname):xs) i             = sr (Defn fname func:xs) i
sr ((ParsedExpr e1):(Keyword "return"):xs) i              = sr (ParsedExpr e1:xs) i
sr ((Defn fname func):xs) i                               = (fname, func):(sr xs i)
sr ((VSym name):xs) (Colon:is)                            = sr (Colon:VSym name:xs) is
sr ((VSym name):xs) i                                     = sr (ParsedExpr (Var name):xs) i
sr ((ParsedExpr arg):(Func fun):xs) i                     = sr (ParsedExpr (App fun arg):xs) i
sr s (i:is)                                               = sr (i:s) is
sr [] i                                                   = []
sr s []                                                   = error("parse stopped at " ++ show s)

addUnder :: [String] -> String
addUnder [] = ""
addUnder (('|':xs):ys) | tail xs == "|" = (('|':(add_ xs) ++ "|") ++ (addUnder ys))
  where add_ [] = []
        add_ (' ':xs) =  ('_':add_ xs)
        add_ (x:xs) = (x:add_ xs)
addUnder (x:xs) = x ++ (addUnder xs)



removeComments :: [String] -> String
removeComments [] = ""
removeComments (('!':'!':_):xs) = removeComments xs
removeComments (x:xs) = x ++ removeComments xs

addSpaces :: String -> String
addSpaces "" = ""
addSpaces (':':xs) = " : " ++ addSpaces xs
addSpaces ('.':xs) = " . " ++ addSpaces xs
addSpaces (',':xs) = " , " ++ addSpaces xs
addSpaces (')':xs) = " ) " ++ addSpaces xs
addSpaces ('(':xs) = " ( " ++ addSpaces xs
addSpaces ('}':xs) = " } " ++ addSpaces xs
addSpaces ('{':xs) = " { " ++ addSpaces xs
addSpaces ('+':xs) = " + " ++ addSpaces xs
addSpaces ('-':xs) = " - " ++ addSpaces xs
addSpaces ('=':xs) = " = " ++ addSpaces xs
addSpaces (x:xs) = (x:addSpaces xs)

classify :: String -> Token
classify "{"                     = LBrace
classify "}"                     = RBrace
classify "int"                   = TSym typeInt
classify "bool"                  = TSym typeBool
classify "float"                 = TSym typeFloat
classify "string"                = TSym typeString
classify "+"                     = OpSym Add
classify "-"                     = OpSym Sub
classify "*"                     = OpSym Mul
classify "=="                    = OpSym Eql
classify "("                     = LPar
classify ")"                     = RPar
classify ":"                     = Colon
classify "."                     = Period
classify ","                     = Comma
classify "false"                 = LSym $ LBool False
classify "true"                  = LSym $ LBool True
classify s | isInt s             = LSym $ LInt (read s)
classify s | isFloat s           = LSym $ LFloat (read s)
classify ('|':[])                = Error
classify ('|':s) | last s == '|' = LSym $ LString $ init s
classify "fst"                   = Keyword "fst"
classify "snd"                   = Keyword "snd"
classify "if"                    = Keyword "if"
classify "then"                  = Keyword "then"
classify "else"                  = Keyword "else"
classify "return"                = Keyword "return"
classify s | isVSym s            = VSym s --Must be last
classify s = error ("No such keyword or operator: " ++ s)


isVSym :: String -> Bool
isVSym "" = False
isVSym (x:xs) = isLower x && q1 xs
  where q1 "" = True
        q1 (y:ys) = (isAlpha y || isDigit y || y `elem` "-_'") && q1 ys

isInt :: String -> Bool
isInt [] = False
isInt (i:is) = isNumber i && isInt' is
            where isInt' [] = True
                  isInt' (i:is) = isNumber i && isInt' is

isFloat :: String -> Bool
isFloat [] = False
isFloat (i:is) = (isNumber i || i == '.') && isFloat' is
            where isFloat' [] = True
                  isFloat' (i:is) = (isNumber i || i == '.') && isFloat' is


var :: [String] -> Expr
var (x:"is":["True"])           = Let x (Lit (LBool True))  (Var x)
var (x:"is":["False"])          = Let x (Lit (LBool False)) (Var x)
var (x:"is":[('|':xs)])         = Let x (Lit (LString (init xs))) (Var x)
var (x:"is":[q]) | '.' `elem` q = Let x (Lit (LFloat (read q))) (Var x)
var (x:"is":[q])                = Let x (Lit (LInt (read q))) (Var x) -- need to test for in
var x                           = error(unwords x)

op :: [String] -> Expr
op (x:"is":q1:"add":[q2]) = Let x (Op Add (Var q1) (Var q2)) (Var x)
op x                      = error(unwords x)











-- op a is 1 add 1
-- a = a+b
--
--
-- "var c is 0" -> ["var", "c", "is", "0"]

-- var c is 0
-- Let "c" (Lit (LInt 0)) (Var "c")
--
-- var b is True
-- Let "b" (LBool Bool) (Var "b")
--
-- var a is b + c
-- Let "a" (Op Add (Var "b") (Var "c")) (Var "a")
--
-- var a is |hello|
