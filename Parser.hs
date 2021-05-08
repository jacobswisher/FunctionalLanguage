module Parser where

import Types
import Control.Monad (when)
import Data.Char
import Language

compile :: String -> String -> IO()
compile infile outfile = do
    contents <- readFile infile
    let newContents = parseIt $ setUpFile $ contents
    when (length newContents > 0) $
      writeFile outfile (show newContents)

run :: String -> IO()
run infile = do
    contents <- readFile infile
    putStrLn $ show $ runIt (read contents)

setUpFile :: String -> String
setUpFile contents = unlines $ addBrackets $ combineLines $ removeComments $ lines contents


parseIt :: String -> [Decl]
parseIt contents = parser $ token $ addSpaces contents

runIt :: [Decl] -> Expr
runIt ds = cbn decls e
  where ((_, e):decls) = reverse ds
        -- decs = map (\(x,y) -> (x, VExpr y)) decls

data Token = VSym String | LSym Lit | OpSym Binop
          | LBrace | RBrace | LPar | RPar | Colon | Comma | Period
          | Keyword String | ParsedExpr Expr | Decl Name Type | Error
          | Func Expr | Defn Name Expr | Funcall Name deriving Show



token :: String -> [Token]
token s = map classify (words s)

parser :: [Token] -> [Decl]
parser s  = sr [] s

sr :: [Token] -> [Token] -> [Decl]
sr ((LSym lit):xs) i                                      = sr ((ParsedExpr (Lit lit)):xs) i
sr ((ParsedExpr e1):(OpSym binop):(ParsedExpr e2):xs) i   = sr (ParsedExpr (Op binop e2 e1):xs) i
sr (RPar:(ParsedExpr e1):Comma:(ParsedExpr e2):LPar:xs) i = sr (ParsedExpr (Pair e2 e1):xs) i
sr ((ParsedExpr e1):(Keyword "fst"):xs) i                 = sr (ParsedExpr (Fst e1):xs) i
sr ((ParsedExpr e1):(Keyword "snd"):xs) i                 = sr (ParsedExpr (Snd e1):xs) i
sr ((ParsedExpr e1):(Keyword "else"):(ParsedExpr e2):(Keyword "then"):(ParsedExpr e3):(Keyword "if"):xs) i = sr (ParsedExpr (If e3 e2 e1):xs) i
sr (RBrace:(ParsedExpr e):LBrace:xs) i                    = sr (Func e:xs) i
sr ((Func func):(ParsedExpr (Var var)):Comma:xs) i        = sr (Func (Lam var func):xs) i
sr ((Func func):(ParsedExpr (Var var)): xs) i             = sr (Func (Lam var func):xs) i
sr ((Func func):Colon:(VSym fname):xs) i                  = sr (Defn fname func:xs) i
sr ((ParsedExpr func):(Keyword "return"):xs) i            = ("return", func):(sr xs i)
sr ((Defn fname func):xs) i                               = (fname, func):(sr xs i)
sr ((VSym name):xs) (Colon:is)                            = sr (Colon:VSym name:xs) is
sr ((VSym name):xs) i                                     = sr (ParsedExpr (Var name):xs) i
sr (RBrace:LBrace:Colon:VSym "main":xs) i                 = sr xs i
sr ((ParsedExpr arg):(Funcall fun):xs) i                  = sr (ParsedExpr (App (FCall fun) arg):xs) i
sr (ParsedExpr a1:ParsedExpr a2:xs) i                     = sr (ParsedExpr (App a2 a1):xs) i
sr (RPar:ParsedExpr e:LPar:xs) i                          = sr (ParsedExpr e:xs) i
sr s (i:is)                                               = sr (i:s) is
sr (RBrace:[]) []                                            = []
sr [] i                                                   = []
sr s []                                                   = error("parse stopped at " ++ show s)

addUnder :: [String] -> String
addUnder [] = ""
addUnder (('|':xs):ys) | tail xs == "|" = (('|':(add_ xs) ++ "|") ++ (addUnder ys))
  where add_ [] = []
        add_ (' ':xs) =  ('_':add_ xs)
        add_ (x:xs) = (x:add_ xs)
addUnder (x:xs) = x ++ (addUnder xs)

test ="fact: cur\nif tur\nthen cur*pur\nelse pur-cur\n\nmain:\nreturn (fact 10)\n"


combineLines :: [String] -> [String]
combineLines [] = []
combineLines (x:[]) = [x]
combineLines (x1:x2:xs) | (':' `elem` x2 || ':' `elem` x1) == False = combineLines $ (x1 ++ " " ++ x2):xs
combineLines (x:xs) = x:combineLines xs


addBrackets :: [String] -> [String]
addBrackets []                         = []
addBrackets (x:xs)     | ':' `elem` x  = (x  ++ "{") : addBrackets xs
addBrackets (x1:x2:xs) | ':' `elem` x2 = (x1 ++ "}") : addBrackets (x2:xs)
addBrackets (x1:[])                    = [x1 ++ "}"]
addBrackets (x:xs)                     = x : addBrackets xs

removeComments :: [String] -> [String]
removeComments [] = []
removeComments (('#':_):xs) = removeComments xs
removeComments (x:xs) = x : removeComments xs

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
addSpaces ('*':xs) = " * " ++ addSpaces xs
addSpaces ('-':xs) = " - " ++ addSpaces xs
addSpaces ('=':'=':xs) = " == " ++ addSpaces xs
addSpaces (x:xs) = (x:addSpaces xs)

classify :: String -> Token
classify "{"                     = LBrace
classify "}"                     = RBrace
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
classify ('$':xs) | isVSym xs    = VSym xs
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
