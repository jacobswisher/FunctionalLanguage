module Parser where

import Types
import Control.Monad (when)
import Data.Char
import Language

main :: IO()
main = do compile "in.sudo" "out.o"
          run "out.o"

compile :: String -> String -> IO()
compile infile outfile = do
    contents <- readFile infile
    let newContents = show $ parseIt $ setUpFile $ contents
    when (length newContents > 0) $
      writeFile outfile newContents

run :: String -> IO()
run infile = do
    contents <- readFile infile
    putStrLn $ show $ toReturn $ runIt (read contents)

setUpFile :: String -> String
setUpFile contents = unlines $ addBrackets $ combineLines $ removeComments $ lines contents


parseIt :: String -> Program
parseIt contents = toProgram decls []
  where decls = parser $ token $ addSpaces contents

runIt :: Program -> Expr
runIt (Program ds e) = cbn ds e

toReturn :: Expr -> Return
toReturn (Lit lit) = case lit of
                        (LInt i)    -> Integer i
                        (LBool i)   -> Boolean i
                        (LFloat i)  -> Float i
                        (LString i) -> String i
toReturn (Types.Pair e1 e2) = Parser.Pair (toReturn e1) (toReturn e2)
toReturn e = Expression e

data Return = Integer Integer | String String | Float Double | Boolean Bool
          | Pair Return Return | Expression Expr deriving Show

data Token = VSym String | LSym Lit | OpSym Binop
          | LBrace | RBrace | LPar | RPar | Colon | Comma
          | Keyword String | ParsedExpr Expr | Decl Name Type
          | Func Expr | Defn Name Expr | Funcall Name deriving Show



token :: String -> [Token]
token s = map classify (words s)-- decs = map (\(x,y) -> (x, VExpr y)) decls

parser :: [Token] -> [Decl]
parser s  = sr [] s

toProgram :: [Decl] -> [Decl] -> Program
toProgram [] ds                 = Program ds Null
toProgram (("return", e):xs) ds = Program (ds ++ xs) e
toProgram (x:xs) ds             = toProgram xs (x:ds)


sr :: [Token] -> [Token] -> [Decl]
sr ((LSym lit):xs) i                                      = sr ((ParsedExpr (Lit lit)):xs) i
sr ((ParsedExpr e1):(OpSym binop):(ParsedExpr e2):xs) i   = sr (ParsedExpr (Op binop e2 e1):xs) i
sr (RPar:(ParsedExpr e1):Comma:(ParsedExpr e2):LPar:xs) i = sr (ParsedExpr (Types.Pair e2 e1):xs) i
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
sr (ParsedExpr a1:ParsedExpr a2:xs) i                     = sr (ParsedExpr (App a2 a1):xs) i
sr (RPar:ParsedExpr e:LPar:xs) i                          = sr (ParsedExpr e:xs) i
sr s (i:is)                                               = sr (i:s) is
sr (RBrace:[]) []                                         = []
sr [] i                                                   = []
sr s []                                                   = error("parse stopped at " ++ show s)

addUnder :: String -> String
addUnder []       = ""
addUnder ('_':xs) = ' ':(addUnder xs)
addUnder (x:xs)   = x:(addUnder xs)



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
addSpaces (',':xs) = " , " ++ addSpaces xs
addSpaces (')':xs) = " ) " ++ addSpaces xs
addSpaces ('(':xs) = " ( " ++ addSpaces xs
addSpaces ('}':xs) = " } " ++ addSpaces xs
addSpaces ('{':xs) = " { " ++ addSpaces xs
addSpaces (x:xs) = (x:addSpaces xs)

classify :: String -> Token
classify "{"                     = LBrace
classify "}"                     = RBrace
classify "add"                   = OpSym Add
classify "sub"                   = OpSym Sub
classify "mul"                   = OpSym Mul
classify "div"                   = OpSym Div
classify "mod"                   = OpSym Mod
classify "eql"                   = OpSym Eql
classify "("                     = LPar
classify ")"                     = RPar
classify ":"                     = Colon
classify ","                     = Comma
classify "false"                 = LSym $ LBool False
classify "true"                  = LSym $ LBool True
classify s | isInt s             = LSym $ LInt (read s)
classify s | isFloat s           = LSym $ LFloat (read s)
classify ('|':s) | last s == '|' = LSym $ LString $ addUnder $ init s
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
