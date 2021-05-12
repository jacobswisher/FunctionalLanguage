# FunctionalLanguage

## General Information

"Sudo" is a strongly typed functional language created by Jacob Swisher and Zach Stermon, with help from Dr. Andrew Polonsky.
Sudo is complete environment to compile and run "pseudocode"
Examples of code can be found in the respective ".sudo" file

## How to use

To use Sudo, simply create a program in a ".sudo" file.
Next, GHCI into Parser.hs.
Finally, compile your program into an object file by typing (compile "infile.sudo" "outfile.o") (Use double quotes around the filenames)
If you wish to run your program, type (run "outfile.o") in GHCI
That't it, you may see that your program returned a null value if there is a bug in the code,
otherwise you should see a return value appear.

## How it works

Sudo is a functional language that first Parses an input (.sudo) file into Tokens.
Next, it shift reduces the list of tokens into a list of declarations (think x = 5).
After shift reducing, the declarations are turned into a program, which is an environment and a return value.
Finally the Program is stored in plain text in an object file of your choosing.
Running the object file takes the program and recursively calls the return expression by name until it is fully simplified.
The remaining expression is converted into a return value and printed to the screen.

## Rules for writing "Sudo code"

1.  A (#) indicates a comment line, lines that begin with the pound symbol are not parsed
2.  The program must contain a "main" function and the main function must have a return value
3.  Global variables are declared with a name followed by the colon (:) symbol, their values are typically written on the line beneath the declaration
4.  Global functions are declared much like variables, except that their arguments immediately follow the colon (:) and are seperated with commas (,)
5.  The "main" function cannot accept any arguments
6.  Complex expressions must be enclosed with parenthesis
7.  There are six main types in Sudo; Integers, Floating Point Numbers, Booleans, Strings, Pairs, and Lists
8.  Integers and Floats are used as expected, 8 is an Integer, 8.0 is a Float

## Reserved symbols

"()"  = Parenthesis are used for order of operations and in creation of pairs
"{}"  = Braces are used by the parser to group functions and variables, not to be used by the programmer
"[]"  = Brackets denote a list
","   = Commas are special characters used in the creation of pairs, and to denote a list of arguments
"."   = Periods are reserved for the creation of a floating point number
"#"   = Pound symbol denotes a comment line
":"   = Colon denotes a function or variable declaration
"|"   = Pipe denotes a string

## Reserved keywords

"fst" = Returns the first value in a pair, or first value in a list
"lst" = Returns the second value in a pair, or last value in a list
"if","then","else" = tests a condition, if true returns the "then" value, if false returns the "else" value
"let" = Declares an expression
"add" = Adds two Integers, Floats, Strings, or Lists. (Can also add values to a list)
"sub" = Subtracts two Integers or Floats
"mul" = Multiplies two Integers, Floats, or can repeat a String by an Integer number of times
"div" = Integer division for Integers or Floating division for Floats
"mod" = Modulus of two integers
"true" = Boolean true
"false" = Boolean false
"eql", "neql", "lt", "lte", "gt", "gte" = Comparison functions
"not", "or", "and", "nor", "xor", "xnor", "nand" = Boolean functions
"map" = Applies a lambda to each element in a list (map :: (a -> b) -> [a] -> [b])
"index" = Returns the value of the index given in the list
"main", "return" = Tells the compiler what to return



## Known bugs

1. Multi-Dimensional lists are currently buggy, only the first element in a list can be a list
2. "Let" does not work
