# FunctionalLanguage

## General Information

"Sudo" is a strongly typed functional language created by Jacob Swisher and Zach Stermon, with help from Dr. Andrew Polonsky.
Sudo is complete environment to compile and run "pseudocode"
Examples of code can be found in the respective ".sudo" file

## How to use

To use Sudo, simply create a program in a ".sudo" file.
Next, GHCI into Parser.hs.
Finally, compile your program into an object file by typing "compile [infile.sudo] [outfile.o]" (Use double quotes around the filenames)
If you wish to run your program, type "run [outfile.o]" in GHCI
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
6.  The return values must be enclosed with parenthesis
7.  There are five main types in sudo, Integers, Floating Point Numbers, Booleans, Strings, and Pairs
8.  Integers and Floats are used as expected, 8 is an Integer, 8.0 is a Float
9.  Booleans are declared by their lowercase value (true) or (false)
10. Strings are enclosed with pipe (|) symbols and must not contain spaces. Underscores will be replaced by spaces in the machine code.

## Reserved symbols

"()"  = Parthenthsis are used for order of operations and in creation of pairs
"{}"  = Braces are used by the parser to group functions and variables, not to be used by the programmer
","   = Commas are special characters used in the creation of pairs, and to denote a list of arguments
"."   = Periods are reserved for the creation of a floating point number
"#"   = Pound symbol denotes a comment line
":"   = Colon denotes a function or variable declaration

## Reserved keywords

"fst" = Returns the first value in a pair
"snd" = Returns the second value in a pair
"if","then","else" = tests a condition, if true returns the "then" value, if false returns the "else" value
"let" = Declares an expression (Not yet implemented)
"add" = Adds two Integers, Floats, or Strings
"sub" = Subtracts two Integers or Floats
"div" = Integer division for Integers or Floating division for Floats
"mod" = Modulus of two integers
"eql" = Tests if any two values are equal
"true" = Boolean true
"false" = boolean false
"return" = Tells the compiler what to return
