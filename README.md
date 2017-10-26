# Interpreter in Functional Programming in Scala

This is an Interpreter for AP++ imperative language using the functional paradigm in scala

## Tasks :
  * Identifies each expression and declaration.
  * Executes the code and should produce an appropriate output.
  * Written using the Functional Paradigm in Scala.
  * Outputs an error, if the program has any syntax errors, identifying the line number and the error.
  * Unit tests

## Rules :
| Rule | Description |
| --- | --- |
| Type ::= τ E {int, bool, alpha}  | Allowed Data types are either int(integer), bool(boolean), or alpha(quoted string)  |
| bool ::= t E {tt,ff}  | Bool refers to the set of tt(true) and ff(false) only  |
| int ::= n E Z  | Int refers to the set of signed Integers(64 bit)  |
| alpha ::= s E  /(["'])\w+\1/  | Alpha refers to the set of quoted words  |
| Id :== {a, ab, ab1,…}  | Identifiers are the variable names, which always start with an alphabet and can contain alphanumeric and the special characters $,_, *, and #  |
| E ::= k\|Id\| E bop E \| uop E  | Each expression can contain a Identifier, a value, binary operations on two expressions or a unary operator on one expression. While there is no explicit restriction in AP++, you may assume the operations are not valid for “alpha”.  |
| k ::= t \| n \| s  | The set of values can either be a bool, an integer, or a string.  |
| bop ::= {+, *, and, or, /, ^, ==, >, <, ><}  | The set of binary operators are +(add), *(product), and(binary AND), or (binary OR), /(difference), ^(binary XOR and powerOf), ==(comparison operator), >(greater than), <(less than), ><(not equal to) |
| uop ::= {-,not}  | The set of unary operator contains – and not with both operators having the same effect, i.e. change the sign for Integers and convert the Bools from true to false and false to true.  |
| D::=nil \| const id: τ=E \| var id:τ=E \| var id:τ=y \| var id:τ  | *Declaration of Identifiers  |
| C::=skip \| id:=E \| C0;C1 \| if E then C0 else C1 \| while E do C  | List of Commands |
| print E  | Prints an expression on a new line  |

