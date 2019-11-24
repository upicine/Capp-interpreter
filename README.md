#### Cappuccino language
Cappuccino language is the imperative C based language. List of language functionalities:
- Types: `int`, `bool`, `string`
- `While`, `if-else`, `for` loop with "read-only" variables, scope statements
- `break` and `continue` statements
- Functions: passing argument by value, return type, typed function's arguments, recursion
- `% + - * / ( )` arithmetic, comparisons
- boolean arithmetic not, and, or: `! && ||`
- Variables assign, arithmetic, comparison, `+=`, `-=`, `++`, `--`
- `print` for `string`, `int`, `bool` type
- Variables shadowing (global/local)
- Runtime handling errors like dividing by `0`.
#### Language gramma

Comment:
```
comment "//" ;
```
Program consists of declarations that ends with ";", it's possible to declare a variable or a function.
```
Program.   Program ::= [Decl] ;
terminator Decl ";" ;
```
Instructions: similar to Latte/C syntax, variable first declaration uses `:=` as assignment operator same syntax occurs in `for` loop.
```
BDeclBlock. Block ::= "{" [Decl] [Stmt] "}" ;
BStmtBlock. Block ::= "{" [Stmt] "}" ;

SBlock.     Stmt  ::= Block ;
separator   Stmt  "" ;
SWhile.     Stmt  ::= "while" "(" Expr ")" Block ;
SFor.       Stmt  ::= "for" "(" Ident ":=" Expr "to" Expr ")" Block;
SAss.       Stmt  ::= Ident "=" Expr ";" ;
SIncr.      Stmt  ::= Ident "++" ";" ;
SDecr.      Stmt  ::= Ident "--" ";" ;
SAssOp.     Stmt  ::= Ident OpAss Expr ";" ;
SIf.        Stmt  ::= "if" "(" Expr ")" Block;
SIfEl.      Stmt  ::= "if" "(" Expr ")" Block "else" Block ;
SReturnE.   Stmt  ::= "return" Expr ";" ;
SCont.      Stmt  ::= "continue" ";" ;
SBreak.     Stmt  ::= "break" ";" ;
SPrint.     Stmt  ::= "print" "(" Expr ")" ";" ;
SExpr.      Stmt  ::= Expr ";" ;

OAssP.      OpAss ::= "+=" ;
OAssM.      OpAss ::= "-=" ;
```
Declarations: similar to many other programming languages like C/Java. 
```
DefVar.     Decl ::= Type [Ident] ;
DefVarExp.  Decl ::= Type Ident ":=" Expr ;
DefFunc.    Decl ::= Type Ident "(" [Arg] ")" Block ;

Arg.        Arg  ::= Type Ident;
separator   Arg  "," ;

separator nonempty Ident "," ;
```
Types:
```
Int.       Type ::= "int" ;
Str.       Type ::= "string" ;
Bool.      Type ::= "bool" ;
```
Expressions: `true` `false` keywords, mentioned earlier operations.
```
EVar.       Expr5 ::= Ident ;
EInt.       Expr5 ::= Integer ;
EStr.       Expr5 ::= String ;
ETrue.      Expr5 ::= "true" ;
EFalse.     Expr5 ::= "false" ;
EApp.       Expr5 ::= Ident "(" [Expr] ")" ;
EUnar.      Expr4 ::= "!" Expr5 ;
EMul.       Expr3 ::= Expr3 OpMul Expr4 ;
EAdd.       Expr2 ::= Expr2 OpAdd Expr3 ;
EComp.      Expr1 ::= Expr1 OpComp Expr2 ;
EAnd.       Expr0 ::= Expr1 "&&" Expr0 ;
EOr.        Expr  ::= Expr0 "||" Expr ;

_.          Expr  ::= Expr0 ;
_.          Expr0 ::= Expr1 ;
_.          Expr1 ::= Expr2 ;
_.          Expr2 ::= Expr3 ;
_.          Expr3 ::= Expr4 ;
_.          Expr4 ::= Expr5 ;
_.          Expr5 ::= "(" Expr ")" ;

separator Expr "," ;

OMod.       OpMul ::= "%" ;
OMul.       OpMul ::= "*" ;
ODiv.       OpMul ::= "/" ;
OPlus.      OpAdd ::= "+" ;
OMinus.     OpAdd ::= "-" ;
OLt.        OpCmp ::= "<" ;
OGt.        OpCmp ::= ">" ;
OLte.       OpCmp ::= "<=" ;
OGte.       OpCmp ::= ">=" ;
OEq.        OpCmp ::= "==" ;
```
#### Examples of programs written in Cappuccino
Hello World! (and string assignment)
```C
int main() {
  string hello := "Hello World!";
  print(hello);
  return 0;
};
```
Expected output:
```
Hello World!
```
---
Print odd numbers function
```C
int print_odd(int range) {
  for (i := 0 to range) {
    if (i % 2 == 1) {
      print(i);
    }
  }
  return 0;
};

int main() {
  int var = 10;
  print_odd(var)
  return 0;
};
```
Expected output:
```
1
3
5
7
9
```
---
Factorial
```C
int factorialr(int n) {
  if (n == 0) {
    return 1;
  } else {
    return n * factorial(n - 1);
  }
};

int main() {
  print(factorialr(5));
  return 0;
};
```
Expected output:
```
120
```
---
Sum numbers using while:
```C
int sum(int n) {
  int s := 0;
  
  while (n > 0) {
    s += n;
    n--;
  }
  
  return s;
};

int main() {
  print(sum(5));
  return 0;
};
```
Expected output:
```
15
```

