module Types

type Token =
    | OPERATOR of string
    | NUMBER of float
    | STRING of string
    | ID of string
    | DOT
    | COMMA
    | SEMICOLON
    | LEFT_CURLY
    | RIGHT_CURLY
    | LEFT_BRACKET
    | RIGHT_BRACKET
    | LEFT_PARENTHESIS
    | RIGHT_PARENTHESIS

type Expr =
    | OPERATOR of string * Expr list
    | NUMBER of double
    | STRING of string
    | ID of string
    | BOOL of bool
    | COND of Expr * Expr * Expr               
    | VARIABLE of string * Expr                    
    | SET of string * Expr                    
    | FUNC_DEF of string * Expr * Expr * env * int 
    | CALL of string * Expr * int
    | PRINT of Expr
    | READ_FILE of string
    | WRITE_FILE of string * string
    | SIMPLE of string
    | SIMPLEOP of string
    | SIMPLELIST of Expr list
    | FOR_LOOP of string * Expr * Expr * Expr
    | WHILE of Expr * Expr
    | HEAL
    | KILL
    | SIMPLEARGLIST of Expr list
    
and env = Map<string, Expr>
