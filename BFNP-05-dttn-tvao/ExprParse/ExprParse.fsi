// Interface of module 

module ExprParse
[<Sealed>]

type terminal
exception Scanerror
val scan: char seq -> terminal list
val insertMult: terminal list -> terminal list

type expr
exception Parseerror
val parse: terminal list -> expr
val dotAST: expr -> string
