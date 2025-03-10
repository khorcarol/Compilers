
type var = string 

type oper = ADD | MUL | DIV | SUB

type unary_oper = NEG

type expr = 
  | Var of var
       | Integer of int
       | UnaryOp of unary_oper * expr
       | Op of expr * oper * expr
       | Seq of (expr list)
       | Bool of bool
       | If of expr * expr * expr
  | Lambda of lambda
  | App of expr * expr
  | Let of var * expr * expr
  | LetFun of var * lambda * expr
  | LetRecFun of var * lambda * expr

and lambda = var * expr

(* printing *) 
val string_of_unary_oper : unary_oper -> string 
val string_of_oper : oper -> string 
val string_of_uop : unary_oper -> string 
val string_of_bop : oper -> string 
val print_expr : expr -> unit 
val eprint_expr : expr -> unit
val string_of_expr : expr -> string 
