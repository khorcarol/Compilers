(* 
   The Parsed AST 
*) 
type var = string 

type loc = Lexing.position 

type type_expr = 
   | TEint 
   | TEbool 
   | TEunit 
   | TEref of type_expr 
   | TEarrow of type_expr * type_expr
   | TEproduct of type_expr * type_expr
   | TEunion of type_expr * type_expr

type oper = ADD | MUL | DIV | SUB

type unary_oper = NEG 


type expr = 
       | Integer of loc * int
       | Bool of loc * bool
       | Var of loc * var
       | UnaryOp of loc * unary_oper * expr
       | Op of loc * expr * oper * expr
	   | Seq of loc * (expr list)
      | If of loc * expr * expr * expr
      | Lambda of loc * lambda
       | App of loc * expr * expr
  | Let of loc * var * type_expr * expr * expr
  | LetFun of loc * var * lambda * type_expr * expr

and  lambda = var * type_expr * expr 
val loc_of_expr : expr -> loc 
val string_of_loc : loc -> string 

(* printing *) 
val string_of_unary_oper : unary_oper -> string 
val string_of_oper : oper -> string 
val string_of_type : type_expr -> string 
val string_of_expr : expr -> string 
val print_expr : expr -> unit 
val eprint_expr : expr -> unit


