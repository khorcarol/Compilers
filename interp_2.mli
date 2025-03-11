
type address = int 

type var = string 

type value = 
     | INT of int 
     | BOOL of bool
    | CLOSURE of closure
    | VAR of var

and closure = code * env 


and instruction = 
  | UNARY of Ast.unary_oper 
  | OPER of Ast.oper   
  | PUSH of value 
  | POP 
  | SWAP
  | TEST of instruction list * instruction list
  | BIND of Ast.var
  | APPLY
  | LOOKUP of Ast.var
    | MKCLOSURE of code

  

and code = instruction list 

and binding = Ast.var * value

and env = binding list

type env_or_value = EV of env | V of value 

type env_value_stack = env_or_value list

(* array of referenced values together with next unallocated address *) 
type state = (value array) * int 

type interp_state = code * env_value_stack * state 

val step : interp_state -> interp_state 

val compile : Ast.expr -> code 

val driver : int -> interp_state -> value * state

val interpret : Ast.expr -> value * state 

val string_of_value : value -> string 

val string_of_code : code -> string 

