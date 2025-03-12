open Ast
type address = int 

type label = string 

type location = label * (address option) 


type value = 
  | INT of int
  | BOOL of bool  | CLOSURE of location * env | VAR of var

and instruction = 
  | PUSH of value
  | UNARY of unary_oper
  | OPER of oper
  | POP
  | TEST of location
  | CASE of location
  | GOTO of location
  | LABEL of label
  | HALT
  | BIND of var
  | APPLY
  | LOOKUP of var
  | MKCLOSURE of location
  | SWAP
  | RETURN

and code = instruction list 

and binding = Ast.var * value

and env = binding list

type env_or_value = 
  | EV of env           (* an environment on the run-time stack *) 
  | V of value          (* a value on the run-time stack *) 
  | RA of address    (* a return address on the run-time stack *) 

type env_value_stack = env_or_value list 

type state = address * env_value_stack 

val installed : (instruction array) ref

val step : state -> state 

val compile : Ast.expr -> code  

val driver : int -> state -> value 

val interpret : Ast.expr -> value 

val string_of_code : code -> string 

val string_of_value : value -> string 

