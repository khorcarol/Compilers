(**************************************
Compiler Construction 2021 - Supo
mjp217@cam.ac.uk

Very much using the structure of the Slang interpreter provided by Tim Griffin
*****************************************) 

(*  Interpreter 0 for L1 

    This is a "definitional" interpreter for  for language L1 
    using high-level constructs of Ocaml (the defining language). 

	The interpreter is deliberately incomplete - we will use the supervisions to populate...
*) 

open Ast 


let complain = Errors.complain

let verbose = ref false 

type address = int 

type store = address -> value 
and  env = var -> value 

and value = 
     | INT of int 
     | BOOL of bool
     | LAMBDA of var * expr


type binding = var * value

type bindings = binding list

(* auxiliary functions *) 

let rec string_of_value = function 
     | INT n -> string_of_int n 
     | BOOL b -> string_of_bool b
     | LAMBDA (x, e) -> "LAMBDA (" ^ x ^ ", " ^ (string_of_expr e) ^ ")"
    
(* update : (env * binding) -> env 
   update : (store * (address * value)) -> store
*) 
let update(env, (x, v)) = fun y -> if x = y then v else env y

let readint () = let _ = print_string "input> " in read_int() 

let do_unary = function 
  | (NEG,  INT m)  -> INT (-m)
  | (op, _) -> complain ("malformed unary operator: " ^ (string_of_unary_oper op))

let do_oper = function 
  | (ADD,  INT m,   INT n)  -> INT (m + n)
  | (SUB,  INT m,   INT n)  -> INT (m - n)
  | (MUL,  INT m,   INT n)  -> INT (m * n)
  | (DIV,  INT m,   INT n)  -> INT (m / n)
  | (op, _, _)  -> complain ("malformed binary operator: " ^ (string_of_oper op))

(*
    interpret : (expr * env * store) -> (value * store) 
              : (expr * (var -> value) * address -> value) -> value
*) 
let rec interpret ( e, env, store) = 
    match e with 
	| Integer n        -> (INT n, store) 
  | Bool b            -> (BOOL b, store)
  | If(e1, e2,e3)     -> let (v1, store1) = interpret(e1, env,store) in
                        (match v1 with 
                        | BOOL true -> interpret(e2, env, store1)
                        | BOOL false -> interpret(e3, env, store1)
                        )
    | Op(e1, op, e2)   -> let (v1, store1) = interpret(e1, env, store) in 
                          let (v2, store2) = interpret(e2, env, store1) in (do_oper(op, v1, v2), store2) 
    | Seq [e]          -> interpret (e, env, store)
    | Seq (e :: rest)  -> let (_,  store1) = interpret(e, env, store) 
                          in interpret(Seq rest, env, store1) 
    | App (e1, e2)      -> let (v1, store1) = interpret(e1, env, store) in 
                          let (v2, store2) = interpret(e2, env, store1) in 
                          (match v1 with 
                          | LAMBDA (x, e) -> interpret(e, update(env, (x, v2)), store2)
                          | _ -> complain ("Applying a non-function value: " ^ (string_of_value v1))
                          )
    (* Lambdas dont reduce in L2, differs from Slang which e is reduced again *)
    | Lambda (x, e)    -> (LAMBDA (x, e), store)
    | Var x            -> (env x, store)
    | Let (x, e1, e2)  -> let (v1, store1) = interpret(e1, env, store) in 
                          interpret(e2, update(env, (x, v1)), store1)
                         
    | LetFun(f, (x,e1), e2) -> interpret(e2, update(env, (f, LAMBDA(x, e1))), store)
     (* Equivalently, we could've done  App( Lambda (f,e2) , Lambda (x, e1)) *)

(* TODO: Do we really need OCaml built-in functions as in Slang? Could we not do it like this? *)
    



(* env_empty : env *) 
let empty_env = fun x -> complain (x ^ " is not defined!\n")

(* store_empty : env *) 
let empty_store = fun x -> complain ((string_of_int x) ^ " is not allocated!\n")

(* interpret_top_level : expr -> value *) 
let interpret_top_level e = let (v, _) = interpret(e, empty_env, empty_store) in v 
    


      
    
    
