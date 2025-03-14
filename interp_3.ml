(**************************************
  Compiler Construction 2020
  Computer Laboratory
  University of Cambridge
  Timothy G. Griffin (tgg22@cam.ac.uk)
  *****************************************)
(*
    Interpreter 3.

    Derived from Interpreter 2 by
    --- Make instructions linear by introducing
        labels and jumps.
    --- labels translated to numeric addresses.
    --- include "code pointer" in state
    --- compiler elimnates WHILE construct
*)

open Ast

let complain = Errors.complain

type address = int
type label = string
type location = label * address option
type var = string

type value = INT of int | BOOL of bool | CLOSURE of location * env | VAR of var
and closure = code * env

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
and binding = var * value
and env = binding list

type env_or_value =
  | EV of env (* an environment on the run-time stack *)
  | V of value (* a value on the run-time stack *)
  | RA of address (* a return address on the run-time stack *)

type env_value_stack = env_or_value list
type state = address * env_value_stack

(* update : (env * binding) -> env *)
let update (env, (x, v)) = (x, v) :: env

let string_of_list sep f l =
  let rec aux f = function
    | [] -> ""
    | [ t ] -> f t
    | t :: rest -> f t ^ sep ^ aux f rest
  in
  "[" ^ aux f l ^ "]"

let rec string_of_value = function
  | INT n -> string_of_int n
  | BOOL b -> string_of_bool b
  | VAR x -> x
  | CLOSURE (loc, env) -> "CLOSURE " ^ string_of_closure (loc, env)

and string_of_closure (loc, env) =
  "(" ^ string_of_location loc ^ ", " ^ string_of_env env ^ ")"

and string_of_env env = string_of_list ",\n " string_of_binding env
and string_of_binding (x, v) = "(" ^ x ^ ", " ^ string_of_value v ^ ")"

and string_of_location = function
  | l, None -> l
  | l, Some i -> l ^ " = " ^ string_of_int i

and string_of_instruction = function
  | UNARY op -> "UNARY " ^ string_of_uop op
  | OPER op -> "OPER " ^ string_of_bop op
  | PUSH v -> "PUSH " ^ string_of_value v
  | POP -> "POP"
  | TEST l -> "TEST " ^ string_of_location l
  | CASE l -> "CASE " ^ string_of_location l
  | GOTO l -> "GOTO " ^ string_of_location l
  | HALT -> "HALT"
  | LABEL l -> "LABEL" ^ l
  | BIND x -> "BIND " ^ x
  | APPLY -> "APPLY"
  | LOOKUP x -> "LOOKUP " ^ x
  | MKCLOSURE l -> "MKCLOSURE " ^ string_of_location l
  | SWAP -> "SWAP"
  | RETURN -> "RETURN"

and string_of_code c = string_of_list "\n " string_of_instruction c

let string_of_env_or_value = function
  | EV env -> "EV " ^ string_of_env env
  | V v -> "V " ^ string_of_value v
  | RA i -> "RA " ^ string_of_int i

let string_of_env_value_stack = string_of_list ";\n " string_of_env_or_value

(* THE MACHINE *)

let installed = ref (Array.of_list [ HALT ])

let string_of_installed_code () =
  let size = Array.length !installed in
  let rec aux k =
    if size = k then ""
    else
      string_of_int k ^ ": "
      ^ string_of_instruction !installed.(k)
      ^ "\n"
      ^ aux (k + 1)
  in
  aux 0

let get_instruction cp = Array.get !installed cp
let heap = Array.make Option.heap_max (INT 0)
let next_address = ref 0

let new_address () =
  let a = !next_address in
  next_address := a + 1;
  a

let string_of_heap () =
  let rec aux k =
    if !next_address < k then ""
    else string_of_int k ^ " -> " ^ string_of_value heap.(k) ^ "\n" ^ aux (k + 1)
  in
  "\nHeap = \n" ^ aux 0

let string_of_state (cp, evs) =
  "\nCode Pointer = " ^ string_of_int cp ^ " -> "
  ^ string_of_instruction (get_instruction cp)
  ^ "\nStack = \n"
  ^ string_of_env_value_stack evs
  ^ if !next_address = 0 then "" else string_of_heap ()

let readint () =
  let _ = print_string "input> " in
  read_int ()

let do_unary = function
  | NEG, INT m -> INT (-m)
  | op, _ -> complain ("malformed unary operator: " ^ string_of_unary_oper op)

let do_oper = function
  | ADD, INT m, INT n -> INT (m + n)
  | SUB, INT m, INT n -> INT (m - n)
  | MUL, INT m, INT n -> INT (m * n)
  | DIV, INT m, INT n -> INT (m / n)
  | op, _, _ -> complain ("malformed binary operator: " ^ string_of_oper op)

let rec lookup_opt = function
  | [], _ -> None
  | (y, v) :: _, x when x = y -> Some v
  | (_, _) :: rest, x -> lookup_opt (rest, x)

let rec search (evs, x) =
  match evs with
  | [] -> complain " not defined!\n"
  | V _ :: rest -> search (rest, x)
  | EV env :: rest -> (
      match lookup_opt (env, x) with None -> search (rest, x) | Some v -> v)

let rec evs_to_env = function
  | [] -> []
  | V _ :: rest -> evs_to_env rest
  | EV env :: rest -> env @ evs_to_env rest

let step (cp, evs) =
  match (get_instruction cp, evs) with
  | PUSH v, evs -> (cp + 1, V v :: evs)
  | POP, s :: evs -> (cp + 1, evs)
  | UNARY op, V v :: evs -> (cp + 1, V (do_unary (op, v)) :: evs)
  | OPER op, V v2 :: V v1 :: evs -> (cp + 1, V (do_oper (op, v1, v2)) :: evs)
  (* new intructions *)
  | LABEL _, evs -> (cp + 1, evs)
  | HALT, evs -> (cp, evs)
  | GOTO (_, Some i), evs -> (i, evs)
  | TEST (_, Some i), V (BOOL true) :: evs -> (cp + 1, evs)
  | TEST (_, Some i), V (BOOL false) :: evs -> (i, evs)
  | BIND x, V v :: evs -> (cp + 1, EV [ (x, v) ] :: evs)
  | LOOKUP x, evs -> (cp + 1, V (search (evs, x)) :: evs)
  | POP, _ :: evs -> (cp + 1, evs)
  | SWAP, x :: y :: evs -> (cp + 1, y :: x :: evs)
  | MKCLOSURE l, evs -> (cp + 1, V (CLOSURE (l, evs_to_env evs)) :: evs)
  | APPLY, V (CLOSURE ((_, Some i), env)) :: V v :: evs ->
      (i, V v :: EV env :: RA (cp+1) :: evs)
  | RETURN, RA cp :: evs -> (cp, evs)
  | _ -> complain ("step : bad state = " ^ string_of_state (cp, evs) ^ "\n")

(* COMPILE *)

let new_label =
  let i = ref 0 in
  let get () =
    let v = !i in
    i := !i + 1;
    "L" ^ string_of_int v
  in
  get

let rec comp = function
  | Integer n -> ([], [ PUSH (INT n) ])
  | Bool b -> ([], [ PUSH (BOOL b) ])
  | UnaryOp (op, e) ->
      let defs, c = comp e in
      (defs, c @ [ UNARY op ])
  | Op (e1, op, e2) ->
      let defs1, c1 = comp e1 in
      let defs2, c2 = comp e2 in
      (defs1 @ defs2, c1 @ c2 @ [ OPER op ])
  | Seq [] -> ([], [])
  | Seq [ e ] -> comp e
  | Seq (e :: rest) ->
      let defs1, c1 = comp e in
      let defs2, c2 = comp (Seq rest) in
      (defs1 @ defs2, c1 @ [ POP ] @ c2)
  | If (e1, e2, e3) ->
      let defs1, c1 = comp e1 in
      let defs2, c2 = comp e2 in
      let defs3, c3 = comp e3 in
      let else_label = new_label () in
      let endif_label = new_label () in

      ( defs1 @ defs2 @ defs3,
        c1
        @ [ TEST (else_label, None) ]
        @ c2
        @ [ GOTO (endif_label, None); LABEL else_label ]
        @ c3 @ [ LABEL endif_label ] )
  | Var x -> ([], [ LOOKUP x ])
  | Let (x, e1, e2) ->
      let defs1, c1 = comp e1 in
      let defs2, c2 = comp e2 in
      (defs1 @ defs2, c1 @ [ BIND x ] @ c2 @ [SWAP; POP])
  | Lambda (x, e) ->
      let f = new_label () in
      let defs, c = comp e in
      ([ LABEL f; BIND x ] @ c @ [ SWAP; POP; SWAP; POP;  SWAP; RETURN ] @ defs, [ MKCLOSURE (f, None) ])
      (* return value; environment; return address *)
  | App (e1, e2) ->
      let defs1, c1 = comp e1 in
      let defs2, c2 = comp e2 in
      (defs2 @ defs1, c2 @ c1 @ [ APPLY ])
  | LetFun (f, (x,e1), e2) -> 
    let lx = new_label () in
    let defs1, c1 = comp e1 in
    let defs2, c2 = comp e2 in

  ([LABEL lx; BIND x] @ c1 @[SWAP; POP; SWAP; POP;  SWAP; RETURN] @ defs2 @ defs1, [MKCLOSURE (lx, None); BIND f] @ c2 @ [SWAP; POP])


  

let compile e =
  let defs, c = comp e in
  let result =
    c (* body of program *) @ [ HALT ] (* stop the interpreter *) @ defs
  in
  (* the function definitions *)
  let _ =
    if Option.verbose then
      print_string ("\nCompiled Code = \n" ^ string_of_code result)
    else ()
  in
  result

let rec driver n state =
  let _ =
    if Option.verbose then
      print_string
        ("\nstate " ^ string_of_int n ^ ":" ^ string_of_state state ^ "\n")
    else ()
  in
  match state with
  | cp, evs ->
      if HALT = get_instruction cp then
        match evs with
        | [ V v ] -> v
        | _ ->
            complain
              ("driver : bad halted state = " ^ string_of_state state ^ "\n")
      else driver (n + 1) (step state)

(* put code listing into an array, associate an array index to each label *)
let load l =
  let rec find lab = function
    | [] -> complain ("find : " ^ lab ^ " is not found")
    | (x, v) :: rest -> if x = lab then v else find lab rest
    (* insert array index for each label *)
  in
  let apply_label_map_to_instruction m = function
    | GOTO (lab, _) -> GOTO (lab, Some (find lab m))
    | TEST (lab, _) -> TEST (lab, Some (find lab m))
    | CASE (lab, _) -> CASE (lab, Some (find lab m))
  | MKCLOSURE (lab, _)  -> MKCLOSURE (lab, Some (find lab m)) 
    | inst -> inst
    (* find array index for each label *)
  in
  let listing_to_label_map l =
    let rec aux carry k = function
      | [] -> carry
      | LABEL lab :: rest -> aux ((lab, k) :: carry) (k + 1) rest
      | _ :: rest -> aux carry (k + 1) rest
    in
    aux [] 0 l
  in
  let l_map = listing_to_label_map l in
  Array.of_list (List.map (apply_label_map_to_instruction l_map) l)

(* interpret : expr -> value *)
let interpret e =
  let c = compile e in
  let _ = installed := load c in
  let _ =
    if Option.verbose then
      print_string ("\nInstalled Code = \n" ^ string_of_installed_code ())
    else ()
    (* set the code pointer to 0 *)
  in
  driver 1 (0, [])
