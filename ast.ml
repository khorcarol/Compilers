type var = string
type oper = ADD | MUL | DIV | SUB
type unary_oper = NEG

type expr =
  | Var of var
  | Integer of int
  | UnaryOp of unary_oper * expr
  | Op of expr * oper * expr
  | Seq of expr list
  | Bool of bool
  | If of expr * expr * expr
  | Lambda of lambda
  | App of expr * expr
  | Let of var * expr * expr
  | LetFun of var * lambda * expr
  | LetRecFun of var * lambda * expr



and lambda = var * expr

open Format

(*
   Documentation of Format can be found here: 
   http://caml.inria.fr/resources/doc/guides/format.en.html
   http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html
*)

let pp_uop = function NEG -> "-"
let pp_bop = function ADD -> "+" | MUL -> "*" | DIV -> "/" | SUB -> "-"
let string_of_oper = pp_bop
let string_of_unary_oper = pp_uop
let fstring ppf s = fprintf ppf "%s" s
let pp_unary ppf t = fstring ppf (pp_uop t)
let pp_binary ppf t = fstring ppf (pp_bop t)

let rec pp_expr ppf = function
  | Integer n -> fstring ppf (string_of_int n)
  | UnaryOp (op, e) -> fprintf ppf "%a(%a)" pp_unary op pp_expr e
  | Op (e1, op, e2) ->
      fprintf ppf "(%a %a %a)" pp_expr e1 pp_binary op pp_expr e2
  | Seq el -> fprintf ppf "begin %a end" pp_expr_list el
  | If (e1, e2, e3) ->
      fprintf ppf "@[if %a then %a else %a @]" pp_expr e1 pp_expr e2 pp_expr e3
  | Bool b -> fstring ppf (string_of_bool b)
  | Lambda (v, e) -> fprintf ppf "fun %s -> %a" v pp_expr e
  | App (e1, e2) -> fprintf ppf "(%a %a)" pp_expr e1 pp_expr e2
  | Let (v, e1, e2) -> fprintf ppf "let %s = %a in %a" v pp_expr e1 pp_expr e2
  | LetFun (v, (lv, le), e) ->
      fprintf ppf "let fun %s %s = %a in %a" v lv pp_expr le pp_expr e
  | LetRecFun (v, (lv, le), e) ->
      fprintf ppf "let rec fun %s %s = %a in %a" v lv pp_expr le pp_expr e
  | Var v -> fstring ppf v

and pp_expr_list ppf = function
  | [] -> ()
  | [ e ] -> pp_expr ppf e
  | e :: rest -> fprintf ppf "%a; %a" pp_expr e pp_expr_list rest

let print_expr e =
  let _ = pp_expr std_formatter e in
  print_flush ()

let eprint_expr e =
  let _ = pp_expr err_formatter e in
  pp_print_flush err_formatter ()

(* useful for debugging *)

let string_of_uop = function NEG -> "NEG"

let string_of_bop = function
  | ADD -> "ADD"
  | MUL -> "MUL"
  | DIV -> "DIV"
  | SUB -> "SUB"

let mk_con con l =
  let rec aux carry = function
    | [] -> carry ^ ")"
    | [ s ] -> carry ^ s ^ ")"
    | s :: rest -> aux (carry ^ s ^ ", ") rest
  in
  aux (con ^ "(") l

let rec string_of_expr = function
  | Bool b -> mk_con "Boolean" [ string_of_bool b ]
  | Integer n -> mk_con "Integer" [ string_of_int n ]
  | UnaryOp (op, e) -> mk_con "UnaryOp" [ string_of_uop op; string_of_expr e ]
  | Op (e1, op, e2) ->
      mk_con "Op" [ string_of_expr e1; string_of_bop op; string_of_expr e2 ]
  | Seq el -> mk_con "Seq" [ string_of_expr_list el ]
  | If (e1, e2, e3) ->
      mk_con "If" [ string_of_expr e1; string_of_expr e2; string_of_expr e3 ]
  | Lambda (v, e) -> mk_con "Lambda" [ v; string_of_expr e ]
  | App (e1, e2) -> mk_con "App" [ string_of_expr e1; string_of_expr e2 ]
  | Let (v, e1, e2) -> mk_con "Let" [ v; string_of_expr e1; string_of_expr e2 ]
  | LetFun (v, (lv, le), e) ->
      mk_con "LetFun" [ v; mk_con "" [lv; string_of_expr le]; string_of_expr e ]
  | LetRecFun (v, (lv, le), e) ->
      mk_con "LetRecFun" [ v; lv; string_of_expr le; string_of_expr e ]
  | Var v -> mk_con "Var" [ v ]

and string_of_expr_list = function
  | [] -> ""
  | [ e ] -> string_of_expr e
  | e :: rest -> string_of_expr e ^ "; " ^ string_of_expr_list rest
