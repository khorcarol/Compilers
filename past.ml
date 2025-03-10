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

type formals = (var * type_expr) list
type oper = ADD | MUL | DIV | SUB
type unary_oper = NEG

type expr =
  | Integer of loc * int
  | Bool of loc * bool
  | Var of loc * var
  | UnaryOp of loc * unary_oper * expr
  | Op of loc * expr * oper * expr
  | Seq of loc * expr list
  | If of loc * expr * expr * expr
  | Lambda of loc * lambda
  | App of loc * expr * expr
  | Let of loc * var * type_expr * expr * expr
  | LetFun of loc * var * lambda * type_expr * expr

and lambda = var * type_expr * expr

let loc_of_expr = function
  | Integer (loc, _) -> loc
  | UnaryOp (loc, _, _) -> loc
  | Op (loc, _, _, _) -> loc
  | Seq (loc, _) -> loc
  | If (loc, _, _, _) -> loc
  | Bool (loc, _) -> loc
  | Lambda (loc, _) -> loc
  | App (loc, _, _) -> loc
  | Let (loc, _, _, _, _) -> loc
  | LetFun (loc, _, _, _, _) -> loc
  | Var (loc, _) -> loc

let string_of_loc loc =
  "line "
  ^ string_of_int loc.Lexing.pos_lnum
  ^ ", " ^ "position "
  ^ string_of_int (loc.Lexing.pos_cnum - loc.Lexing.pos_bol + 1)

open Format

(*
   Documentation of Format can be found here: 
   http://caml.inria.fr/resources/doc/guides/format.en.html
   http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html
*)

let rec pp_type = function
  | TEint -> "int"
  | TEbool -> "bool"
  | TEunit -> "unit"
  | TEref t -> "(" ^ pp_type t ^ " ref)"
  | TEarrow (t1, t2) -> "(" ^ pp_type t1 ^ " -> " ^ pp_type t2 ^ ")"
  | TEproduct (t1, t2) -> "(" ^ pp_type t1 ^ " * " ^ pp_type t2 ^ ")"
  | TEunion (t1, t2) -> "(" ^ pp_type t1 ^ " + " ^ pp_type t2 ^ ")"

let pp_uop = function NEG -> "-"
let pp_bop = function ADD -> "+" | MUL -> "*" | DIV -> "/" | SUB -> "-"
let string_of_oper = pp_bop
let string_of_unary_oper = pp_uop
let fstring ppf s = fprintf ppf "%s" s
let pp_type ppf t = fstring ppf (pp_type t)
let pp_unary ppf op = fstring ppf (pp_uop op)
let pp_binary ppf op = fstring ppf (pp_bop op)

(* ignore locations *)
let rec pp_expr ppf = function
  | Integer (_, n) -> fstring ppf (string_of_int n)
  | UnaryOp (_, op, e) -> fprintf ppf "%a(%a)" pp_unary op pp_expr e
  | Op (_, e1, op, e2) ->
      fprintf ppf "(%a %a %a)" pp_expr e1 pp_binary op pp_expr e2
  | Seq (_, []) -> ()
  | Seq (_, [ e ]) -> pp_expr ppf e
  | Seq (l, e :: rest) -> fprintf ppf "%a; %a" pp_expr e pp_expr (Seq (l, rest))
  | If (_, e1, e2, e3) ->
      fprintf ppf "@[if %a then %a else %a @]" pp_expr e1 pp_expr e2 pp_expr e3
  | Bool (_, b) -> fstring ppf (string_of_bool b)
  | Lambda (_, (v, t, e)) ->
      fprintf ppf "fun (%s : %a) -> %a" v pp_type t pp_expr e
  | App (_, e1, e2) -> fprintf ppf "(%a %a)" pp_expr e1 pp_expr e2
  | Let (_, v, t, e1, e2) ->
      fprintf ppf "let %s : %a = %a in %a" v pp_type t pp_expr e1 pp_expr e2
  | LetFun (_, v, (lv, lt, le), t, e) ->
      fprintf ppf "let fun %s (%s : %a) : %a = %a in %a" v lv pp_type lt pp_type
        t pp_expr le pp_expr e
  | Var (_, v) -> fstring ppf v

let print_expr e =
  let _ = pp_expr std_formatter e in
  print_flush ()

let eprint_expr e =
  let _ = pp_expr err_formatter e in
  print_flush ()

(* useful for degugging *)

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

let rec string_of_type = function
  | TEint -> "TEint"
  | TEbool -> "TEbool"
  | TEunit -> "TEunit"
  | TEref t -> mk_con "TEref" [ string_of_type t ]
  | TEarrow (t1, t2) ->
      mk_con "TEarrow" [ string_of_type t1; string_of_type t2 ]
  | TEproduct (t1, t2) ->
      mk_con "TEproduct" [ string_of_type t1; string_of_type t2 ]
  | TEunion (t1, t2) ->
      mk_con "TEunion" [ string_of_type t1; string_of_type t2 ]

let rec string_of_expr = function
  | Integer (_, n) -> mk_con "Integer" [ string_of_int n ]
  | UnaryOp (_, op, e) ->
      mk_con "UnaryOp" [ string_of_uop op; string_of_expr e ]
  | Op (_, e1, op, e2) ->
      mk_con "Op" [ string_of_expr e1; string_of_bop op; string_of_expr e2 ]
  | Seq (_, el) -> mk_con "Seq" [ string_of_expr_list el ]
  | If (_, e1, e2, e3) ->
      mk_con "If" [ string_of_expr e1; string_of_expr e2; string_of_expr e3 ]
  | Bool (_, b) -> mk_con "Boolean" [ string_of_bool b ]
  | Lambda (_, (v, t, e)) ->
      mk_con "Lambda" [ v; string_of_type t; string_of_expr e ]
  | App (_, e1, e2) -> mk_con "App" [ string_of_expr e1; string_of_expr e2 ]
  | Let (_, v, t, e1, e2) ->
      mk_con "Let" [ v; string_of_type t; string_of_expr e1; string_of_expr e2 ]
  | LetFun (_, v, (lv, lt, le), t, e) ->
      mk_con "LetFun"
        [
          v;
         mk_con "" [lv; string_of_type lt; string_of_expr le]; 
          string_of_type t;
          string_of_expr e;
        ]
  | Var (_, v) -> mk_con "Var" [ v ]

and string_of_expr_list = function
  | [] -> ""
  | [ e ] -> string_of_expr e
  | e :: rest -> string_of_expr e ^ "; " ^ string_of_expr_list rest
