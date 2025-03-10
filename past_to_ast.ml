(* translate_expr : Past.expr -> Ast.expr

   Lifted and amended from the original Slang interpreter
*)



let translate_uop = function Past.NEG -> Ast.NEG

let translate_bop = function
  | Past.ADD -> Ast.ADD
  | Past.MUL -> Ast.MUL
  | Past.DIV -> Ast.DIV
  | Past.SUB -> Ast.SUB

let rec translate_expr = function
  | Past.Integer (_, n) -> Ast.Integer n
  | Past.UnaryOp (_, op, e) -> Ast.UnaryOp (translate_uop op, translate_expr e)
  | Past.Op (_, e1, op, e2) ->
      Ast.Op (translate_expr e1, translate_bop op, translate_expr e2)
  | Past.Seq (_, e1) -> Ast.Seq (List.map translate_expr e1)
  | Past.Bool (_, e) -> Ast.Bool e
  | Past.Var (_, v) -> Ast.Var v
  | Past.Lambda (_, (v, _, e)) -> Ast.Lambda (v, translate_expr e)
  | Past.App (_, e1, e2) -> Ast.App (translate_expr e1, translate_expr e2)
  | Past.Let (_, v, _, e1, e2) ->
      Ast.Let (v,  translate_expr e1, translate_expr e2)
  | Past.LetFun (_, f, (x, _, e) ,_, e2) ->
    (* TODO: if Var f shows up in e, translate to Ast.LetRecFun *)
      Ast.LetFun (f, (x, translate_expr e), translate_expr e2)
  | Past.If (_, e1, e2, e3) ->
      Ast.If (translate_expr e1, translate_expr e2, translate_expr e3)


