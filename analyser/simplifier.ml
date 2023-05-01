open Ast
let rec simplify_expr expr =
  match expr with
  | Constant_i (i, annotation) -> Constant_i (i, annotation)
  | Constant_f (f, annotation) -> Constant_f (f, annotation)
  | Constant_b (b, annotation) -> Constant_b (b, annotation)
  | Pos (x, y, annotation) ->
      let x_simplified = simplify_expr x in
      let y_simplified = simplify_expr y in
      Pos (x_simplified, y_simplified, annotation)
  | Color (r, g, b, annotation) ->
      let r_simplified = simplify_expr r in
      let g_simplified = simplify_expr g in
      let b_simplified = simplify_expr b in
      Color (r_simplified, g_simplified, b_simplified, annotation)
  | Point (x, y, annotation) ->
      let x_simplified = simplify_expr x in
      let y_simplified = simplify_expr y in
      Point (x_simplified, y_simplified, annotation)
  | Variable (name, annotation) -> Variable (name, annotation)
  | Binary_operator (op, e1, e2, annotation) ->
      let e1_simplified = simplify_expr e1 in
      let e2_simplified = simplify_expr e2 in
      Binary_operator (op, e1_simplified, e2_simplified, annotation)
  | Unary_operator (op, e, annotation) ->
      let e_simplified = simplify_expr e in
      Unary_operator (op, e_simplified, annotation)
  | Field_accessor (accessor, e, annotation) ->
      let e_simplified = simplify_expr e in
      Field_accessor (accessor, e_simplified, annotation)
  | List (exprs, annotation) ->
      let simplified_exprs = List.map simplify_expr exprs in
      List (simplified_exprs, annotation)
  | Cons (e1, e2, annotation) ->
      let e1_simplified = simplify_expr e1 in
      let e2_simplified = simplify_expr e2 in
      Cons (e1_simplified, e2_simplified, annotation)

let rec simplify_stmt stmt =
  let simplified_stmt, annotation =
    match stmt with
    | Assignment (lhs, rhs, annotation) ->
        let lhs_simplified = simplify_expr lhs in
        let rhs_simplified = simplify_expr rhs in
        (Assignment (lhs_simplified, rhs_simplified, annotation), annotation)
    | Variable_declaration (name, typ, annotation) ->
        (Variable_declaration (name, typ, annotation), annotation)
    | Block (stmts, annotation) ->
        let simplified_stmts = List.map (fun s -> fst (simplify_stmt s)) stmts in
        (Block (simplified_stmts, annotation), annotation)
    | IfThenElse (cond, stmt1, stmt2, annotation) ->
        let cond_simplified = simplify_expr cond in
        let stmt1_simplified, _ = simplify_stmt stmt1 in
        let stmt2_simplified, _ = simplify_stmt stmt2 in
        (IfThenElse (cond_simplified, stmt1_simplified, stmt2_simplified, annotation), annotation)
    | For (name, start, stop, step, body, annotation) ->
        let start_simplified = simplify_expr start in
        let stop_simplified = simplify_expr stop in
        let step_simplified = simplify_expr step in
        let body_simplified, _ = simplify_stmt body in
        (For (name, start_simplified, stop_simplified, step_simplified, body_simplified, annotation), annotation)
    | Foreach (name, expr, body, annotation) ->
        let expr_simplified = simplify_expr expr in
        let body_simplified, _ = simplify_stmt body in
        (Foreach (name, expr_simplified, body_simplified, annotation), annotation)
    | Draw (expr, annotation) ->
        let expr_simplified = simplify_expr expr in
        (Draw (expr_simplified, annotation), annotation)
    | Print (expr, annotation) ->
        let expr_simplified = simplify_expr expr in
        (Print (expr_simplified, annotation), annotation)


  in
  (simplified_stmt, annotation)

let simplifier (Program (args, stmt)) =
  let simplified_stmt, _ = simplify_stmt stmt in
  Program (args, simplified_stmt)

 (* let simplifier program = program*)
