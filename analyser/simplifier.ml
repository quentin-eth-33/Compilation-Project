(* Codez ici le simplificateur de termes.

    Tout comme pour le langage du cours, l’idée consiste à remplacer les termes constants par le résultat de leur calcul.

    Faites une sous-fonctions récursive pour les expressions et les statements.
    Ces fonction font un pattern matching sur leur argument et traitent chaque cas séparément. Elles renvoient un argument de même type que celui reçu.
    Par exemple : simplify_expression : Ast.expression -> Ast.expression

    Les cas minimaux attendus sont les cas sur les entiers, les flottants, les booléens, ainsi que les if dont le test est constant, et les for qui ne s’exécutent jamais.

    Deux points qui peuvent vous permettre d’aller plus loin :
      - les expressions ne peuvent pas faire d’effet de bord ici, ce qui permet de simplifier des expressions pas nécessairement constantes.
      - Les types composés (point, position et couleur) peuvent également être simplifiés (e.g., (1,2) + (2,x) peut être simplifié en (3,2+x)).

    Vous détaillerez dans le rapport les différents cas que vous simplifiez dans votre simplificateur. //
*)
open Ast

let rec simplify_expr expr =
  let simplified_expr, annotation =
    match expr with
    | Constant_i (i, annotation) -> (Constant_i (i, annotation), annotation)
    | Constant_f (f, annotation) -> (Constant_f (f, annotation), annotation)
    | Constant_b (b, annotation) -> (Constant_b (b, annotation), annotation)
    | Pos (x, y, annotation) ->
        let x_simplified, _ = simplify_expr x in
        let y_simplified, _ = simplify_expr y in
        (Pos (x_simplified, y_simplified, annotation), annotation)
    | Color (r, g, b, annotation) ->
        let r_simplified, _ = simplify_expr r in
        let g_simplified, _ = simplify_expr g in
        let b_simplified, _ = simplify_expr b in
        (Color (r_simplified, g_simplified, b_simplified, annotation), annotation)
    | Point (x, y, annotation) ->
        let x_simplified, _ = simplify_expr x in
        let y_simplified, _ = simplify_expr y in
        (Point (x_simplified, y_simplified, annotation), annotation)
    | Variable (name, annotation) -> (Variable (name, annotation), annotation)
    | Binary_operator (op, e1, e2, annotation) ->
        let e1_simplified, _ = simplify_expr e1 in
        let e2_simplified, _ = simplify_expr e2 in
        (Binary_operator (op, e1_simplified, e2_simplified, annotation), annotation)
    | Unary_operator (op, e, annotation) ->
        let e_simplified, _ = simplify_expr e in
        (Unary_operator (op, e_simplified, annotation), annotation)
    | Field_accessor (accessor, e, annotation) ->
        let e_simplified, _ = simplify_expr e in
        (Field_accessor (accessor, e_simplified, annotation), annotation)
    | List (exprs, annotation) ->
        let simplified_exprs = List.map (fun e -> fst (simplify_expr e)) exprs in
        (List (simplified_exprs, annotation), annotation)
    | Cons (e1, e2, annotation) ->
        let e1_simplified, _ = simplify_expr e1 in
        let e2_simplified, _ = simplify_expr e2 in
        (Cons (e1_simplified, e2_simplified, annotation), annotation)
  in
  (simplified_expr, annotation)

  let rec simplify_stmt stmt =
    let simplified_stmt, annotation =
      match stmt with
      | Assignment (lhs, rhs, annotation) ->
          let lhs_simplified, _ = simplify_expr lhs in
          let rhs_simplified, _ = simplify_expr rhs in
          (Assignment (lhs_simplified, rhs_simplified, annotation), annotation)
      | Variable_declaration (name, typ, annotation) ->
          (Variable_declaration (name, typ, annotation), annotation)
      | Block (stmts, annotation) ->
          let simplified_stmts = List.map (fun s -> fst (simplify_stmt s)) stmts in
          (Block (simplified_stmts, annotation), annotation)
      | IfThenElse (cond, stmt1, stmt2, annotation) ->
          let cond_simplified, _ = simplify_expr cond in
          let stmt1_simplified, _ = simplify_stmt stmt1 in
          let stmt2_simplified, _ = simplify_stmt stmt2 in
          (IfThenElse (cond_simplified, stmt1_simplified, stmt2_simplified, annotation), annotation)
      | For (name, start, stop, step, body, annotation) ->
          let start_simplified, _ = simplify_expr start in
          let stop_simplified, _ = simplify_expr stop in
          let step_simplified, _ = simplify_expr step in
          let body_simplified, _ = simplify_stmt body in
          (For (name, start_simplified, stop_simplified, step_simplified, body_simplified, annotation), annotation)
      | Foreach (name, expr, body, annotation) ->
          let expr_simplified, _ = simplify_expr expr in
          let body_simplified, _ = simplify_stmt body in
          (Foreach (name, expr_simplified, body_simplified, annotation), annotation)
      | Draw (expr, annotation) ->
          let expr_simplified, _ = simplify_expr expr in
          (Draw (expr_simplified, annotation), annotation)
     (*| Nop -> (Nop, Annotation.create Position.undefined)*)
      | Print (expr, annotation) ->
          let expr_simplified, _ = simplify_expr expr in
          (Print (expr_simplified, annotation), annotation)
    in
    (simplified_stmt, annotation)


    let simplifier (Program (args, stmt)) =
      let simplified_stmt, _ = simplify_stmt stmt in
      Program (args, simplified_stmt)
    