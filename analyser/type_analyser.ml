open Util
open Ast
let type_analyser report program =
  let type_environment = Environment.new_environment () in
  Environment.add type_environment "x" Type_int;
  Error_report.add_warning report
    ("sample_warning", (Lexing.dummy_pos, Lexing.dummy_pos));

  let rec type_expression env expr =
    match expr with
      | Constant_i (i, annotation) -> Annotation.set_type annotation Type_int;(* Constant_i(i, set_type annotation Type_int)*)
      | Constant_f (f, annotation) -> Annotation.set_type annotation Type_float;
      | Constant_b (b, annotation) -> Annotation.set_type annotation Type_bool;
      | Pos (e1, e2, annotation) ->
        let te1 = type_expression env e1 in
    let te2 = type_expression env e2 in
    Annotation.set_type annotation Type_pos;
        
      | Point (e1, e2, _) ->
        type_expression env e1;
        type_expression env e2;
        Type_point
      | Color (e1, e2, e3, _) ->
        type_expression env e1;
        type_expression env e2;
        type_expression env e3;
        Type_color
      | Variable (name, _) ->
        Environment.find_variable_type env name
      | Field_accessor (fa, e, _) ->
        type_expression env e;
        (match fa with
         | Color_accessor -> Type_color
         | Position_accessor -> Type_pos
         | X_accessor | Y_accessor -> Type_int
         | Blue_accessor | Red_accessor | Green_accessor -> Type_int)
      | Binary_operator (op, e1, e2, _) ->
        type_expression env e1;
        type_expression env e2;
        (match op with
         | Add | Sub | Mul | Div | Mod -> Type_int
         | And | Or | Lt | Gt | Le | Ge | Ne | Eq -> Type_bool)
      | Unary_operator (op, e, _) ->
        type_expression env e;
        (match op with
         | USub | Floor | Float_of_int | Cos | Sin -> Type_int
         | Not | Head | Tail -> Type_bool)
      | List (elems, _) ->
        let rec list_type = function
          | [] -> Type_list Type_null
          | h :: t ->
            let ht = type_expression env h in
            let tt = list_type t in
            if ht = tt then ht else raise (TypeError "Type mismatch in list")
        in list_type elems
      | Cons (e1, e2, _) ->
        type_expression env e1;
        type_expression env e2;
        Type_list (type_expression env e1)
    in
    ann.type_expr <- Some te

  and type_statement env stmt =
    match stmt with
    | Variable_declaration (name, typ, _) ->
      Environment.add_variable env name typ
    | Assignment (expr1, expr2, _) ->
      type_expression env expr1;
      type_expression env expr2
    | Block (stmts, _) ->
      let new_env = Environment.add_layer env in
      List.iter (type_statement new_env) stmts;
      Environment.remove_layer new_env
    | IfThenElse (test, i1, i2, _) ->
      type_expression env test;
      type_statement env i1;
      type_statement env i2
    | For (id, start_expr, end_expr, step_expr, stmt, _) ->
      type_expression env start_expr;
      type_expression env end_expr;
      type_expression env step_expr;
      type_statement (Environment.add_variable env id Type_int) stmt
      | Foreach (id, list_expr, stmt, _) ->
        let list_type = type_expression env list_expr in
        (match list_type with
         | Type_list elem_type -> type_statement (Environment.add_variable env id elem_type) stmt
         | _ -> raise (TypeError "Foreach expects a list"))
      | Draw (expr, _) ->
        let expr_type = type_expression env expr in
        (match expr_type with
         | Type_pos | Type_point -> ()
         | _ -> raise (TypeError "Draw expects a pos or point"))
      | Print (expr, _) ->
        type_expression env expr;
        ()
      | Nop -> ()
  
    match program with
    | Program (args, stmt) ->
      let env = Environment.add_layer type_environment in
      List.iter (fun (Argument (name, t, _)) ->
        let env = Environment.add_variable env name t in ()) args;
      type_statement env stmt
  