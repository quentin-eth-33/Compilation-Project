open Util
open Ast

let type_analyser report program =
  let type_environment = Environment.new_environment () in
  Environment.add type_environment "x" Type_int;
  Error_report.add_warning report
    ("sample_warning", (Lexing.dummy_pos, Lexing.dummy_pos));

    let rec type_expression env expr report =
      match expr with
      | Constant_i (i, annotation) -> Annotation.set_type annotation Type_int
      | Constant_f (f, annotation) -> Annotation.set_type annotation Type_float
      | Constant_b (b, annotation) -> Annotation.set_type annotation Type_bool
      | Pos (e1, e2, annotation) ->
        let _ = type_expression env e1 report in
        let _ = type_expression env e2 report in
        Annotation.set_type annotation Type_pos
      | Point (e1, e2, annotation) ->
        let _ = type_expression env e1 report in
        let _ = type_expression env e2 report in
        Annotation.set_type annotation Type_point
      | Color (e1, e2, e3, annotation) ->
        let _ = type_expression env e1 report in
        let _ = type_expression env e2 report in
        let _ = type_expression env e3 report in
        Annotation.set_type annotation Type_color
      | Variable (name, annotation) ->
        (match Environment.get env name with
        | Some type_expr ->
          let a = type_expr in
          Annotation.set_type annotation a
        | None -> 
          Error_report.add_error report
            (Format.sprintf "Variable %s undeclared" name, Annotation.get_pos annotation);
          let empty_annotation = Annotation.create (Lexing.dummy_pos, Lexing.dummy_pos) in
          empty_annotation)
      | Binary_operator (_, e1, e2, annotation) ->
        let te1 = type_expression env e1 report in
        let te2 = type_expression env e2 report in
        let t1 = Annotation.get_type te1 in
        let t2 = Annotation.get_type te2 
        if t1 <> t2 then begin
          Error_report.add_error report
            (Format.sprintf "Type mismatch: expected %s, but got %s" (Type.to_string t1) (Type.to_string t2), Annotation.get_pos annotation);
          let empty_annotation = Annotation.create (Lexing.dummy_pos, Lexing.dummy_pos) in
          empty_annotation
        end else begin
          Annotation.set_type annotation t1
        end
      | Unary_operator (e, annotation) ->
        let t = type_expression env e report in
        (* Add proper type checking for unary operators *)
        Annotation.set_type annotation Annotation.get_type t; (* Replace Type_int with the correct type based on the operator *)
      | Field_accessor (_, e, annotation) ->
        let _ = type_expression env e report in
        (* Add proper type checking for field accessors *)
        Annotation.set_type annotation Type_int (* Replace Type_int with the correct type based on the accessor *)
      | List (expr_list, annotation) ->
        List.iter (fun e -> ignore (type_expression env e report)) expr_list;
        (* Add proper type checking for lists *)
        Annotation.set_type annotation (Type_list Type_int) (* Replace Type_int with the correct type based on the list elements *)
      | Cons (e1, e2, annotation) ->
        let _ = type_expression env e1 report in
        let _ = type_expression env e2 report in
        (* Add proper type checking for cons *)
        Annotation.set_type annotation (Type_list Type_int) (* Replace Type_int with the correct type based on the cons elements *)
      in 
      let process_expression expr =
        ignore (type_expression type_environment expr report)
      in
    
      List.iter process_expression program;
      report