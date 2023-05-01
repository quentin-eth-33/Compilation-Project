open Util
open Ast
(*
let type_analyser report program =
  let type_environment = Environment.new_environment () in
 
    let _ = 
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
      | Binary_operator (op, e1, e2, annotation) ->
        let te1 = type_expression env e1 report in
        let te2 = type_expression env e2 report in
        let t1 = Annotation.get_type te1 in
        let t2 = Annotation.get_type te2 in
        if t1 <> t2 then 
          begin
            Error_report.add_error report
            (Format.sprintf "Type mismatch: expected %s, but got %s" (Ast.string_of_type_expr (Option.get t1)) (Ast.string_of_type_expr (Option.get t2)), Annotation.get_pos annotation);
            let empty_annotation = Annotation.create (Lexing.dummy_pos, Lexing.dummy_pos) in
            empty_annotation
          end
        else begin
          Annotation.set_type annotation (Option.get t1)
        end
       
      | Unary_operator (_, e, annotation) ->
        let t = type_expression env e report in
        let t1 = Annotation.get_type t in
        Annotation.set_type annotation (Option.get t1);
      | Field_accessor (accessor, e, annotation) ->
        let a = type_expression env e report in
        let t1 = Annotation.get_type a in
        Annotation.set_type annotation (Option.get t1); 
        | List (expr_list, annotation) ->
          begin
            match expr_list with
            | [] ->
                Error_report.add_error report
                  ("Empty list encountered. Please provide list type information.", Annotation.get_pos annotation);
                let empty_annotation = Annotation.create (Lexing.dummy_pos, Lexing.dummy_pos) in
                empty_annotation
            | first_expr :: rest ->
                let first_type = type_expression env first_expr report in
                let same_type = List.for_all (fun e ->
                  let current_type = type_expression env e report in
                  current_type = first_type
                ) rest in
                if same_type then
                  let t1 = Annotation.get_type first_type in
                  Annotation.set_type annotation (Type_list (Option.get t1))
                else begin
                  Error_report.add_error report
                    ("Type mismatch: all elements in the list should have the same type.", Annotation.get_pos annotation);
                  let empty_annotation = Annotation.create (Lexing.dummy_pos, Lexing.dummy_pos) in
                  empty_annotation
                end
          end
        | Cons (e1, e2, annotation) ->
            let te1 = type_expression env e1 report in
            let te2 = type_expression env e2 report in
            let t1 = Annotation.get_type te1 in
            let t2 = Annotation.get_type te2 in
            begin
              match t2 with
              | Some (Type_list elem_type) ->
                  if elem_type <> Option.get t1 then begin
                    Error_report.add_error report
                      (Format.sprintf "Type mismatch: expected list of %s, but got list of %s" (Ast.string_of_type_expr (Option.get t1)) (Ast.string_of_type_expr elem_type), Annotation.get_pos annotation);
                    let empty_annotation = Annotation.create (Lexing.dummy_pos, Lexing.dummy_pos) in
                    empty_annotation
                  end else
                    Annotation.set_type annotation ( Option.get t1)
              | _ ->
                  Error_report.add_error report
                    (Format.sprintf "Type mismatch: expected list, but got %s" (Ast.string_of_type_expr (Option.get t2)), Annotation.get_pos annotation);
                  let empty_annotation = Annotation.create (Lexing.dummy_pos, Lexing.dummy_pos) in
                  empty_annotation
            end
        

      in 
      let rec type_statement env statement report =
        match statement with
      | Assignment(e1, e2 ,annotation) -> 
        let te1 = type_expression env e1 report in 
        let te2 = type_expression env e2 report in 
        (match e1 with
        | Variable (name, _) ->
          (match Environment.get env name with
           | Some type_expr ->
             (match Annotation.get_type te2 with
              | Some te2_type ->
                if type_expr = te2_type then
                  Annotation.set_type annotation type_expr
                else
                  begin 
                    Error_report.add_error report
                      (Format.sprintf "Type mismatch: expected %s but got %s" (Ast.string_of_type_expr type_expr) (Ast.string_of_type_expr te2_type), Annotation.get_pos annotation);
                    let empty_annotation = Annotation.create (Lexing.dummy_pos, Lexing.dummy_pos) in
                    empty_annotation
                  end 
              | None -> let empty_annotation = Annotation.create (Lexing.dummy_pos, Lexing.dummy_pos) in
              empty_annotation)
              | None -> 
                Error_report.add_error report (Format.sprintf "Variable %s undeclared" name, Annotation.get_pos annotation);
                let empty_annotation = Annotation.create (Lexing.dummy_pos, Lexing.dummy_pos) in
                empty_annotation)
        | _ -> (
          Error_report.add_error report
            (Format.sprintf "Left side of assignment must be a variable", Annotation.get_pos annotation); 
            let empty_annotation = Annotation.create (Lexing.dummy_pos, Lexing.dummy_pos) in
                empty_annotation))
    
        | Variable_declaration (name, type_expr, annotation) ->
          if Environment.is_def_in_current_layer env name then
            Error_report.add_error report
              (Format.sprintf "Variable %s already declared in the current scope" name, Annotation.get_pos annotation);
            let new_annotation = Annotation.create (Lexing.dummy_pos, Lexing.dummy_pos) in
            new_annotation
    
        | Block (stmt_list, annotation)  -> 
          Environment.add_layer env;
          ignore (List.map (fun stmt -> type_statement env stmt report) stmt_list);
          Environment.remove_layer env;
          let new_annotation = Annotation.create (Lexing.dummy_pos, Lexing.dummy_pos) in
            new_annotation
            | IfThenElse  (test, i_then, i_else, annotation) -> 
              let test_annotation = type_expression env test report in 
              let t = Annotation.get_type test_annotation in 
                if Option.get t <> Type_bool then 
                  Error_report.add_error report 
                (Format.sprintf "Type of test is %s instead of bool"
                (Ast.string_of_type_expr (Option.get t)), Annotation.get_pos annotation);
            
                ignore (type_statement env i_then report);
                ignore ( type_statement env i_else report );
                let new_annotation = Annotation.create (Lexing.dummy_pos, Lexing.dummy_pos) in
                new_annotation
            
            | Print (e1, annotation) -> 
              type_expression env e1 report 
            | Draw (e1, annotation) -> 
              type_expression env e1 report 
            
      
      in
      
      let process_expression expr =
        ignore (type_expression type_environment expr report)
      in
    
      List.iter process_expression program;
      match program with _ -> ()
    in
    ()
      
*)

      
      let type_analyser report program =
        let type_environment = Environment.new_environment () in
        Environment.add type_environment "x" Type_int;
        Error_report.add_warning report
          ("sample_warning", (Lexing.dummy_pos, Lexing.dummy_pos));
        match program with _ -> ()
      