open Util
open Ast

(*
    Réalisez ici l’analyse de type d’un programme. Faites une sous-fonction récursive pour les expressions et les statements.

    L’idée est la même que dans le langage du cours : pour chaque élément, commencez par typer ses sous-éléments. Ensuite, en fonction de l’élément et du type de ses sous-éléments, déterminez son type, et placez-le dans l’annotation qui l’accompagne.
    Seules les expressions peuvent ainsi être typées.

    Les fonctions devront manipuler un environnement qui associera chaque variable à son type. Les déclarations fixent ces types et ces types sont vérifiés lors de chaque utilisation d’une variable.

    Attention, les déclarations sont locales à un bloc, vous devez donc utiliser les fonctions Environment.add_layer et Environment.remove_layer pour le gérer.

    Cette fonction doit également effectuer les rapports d’erreur et d’avertissement dans [report].

    Ces fonction font un pattern matching sur leur argument principal et traitent chaque cas séparément. Elles font uniquement des effet de bord.
    Par exemple : type_expression : Ast.type_expr annotation -> Util.Error_report.t -> Ast.expression -> unit

    Vous pouvez également effectuer ici (en même temps ou dans une fonction séparée) une analyse d’initialisation des variables (auquel cas, il faut ajouter un argument supplémentaire à ce qui est décrit ci-dessus).

    Vous préciserez ce que vous avez traité dans votre rapport.
*)

let type_analyser report program =
  let type_environment = Environment.new_environment () in
  Environment.add type_environment "x" Type_int;
  Error_report.add_warning report
    ("sample_warning", (Lexing.dummy_pos, Lexing.dummy_pos));

  let rec type_expression env (expr, ann) =
    let te = match expr with
      | Constant_i (i, _) -> Type_int
      | Constant_f (f, _) -> Type_float
      | Constant_b (b, _) -> Type_bool
      | Pos (e1, e2, _) ->
        type_expression env e1;
        type_expression env e2;
        Type_pos
    in
    ann.type_expr <- Some te

  and type_statement env stmt =
  
  match program with
  | Program (args, stmt) ->
    let env = Environment.add_layer type_environment in
    List.iter (fun (Argument (name, t, _)) ->
      let env = Environment.add_variable env name t in ()) args;
    type_statement env stmt
