%{
    open Ast
     
    exception SyntaxError of string
    (*Le parseur doit générer des Ast.program.
    Pour les annotation, il vous suffit de créer l’annotation avec [Annotation.create $loc] qui sera ce qu’il vous faut dans tous les cas : $loc est un paramètre qui correspond aux positions délimitant la chaîne de caractères parsée par la règle où il est utilisé, ce qui est exactement ce qu’il faut mettre dans l’annotation.*)
%}



(* Ajoutez ici vos règles de grammaire *)


%token FLOAT_TYP
%token VAR
%token COMMA
%token SEMICOLON
%token ASSIGN
%token DEF
%token L_CUR_BRK
%token R_CUR_BRK
%token L_SQ_BRK
%token R_SQ_BRK
%token L_PAR
%token R_PAR
%token NULL_TYP
%token BOOL_TYP
%token INT_TYP
%token AND BEGIN BLUE COLOR COPY COS DRAW ELSE END FLOAT_OF_INT
%token FLOOR FOR FOREACH FROM GREEN HEAD IF IN LIST NOT OR POINT POS
%token PRINT RED SIN STEP TAIL TO X Y
%token ADD SUB MUL DIV MOD EQ NEQ LEQ GEQ LT GT CONS DOT EOF
%token RETURN
%token <string> ID
%token <string> STRING
%token <int> INT
%token <float> FLOAT
%token <bool> BOOL

%start <program> main
%%

main:
| prog = program EOF { prog }


program:
| LT args = arguments GT stmt = statement { Program(args, stmt) }

arguments:
| { [] } 
| arg = argument SEMICOLON args = arguments { arg :: args }

argument:
| t = type_expr id = ID { Argument(id, t, Annotation.create $loc) }

/* Règles pour type_expr */
type_expr:
| INT_TYP { Type_int }
| FLOAT_TYP { Type_float }
| BOOL_TYP { Type_bool }

statement_list: (* Nouvelle règle pour une liste d'instructions *)
| { [] }
| stmt = statement stmts = statement_list { stmt :: stmts }


/* Règles pour statement */
statement:
| typ = type_expr L_PAR name = ID R_PAR SEMICOLON { Variable_declaration(name, typ, Annotation.create $loc) }
| COPY L_PAR expr1 = expression COMMA expr2 = expression R_PAR SEMICOLON { Assignment(expr1, expr2,Annotation.create $loc) }
| BEGIN stmts = statement_list END { Block(stmts, Annotation.create $loc) } (* Ajouté pour gérer plusieurs instructions à l'intérieur d'un bloc *)
//| VAR name = ID L_PAR expr = expression  R_PAR SEMICOLON { Variable_declaration(name, Type_int, Annotation.create $loc) } // Modifier Type_int en fonction du type d'expression
//| BEGIN stmts = statement_list END { Block(stmts, Annotation.create $loc) }
//| IF test = expression THEN i1 = statement ELSE i2 = statement { IfThenElse(test, i1, i2, Annotation.create $loc) }
//| IF test = expression THEN i1 = statement { IfThenElse(test, i1, Nop, Annotation.create $loc) }
// Ajoutez des règles pour les autres types de statement ici

expression:
| i = INT {
    if i >= -2147483648 && i <= 2147483647 then
        Constant_i(i, Annotation.create $loc)
    else
        raise (SyntaxError "Les entiers doivent être compris entre -2147483648 et 2147483647 inclus")
}
| f = FLOAT { Constant_f(f, Annotation.create $loc) }
| b = BOOL { Constant_b(b, Annotation.create $loc) }
| POS L_PAR e1 = expression COMMA e2 = expression R_PAR SEMICOLON {
    match e1, e2 with
    | Constant_i(i1, _), Constant_i(i2, _) when i1 >= 0 && i1 < (1 lsl 16) && i2 >= 0 && i2 < (1 lsl 16) ->
        Pos(Constant_i(i1, Annotation.create $loc), Constant_i(i2, Annotation.create $loc), Annotation.create $loc)
    | _ -> raise (SyntaxError "Les expressions e1 et e2 doivent être des entiers positifs et inférieurs à 2^16")
}
| COLOR L_PAR e1 = expression COMMA e2 = expression COMMA e3 = expression R_PAR SEMICOLON {
    match e1, e2, e3 with
    | Constant_i(i1, _), Constant_i(i2, _), Constant_i(i3, _) when i1 >= 0 && i1 <= 255 && i2 >= 0 && i2 <= 255 && i3 >= 0 && i3 <= 255 ->
        Color(Constant_i(i1, Annotation.create $loc), Constant_i(i2, Annotation.create $loc), Constant_i(i3, Annotation.create $loc), Annotation.create $loc)
    | _ -> raise (SyntaxError "Les expressions e1, e2 et e3 doivent être des entiers compris entre 0 et 255")
}
| POINT L_PAR e1 = expression COMMA e2 = expression R_PAR SEMICOLON { Point(e1, e2, Annotation.create $loc) }
| i = ID {Variable(i, Annotation.create $loc)}
| e1 = expression b = binop e2 = expression { Binary_operator(b,e1,e2,Annotation.create $loc) }
| L_PAR e = expression R_PAR { e }
| u = unop e = expression { Unary_operator(u,e,Annotation.create $loc) } 
| e = expression POINT f = field_acc  { Field_accessor(f,e,Annotation.create $loc) }
| l = list_expression { List (l, Annotation.create $loc) }
| e1 = expression CONS e2 = expression {
    match e2 with
    | List(elems, _) -> List(e1 :: elems, Annotation.create $loc)
    | _ -> raise (SyntaxError "Le second argument de l'opérateur :: doit être une liste")
}


list_expression:
| L_SQ_BRK R_SQ_BRK { [] }
| L_SQ_BRK elems = list_elements R_SQ_BRK { elems }

list_elements:
| e = expression { [e] }
| e = expression COMMA elems = list_elements { e :: elems }

%inline field_acc: 
| COLOR { Color_accessor}
| POS { Position_accessor } 
| X { X_accessor}
| Y { Y_accessor}
| BLUE { Blue_accessor } 
| RED { Red_accessor }
| GREEN { Green_accessor }

%inline binop:
| ADD   { Add }
| SUB   { Sub }
| MUL   { Mul }
| DIV   { Div }
| MOD   { Mod }
| AND   { And }
| OR    { Or }
| EQ    { Eq }
| NEQ   { Ne }
| LT    { Lt }
| GT    { Gt }
| LEQ   { Le }
| GEQ   { Ge }

%inline unop:
| SUB   { USub } 
| NOT   { Not }
