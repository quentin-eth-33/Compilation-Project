%{
    open Ast
    exception SyntaxError of string
    (*Le parseur doit générer des Ast.program.
    Pour les annotation, il vous suffit de créer l’annotation avec [Annotation.create $loc] qui sera ce qu’il vous faut dans tous les cas : $loc est un paramètre qui correspond aux positions délimitant la chaîne de caractères parsée par la règle où il est utilisé, ce qui est exactement ce qu’il faut mettre dans l’annotation.*)
%}

(* Ajoutez ici vos règles de grammaire *)
%token FLOAT_TYP
%token COMMA
%token SEMICOLON
%token L_SQ_BRK
%token R_SQ_BRK
%token L_PAR
%token R_PAR
%token BOOL_TYP
%token INT_TYP
%token AND BEGIN BLUE COLOR COPY COS DRAW ELSE END FLOAT_OF_INT
%token FLOOR FOR FOREACH FROM GREEN HEAD IF IN LIST NOT OR POINT POS
%token PRINT RED SIN STEP TAIL TO X Y
%token ADD SUB MUL DIV MOD EQ NE LE GE LT GT CONS DOT EOF
%token <string> ID
%token <int> INT
%token <float> FLOAT
%token <bool> BOOL

%start <program> main
%nonassoc R_PAR
%nonassoc ELSE
%left AND OR
%left EQ NE
%left LT GT LE GE 
%left ADD SUB
%left MUL DIV MOD
%left CONS
%nonassoc NOT
%nonassoc DOT
%%

main:
| prog = program EOF { prog }

program:
| LT args = arguments GT stmt = statement { Program(args, stmt) } (* Modifié pour utiliser statement_list au lieu de statement *)
| stmt = statement { Program([], stmt) }

arguments:
| { [] }
| arg = argument SEMICOLON args = arguments { arg :: args }

argument:
| t = type_expr id = ID { Argument(id, t, Annotation.create $loc) }

type_expr:
| INT_TYP { Type_int }
| FLOAT_TYP { Type_float }
| BOOL_TYP { Type_bool }
| COLOR { Type_color }
| POINT { Type_point }
| POS { Type_pos }
| LIST L_PAR i= type_expr R_PAR { Type_list(i) }

statement_list: (* Nouvelle règle pour une liste d'instructions *)
| { [] }
| stmt = statement stmts = statement_list { stmt :: stmts }

statement:
| typ = type_expr L_PAR name = ID R_PAR SEMICOLON { Variable_declaration(name, typ, Annotation.create $loc) }
| COPY L_PAR expr1 = expression COMMA expr2 = expression R_PAR SEMICOLON { Assignment(expr1, expr2,Annotation.create $loc) }
| BEGIN stmts = statement_list END { Block(stmts, Annotation.create $loc) } 
| IF L_PAR test = expression R_PAR i1 = statement ELSE i2 = statement { IfThenElse(test,i1,i2,Annotation.create $loc) }
| IF L_PAR test = expression R_PAR i1 = statement { IfThenElse(test,i1,(Block([],Annotation.create $loc)), Annotation.create $loc)}
| FOR id = ID FROM start_expr = expression TO end_expr = expression STEP step_expr = expression stmt = statement { For(id, start_expr, end_expr, step_expr, stmt, Annotation.create $loc) }
| FOREACH id = ID IN list_expr = expression stmt = statement { Foreach(id, list_expr, stmt, Annotation.create $loc) }
| DRAW L_PAR expr = expression R_PAR { Draw(expr, Annotation.create $loc)}
| PRINT L_PAR expr = expression R_PAR { Print(expr, Annotation.create $loc)}
| NOT { Nop } 

expression:
| i = INT {
    if i >= -2147483648 && i <= 2147483647 then
        Constant_i(i, Annotation.create $loc)
    else
        raise (SyntaxError "Les entiers doivent être compris entre -2147483648 et 2147483647 inclus")
}
| f = FLOAT { Constant_f(f, Annotation.create $loc) }
| b = BOOL { Constant_b(b, Annotation.create $loc) }
| s = ID {Variable(s,Annotation.create $loc) }
| POS L_PAR e1 = expression COMMA e2 = expression R_PAR option_semicolon {
    Pos(e1, e2, Annotation.create $loc)
}
| POINT L_PAR e1 = expression COMMA e2 = expression R_PAR option_semicolon { Point(e1, e2, Annotation.create $loc) }
| COLOR L_PAR e1 = expression COMMA e2 = expression COMMA e3 = expression R_PAR option_semicolon { Color(e1, e2, e3, Annotation.create $loc) }
| e = expression DOT f = field_acc  { Field_accessor(f,e,Annotation.create $loc) }
| e1 = expression b = binop e2 = expression { Binary_operator(b,e1,e2,Annotation.create $loc)}
| u = unop e = expression { Unary_operator(u,e, Annotation.create $loc)} %prec NOT
| l = list_expression { List (l, Annotation.create $loc) }
| e1 = expression CONS e2 = expression { Cons(e1, e2, Annotation.create $loc) }
| L_PAR e = expression R_PAR { e } 

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
| LT    { Lt }
| GT    { Gt }
| LE    { Le }
| GE    { Ge }
| NE    { Ne }
| EQ    { Eq }

%inline unop:
| SUB           { USub } 
| NOT           { Not }
| HEAD          { Head }
| TAIL          { Tail }
| FLOOR         { Floor }
| FLOAT_OF_INT  {Float_of_int }
| COS           { Cos }
| SIN           { Sin }  

option_semicolon:
| SEMICOLON { () }
| { () }
