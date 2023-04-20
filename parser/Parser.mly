%{
  open Ast
(*
  let make_pos x y = Pos (x, y, Annotation.create $loc)
  let make_color r g b = Color (r, g, b, Annotation.create $loc)
  let make_point pos color = Point (pos, color, Annotation.create $loc)
  let make_list elts = List (elts, Annotation.create $loc)
  let make_cons head tail = Cons (head, tail, Annotation.create $loc)*)
%}

%token <string> ID
%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token L_CUR_BRK
%token R_CUR_BRK
%token SEMICOLON
%token THEN
%token COMMA
%token FLOAT_TYP
%token INT_TYP
%token BOOL_TYP
%token ASSIGN
%token AND BEGIN BLUE COLOR COPY COS DRAW ELSE END FLOAT_OF_INT
%token FLOOR FOR FOREACH FROM GREEN HEAD IF IN LIST NOT OR POINT POS
%token PRINT RED SIN STEP TAIL TO X Y
%token L_PAR
%token R_PAR

%token L_SQ_BRK
%token R_SQ_BRK

%token ADD SUB MUL DIV MOD EQ NEQ LEQ GEQ LT GT CONS DOT

%token EOF

%start <program> main
%%

main:
| program EOF { $1 }
;

program:
| args statement { Program ($1, $2) }
;

args:
| L_CUR_BRK arg_list R_CUR_BRK { $2 }
| { [] }
;

arg_list:
| arg { [$1] }
| arg_list COMMA arg { $1 @ [$3] }
;

arg:
| type_expr ID { Argument ($2, $1, Annotation.create $loc) }
;

type_expr:
| INT_TYP { Type_int }
| FLOAT_TYP { Type_float }
| BOOL_TYP { Type_bool }
| POS { Type_pos }
| COLOR { Type_color }
| POINT { Type_point }
| LIST L_PAR type_expr R_PAR { Type_list $3 }
;

statement:
| BEGIN statement_list END { Block ($2, Annotation.create $loc) }
| IF expression THEN statement ELSE statement { IfThenElse ($2, $4, $6, Annotation.create $loc) }
| FOR ID FROM expression TO expression STEP expression statement { For ($2, $4, $6, $8, $9, Annotation.create $loc) }
| FOREACH ID IN expression statement { Foreach ($2, $4, $5, Annotation.create $loc) }
| DRAW expression { Draw ($2, Annotation.create $loc) }
| PRINT expression { Print ($2, Annotation.create $loc) }
| type_expr ID ASSIGN expression { Assignment (Variable ($2, Annotation.create $loc), $4, Annotation.create $loc) }
| type_expr ID { Variable_declaration ($2, $1, Annotation.create $loc) }
;

statement_list:
| statement { [$1] }
| statement_list SEMICOLON statement { $1 @ [$3] }
;

expression:
| INT { Constant_i ($1, Annotation.create $loc) }
| FLOAT { Constant_f ($1, Annotation.create $loc) }
| BOOL { Constant_b ($1, Annotation.create $loc) }
| ID { Variable ($1, Annotation.create $loc) }
| expression ADD expression { Binary_operator (Add, $1, $3, Annotation.create $loc) }
| expression SUB expression { Binary_operator (Sub, $1, $3, Annotation.create $loc) }
| expression MUL expression { Binary_operator (Mul, $1, $3, Annotation.create $loc) }
| expression DIV expression { Binary_operator (Div, $1, $3, Annotation.create $loc) }
| expression MOD expression { Binary_operator (Mod, $1, $3, Annotation.create $loc) }
| expression AND expression { Binary_operator (And, $1, $3, Annotation.create $loc) }
| expression OR expression { Binary_operator (Or, $1, $3, Annotation.create $loc) }
| expression EQ expression { Binary_operator (Eq, $1, $3, Annotation.create $loc) }
| expression NEQ expression { Binary_operator (Ne, $1, $3, Annotation.create $loc) }
| expression LT expression { Binary_operator (Lt, $1, $3, Annotation.create $loc) }
| expression GT expression { Binary_operator (Gt, $1, $3, Annotation.create $loc) }
| expression LEQ expression { Binary_operator (Le, $1, $3, Annotation.create $loc) }
| expression GEQ expression { Binary_operator (Ge, $1, $3, Annotation.create $loc) }
| SUB expression %prec UNARY { Unary_operator (USub, $2, Annotation.create $loc) }
| NOT expression { Unary_operator (Not, $2, Annotation.create $loc) }
| HEAD expression { Unary_operator (Head, $2, Annotation.create $loc) }
| TAIL expression { Unary_operator (Tail, $2, Annotation.create $loc) }
| FLOOR expression { Unary_operator (Floor, $2, Annotation.create $loc) }
| FLOAT_OF_INT expression { Unary_operator (Float_of_int, $2, Annotation.create $loc) }
| COS expression { Unary_operator (Cos, $2, Annotation.create $loc) }
| SIN expression { Unary_operator (Sin, $2, Annotation.create $loc) }
| expression CONS expression { Cons ($1, $3, Annotation.create $loc) }
| L_SQ_BRK expr_list R_SQ_BRK { List ($2, Annotation.create $loc) }
| POS L_PAR expression COMMA expression R_PAR { make_pos $3 $5 }
| COLOR L_PAR expression COMMA expression COMMA expression R_PAR { make_color $3 $5 $7 }
| POINT L_PAR expression COMMA expression R_PAR { make_point $3 $5 }
| expression DOT field_accessor { Field_accessor ($3, $1, Annotation.create $loc) }
;

expr_list:
| { [] }
| expression { [$1] }
| expr_list COMMA expression { $1 @ [$3] }
;

field_accessor:
| X { X_accessor }
| Y { Y_accessor }
| RED { Red_accessor }
| GREEN { Green_accessor }
| BLUE { Blue_accessor }
| COLOR { Color_accessor }
| POSITION { Position_accessor }
;
