%{
    open Ast

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
| args = arguments stmt = statement { Program(args, stmt) }

arguments:
| { [] } 
| arg = argument SEMICOLON args = arguments { arg :: args }

argument:
| t = type_expr id = ID { Argument(id, t, Annotation.create $loc) }

/* Règles pour type_expr */
type_expr:
// Ajoutez des règles pour construire des objets de type type_expr ici, par exemple:
| INT_TYP { Type_int }
| FLOAT_TYP { Type_float }
| BOOL_TYP { Type_bool }

/* Règles pour statement */
statement:
// Mettez à jour les règles existantes pour construire des objets de type statement ici
| typ = type_expr L_PAR name = ID R_PAR SEMICOLON { Variable_declaration(name, typ, Annotation.create $loc) }
| COPY L_PAR name = ID COMMA expr = expression R_PAR SEMICOLON { Assignment(Variable(name, Annotation.create $loc), expr, Annotation.create $loc) }
//| VAR name = ID L_PAR expr = expression  R_PAR SEMICOLON { Variable_declaration(name, Type_int, Annotation.create $loc) } // Modifier Type_int en fonction du type d'expression
//| BEGIN stmts = statement_list END { Block(stmts, Annotation.create $loc) }
//| IF test = expression THEN i1 = statement ELSE i2 = statement { IfThenElse(test, i1, i2, Annotation.create $loc) }
//| IF test = expression THEN i1 = statement { IfThenElse(test, i1, Nop, Annotation.create $loc) }
// Ajoutez des règles pour les autres types de statement ici

/* Règle pour statement_list */
statement_list:
| { [] } (* Aucun statement *)
| stmt = statement stmts = statement_list { stmt :: stmts }

expression:
| i = INT { Constant_i(i, Annotation.create $loc) }
| f = FLOAT { Constant_f(f, Annotation.create $loc) }
| b = BOOL { Constant_b(b, Annotation.create $loc) }

/*statement: */



%inline binop:
| ADD   { Add }
| SUB   { Sub }
| MUL   { Mul }
| DIV   { Div }
| MOD   { Mod }
| AND   { And }
| OR    { Or }
| EQ    { Eq }
| NEQ   { Nq }
| LT    { Lt }
| GT    { Gt }
| LEQ   { Lq }
| GEQ   { Gq }