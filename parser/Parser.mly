%{
    open Ast

    (*Le parseur doit générer des Ast.program.
    Pour les annotation, il vous suffit de créer l’annotation avec [Annotation.create $loc] qui sera ce qu’il vous faut dans tous les cas : $loc est un paramètre qui correspond aux positions délimitant la chaîne de caractères parsée par la règle où il est utilisé, ce qui est exactement ce qu’il faut mettre dans l’annotation.*)
%}



(* Ajoutez ici vos règles de grammaire *)
%token <string> ID
%token <string> STRING
%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token NULL_TYP
%token BOOL_TYP
%token AND BEGIN BLUE COLOR COPY COS DRAW ELSE END FLOAT_OF_INT
%token FLOOR FOR FOREACH FROM GREEN HEAD IF IN LIST NOT OR POINT POS
%token PRINT RED SIN STEP TAIL TO X Y
%token ADD SUB MUL DIV MOD EQ NEQ LEQ GEQ LT GT CONS DOT EOF
%token RETURN

%start <program> main
%%

main:
| EOF { Program([],Block([],Annotation.create $loc)) }