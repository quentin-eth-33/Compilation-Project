%{
    open Ast

    (*Le parseur doit générer des Ast.program.
    Pour les annotation, il vous suffit de créer l’annotation avec [Annotation.create $loc] qui sera ce qu’il vous faut dans tous les cas : $loc est un paramètre qui correspond aux positions délimitant la chaîne de caractères parsée par la règle où il est utilisé, ce qui est exactement ce qu’il faut mettre dans l’annotation.*)
%}



(* Ajoutez ici vos règles de grammaire *)
%token <string> ID
%token AND BEGIN BLUE BOOL COLOR COPY COS DRAW ELSE END FLOAT FLOAT_OF_INT
%token FLOOR FOR FOREACH FROM GREEN HEAD IF IN INT LIST NOT OR POINT POS
%token PRINT RED SIN STEP TAIL TO X Y
%token ADD SUB MUL DIV MOD EQ NEQ LEQ GEQ LT GT CONS DOT EOF


%start <program> main
%%

main:
| EOF { Program([],Block([],Annotation.create $loc)) }