{
    open Parser
    exception Error of string
}

let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
(*let alphanum = [lower upper digit '\'' '_']*)

rule token = parse
    | "//" [^ '\n']* '\n' {Lexing.new_line lexbuf; token lexbuf}
    | "/*"              {commentary lexbuf}
    | [' ' '\t' '\r']   {token lexbuf}
    | '\n'              { Lexing.new_line lexbuf ; token lexbuf }

   (* | lower (alphanum)* as s  { ID(s) }      *)

    (* Mots clés *)
    | "And"       { AND }
    | "Begin"     { BEGIN }
    | "Blue"      { BLUE }
    | "Bool"      { BOOL }
    | "Color"     { COLOR }
    | "Copy"      { COPY }
    | "Cos"       { COS }
    | "Draw"      { DRAW }
    | "Else"      { ELSE }
    | "End"       { END }
    | "Float"     { FLOAT }
    | "Float_of_int" { FLOAT_OF_INT }
    | "Floor"     { FLOOR }
    | "For"       { FOR }
    | "Foreach"   { FOREACH }
    | "From"      { FROM }
    | "Green"     { GREEN }
    | "Head"      { HEAD }
    | "If"        { IF }
    | "In"        { IN }
    | "Int"       { INT }
    | "List"      { LIST }
    | "Not"       { NOT }
    | "Or"        { OR }
    | "Point"     { POINT }
    | "Pos"       { POS }
    | "Print"     { PRINT }
    | "Red"       { RED }
    | "Sin"       { SIN }
    | "Step"      { STEP }
    | "Tail"      { TAIL }
    | "To"        { TO }
    | "X"         { X }
    | "Y"         { Y }

    (* Opérateurs *)
    | "+"  { ADD }
    | "-"  { SUB }
    | "*"  { MUL }
    | "/"  { DIV }
    | "%"  { MOD }
    | "="  { EQ }
    | "<>" { NEQ }
    | "<=" { LEQ }
    | ">=" { GEQ }
    | "<"  { LT }
    | ">"  { GT }
    | "::" { CONS }
    | "."  { DOT }

    | eof  { EOF }
    | _ as s            { let pos = Lexing.lexeme_start_p lexbuf in raise (Error(Format.sprintf "Line %d, char %d ,Read: '%c'. It is not an acceptable character" pos.pos_lnum (pos.pos_cnum - pos.pos_bol +1) s)) }

and commentary = parse
    | '\n'      {Lexing.new_line lexbuf; commentary lexbuf}
    | "*/"      { token lexbuf }
    | _ { commentary lexbuf }
