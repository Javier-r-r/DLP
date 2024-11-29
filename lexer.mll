
{
  open Parser;;
  exception Lexical_error;;
}

rule token = parse
    [' ' '\t']  { token lexbuf }
  | "lambda"    { LAMBDA }
  | "L"         { LAMBDA }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "succ"      { SUCC }
  | "pred"      { PRED }
  | "iszero"    { ISZERO }
  | "let"       { LET }
  | "letrec"    { LETREC }
  | "fix"       { FIX }
  | "in"        { IN }
  | "concat"    { CONCAT }
  | "nil"       { NIL }
  | "cons"      { CONS }
  | "isnil"     { ISNIL }
  | "head"      { HEAD }
  | "tail"      { TAIL }
  | "Bool"      { BOOL }
  | "Nat"       { NAT }
  | "as"        { AS }
  | "String"    { STRING }
  | "List"      { LIST }
  | "list"      { LIST }
  | "quit"      { QUIT }
  | "case"      { CASE }
  | "of"        { OF }
  | "|"         { PIPE }
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | '{'         { LBRACE }
  | '}'         { RBRACE }
  | '['         { LBRACKET }
  | ']'         { RBRACKET }
  | "<"         { LARROW }
  | ">"         { RARROW }
  | ','         { COMMA }
  | '.'         { DOT }
  | '='         { EQ }
  | ':'         { COLON }
  | "->"        { ARROW }
  | "=>"        { DOUBLEARROW }
  | ['0'-'9']+  { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | ['a'-'z']['a'-'z' '_' '0'-'9']*
                { IDV (Lexing.lexeme lexbuf) }
  | ['A'-'Z']['A'-'Z' 'a'-'z' '_' '0'-'9']*
                { IDT (Lexing.lexeme lexbuf) }
  | eof         { EOF }
  | _           { raise Lexical_error }

