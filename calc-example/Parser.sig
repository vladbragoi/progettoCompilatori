local
in
datatype token =
    DIV
  | EOF
  | EOL
  | INT of int
  | LPAREN
  | MINUS
  | PLUS
  | RPAREN
  | TIMES
  | UMINUS
end;

val Main :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> int;
