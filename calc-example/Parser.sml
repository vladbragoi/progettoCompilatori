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

open Obj Parsing;
prim_val vector_ : int -> 'a -> 'a Vector.vector = 2 "make_vect";
prim_val update_ : 'a Vector.vector -> int -> 'a -> unit = 3 "set_vect_item";

val yytransl = #[
  257 (* DIV *),
  258 (* EOF *),
  259 (* EOL *),
  260 (* INT *),
  261 (* LPAREN *),
  262 (* MINUS *),
  263 (* PLUS *),
  264 (* RPAREN *),
  265 (* TIMES *),
  266 (* UMINUS *),
    0];

val yylhs = "\255\255\
\\002\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\\000\000";

val yylen = "\002\000\
\\002\000\001\000\003\000\003\000\003\000\003\000\003\000\002\000\
\\002\000";

val yydefred = "\000\000\
\\000\000\000\000\002\000\000\000\000\000\000\000\009\000\000\000\
\\000\000\000\000\001\000\000\000\000\000\000\000\003\000\000\000\
\\000\000\000\000\000\000";

val yydgoto = "\002\000\
\\006\000\007\000";

val yysindex = "\001\000\
\\047\255\000\000\000\000\047\255\047\255\037\255\000\000\041\255\
\\000\000\047\255\000\000\047\255\047\255\047\255\000\000\000\000\
\\003\255\003\255\000\000";

val yyrindex = "\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\255\000\000\000\000\000\000\000\000\000\000\000\000\019\255\
\\052\255\058\255\028\255";

val yygindex = "\000\000\
\\252\255\000\000";

val YYTABLESIZE = 66;
val yytable = "\008\000\
\\009\000\001\000\000\000\010\000\000\000\016\000\000\000\017\000\
\\018\000\019\000\008\000\014\000\008\000\000\000\000\000\008\000\
\\008\000\008\000\008\000\007\000\000\000\007\000\000\000\000\000\
\\007\000\007\000\007\000\007\000\006\000\000\000\006\000\000\000\
\\000\000\006\000\006\000\006\000\006\000\010\000\000\000\011\000\
\\000\000\010\000\012\000\013\000\000\000\014\000\012\000\013\000\
\\015\000\014\000\003\000\004\000\000\000\000\000\005\000\000\000\
\\005\000\005\000\005\000\005\000\004\000\000\000\000\000\004\000\
\\004\000\004\000";

val yycheck = "\004\000\
\\005\000\001\000\255\255\001\001\255\255\010\000\255\255\012\000\
\\013\000\014\000\001\001\009\001\003\001\255\255\255\255\006\001\
\\007\001\008\001\009\001\001\001\255\255\003\001\255\255\255\255\
\\006\001\007\001\008\001\009\001\001\001\255\255\003\001\255\255\
\\255\255\006\001\007\001\008\001\009\001\001\001\255\255\003\001\
\\255\255\001\001\006\001\007\001\255\255\009\001\006\001\007\001\
\\008\001\009\001\004\001\005\001\255\255\255\255\003\001\255\255\
\\010\001\006\001\007\001\008\001\003\001\255\255\255\255\006\001\
\\007\001\008\001";

val yyact = vector_ 10 (fn () => ((raise Fail "parser") : obj));
(* Rule 1, file Parser.grm, line 19 *)
val _ = update_ yyact 1
(fn () => repr(let
val d__1__ = peekVal 1 : int
in
( (d__1__) ) end : int))
;
(* Rule 2, file Parser.grm, line 22 *)
val _ = update_ yyact 2
(fn () => repr(let
val d__1__ = peekVal 0 : int
in
( (d__1__) ) end : int))
;
(* Rule 3, file Parser.grm, line 23 *)
val _ = update_ yyact 3
(fn () => repr(let
val d__2__ = peekVal 1 : int
in
( (d__2__) ) end : int))
;
(* Rule 4, file Parser.grm, line 24 *)
val _ = update_ yyact 4
(fn () => repr(let
val d__1__ = peekVal 2 : int
val d__3__ = peekVal 0 : int
in
( (d__1__) + (d__3__) ) end : int))
;
(* Rule 5, file Parser.grm, line 25 *)
val _ = update_ yyact 5
(fn () => repr(let
val d__1__ = peekVal 2 : int
val d__3__ = peekVal 0 : int
in
( (d__1__) - (d__3__) ) end : int))
;
(* Rule 6, file Parser.grm, line 26 *)
val _ = update_ yyact 6
(fn () => repr(let
val d__1__ = peekVal 2 : int
val d__3__ = peekVal 0 : int
in
( (d__1__) * (d__3__) ) end : int))
;
(* Rule 7, file Parser.grm, line 27 *)
val _ = update_ yyact 7
(fn () => repr(let
val d__1__ = peekVal 2 : int
val d__3__ = peekVal 0 : int
in
( (d__1__) div (d__3__) ) end : int))
;
(* Rule 8, file Parser.grm, line 28 *)
val _ = update_ yyact 8
(fn () => repr(let
val d__2__ = peekVal 0 : int
in
( ~ (d__2__) ) end : int))
;
(* Entry Main *)
val _ = update_ yyact 9 (fn () => raise yyexit (peekVal 0));
val yytables : parseTables =
  ( yyact,
    yytransl,
    yylhs,
    yylen,
    yydefred,
    yydgoto,
    yysindex,
    yyrindex,
    yygindex,
    YYTABLESIZE,
    yytable,
    yycheck );
fun Main lexer lexbuf = yyparse yytables 1 lexer lexbuf;
